# R/transforms/tax_period.R
# Parse the 6-char YYYYMM tax_period field into tax_year + tax_month integers.
# Keeps tax_period as character (preserves leading zeros, etc.).

#' @param dt data.table containing a `tax_period` column.
#' @param logger optional log4r logger.
#' @param tax_year_bounds integer length-2 plausibility window (default from data.R).
#' @return dt, modified in place (also returned invisibly).
transform_tax_period <- function(dt, logger = NULL, tax_year_bounds = tax_year_range()) {
  stopifnot(data.table::is.data.table(dt), "tax_period" %in% names(dt))

  dt[, tax_period := trimws(as.character(tax_period))]

  ok <- grepl("^[0-9]{6}$", dt$tax_period)
  yr <- suppressWarnings(as.integer(substr(dt$tax_period, 1L, 4L)))
  mo <- suppressWarnings(as.integer(substr(dt$tax_period, 5L, 6L)))

  ok <- ok &
    !is.na(yr) & yr >= tax_year_bounds[1] & yr <= tax_year_bounds[2] &
    !is.na(mo) & mo >= 1L & mo <= 12L

  yr[!ok] <- NA_integer_
  mo[!ok] <- NA_integer_

  dt[, tax_year  := yr]
  dt[, tax_month := mo]

  n_bad <- sum(!ok)
  if (n_bad > 0L && !is.null(logger)) {
    log4r::warn(logger, sprintf("tax_period: %d / %d rows have invalid YYYYMM",
                                n_bad, nrow(dt)))
  }

  invisible(dt)
}
