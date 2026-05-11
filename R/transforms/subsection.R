# R/transforms/subsection_cd.R
# Coerce subsection_cd to integer, validate against known IRC 501 subsection_cd codes,
# and derive the is_501c3 boolean indicator column.

#' @param dt data.table containing a `subsection_cd` column.
#' @param logger optional log4r logger.
#' @return dt, modified in place (also returned invisibly). Adds `is_501c3`.
transform_subsection_cd <- function(dt, logger = NULL) {
  stopifnot(data.table::is.data.table(dt), "subsection_cd" %in% names(dt))

  sub <- suppressWarnings(as.integer(dt$subsection_cd))
  unknown <- !is.na(sub) & !(sub %in% KNOWN_SUBSECTION_CODES)
  sub[unknown] <- NA_integer_

  dt[, subsection_cd := sub]
  dt[, is_501c3   := !is.na(sub) & sub == SUBSECTION_501C3]

  n_unknown <- sum(unknown)
  if (n_unknown > 0L && !is.null(logger)) {
    log4r::warn(logger, sprintf("subsection_cd: %d / %d rows had codes outside KNOWN_SUBSECTION_CODES",
                                n_unknown, nrow(dt)))
  }

  invisible(dt)
}
