# R/transforms/indicators.R
# Normalize _cd indicator columns to logical.
# {Y, y, 1, T, TRUE, true} -> TRUE; {N, n, 0, F, FALSE, false} -> FALSE; else NA.

INDICATOR_TRUE  <- c("Y", "y", "1", "T", "TRUE",  "true",  "True")
INDICATOR_FALSE <- c("N", "n", "0", "F", "FALSE", "false", "False")

#' @param dt data.table.
#' @param cols character vector of indicator column names (typically `_cd` suffix).
#' @param logger optional log4r logger.
#' @return dt, modified in place (also returned invisibly).
transform_indicators <- function(dt, cols, logger = NULL) {
  stopifnot(data.table::is.data.table(dt))
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols)) {
    stop(sprintf("transform_indicators: columns not in dt: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  for (col in cols) {
    raw <- dt[[col]]
    if (is.logical(raw)) next

    s <- trimws(as.character(raw))
    out <- rep(NA, length(s))
    out[s %in% INDICATOR_TRUE]  <- TRUE
    out[s %in% INDICATOR_FALSE] <- FALSE

    n_unknown <- sum(is.na(out) & !is.na(s) & s != "")
    if (n_unknown > 0L && !is.null(logger)) {
      log4r::warn(logger, sprintf("indicators: %s had %d values outside {Y,N,1,0,T,F}",
                                  col, n_unknown))
    }
    data.table::set(dt, j = col, value = as.logical(out))
  }

  invisible(dt)
}
