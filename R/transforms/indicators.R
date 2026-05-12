# R/transforms/indicators.R
# Normalize _cd indicator columns to logical.
# {Y, y, 1, T, TRUE, true} -> TRUE; {N, n, 0, 2, F, FALSE, false} -> FALSE; else NA.
#
# "2" is in FALSE because IRS shifted some binary _cd columns to 1/2 encoding
# (1 = yes, 2 = no) in recent vintages, e.g. hospital_audited_attached_cd (py2022)
# and qualified_health_plan_multi_state_cd (py2023). See
# docs/11-upstream-source-quirks.qmd for the trigger and validation.
#
# REVISIT IF: a _cd-suffixed harmonized column is ever introduced that uses
# trinary encoding (e.g. 0=no, 1=yes, 2=unknown). No such column exists today;
# real integer-code columns in the SOI extract are renamed off the _cd suffix
# at the crosswalk stage so they bypass this transform entirely.

INDICATOR_TRUE  <- c("Y", "y", "1", "T", "TRUE",  "true",  "True")
INDICATOR_FALSE <- c("N", "n", "0", "2", "F", "FALSE", "false", "False")

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
      log4r::warn(logger, sprintf("indicators: %s had %d values outside {Y,N,1,2,0,T,F}",
                                  col, n_unknown))
    }
    data.table::set(dt, j = col, value = as.logical(out))
  }

  invisible(dt)
}
