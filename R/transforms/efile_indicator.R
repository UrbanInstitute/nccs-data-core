# R/transforms/efile_indicator.R
# Map the IRS e-file indicator to boolean.
# Source values: "E" = electronically filed (TRUE), "P" = paper filed (FALSE).
# Anything else (incl. NA) -> NA, logged.

# IRS uses inconsistent values for the e-file indicator across vintages:
#   2015 990 + 990-EZ:        "E" / "P"  (electronic / paper)
#   2016-2017 990 + 990-EZ:   "Y" / "N"  (yes/no e-filed)
#   2018+ extracts:           "E" / "P"
# Plus occasional 1/0 from older transitional vintages. Accept all three.
EFILE_TRUE  <- c("E", "e", "Y", "y", "1", "T", "TRUE", "true", "True")
EFILE_FALSE <- c("P", "p", "N", "n", "0", "F", "FALSE", "false", "False")

#' @param dt data.table containing an `efile_indicator` column.
#' @param logger optional log4r logger.
#' @return dt, modified in place (also returned invisibly).
transform_efile_indicator <- function(dt, logger = NULL) {
  stopifnot(data.table::is.data.table(dt), "efile_indicator" %in% names(dt))

  raw <- dt$efile_indicator
  if (is.logical(raw)) return(invisible(dt))

  s <- trimws(as.character(raw))
  out <- rep(NA, length(s))
  out[s %in% EFILE_TRUE]  <- TRUE
  out[s %in% EFILE_FALSE] <- FALSE

  n_unknown <- sum(is.na(out) & !is.na(s) & s != "")
  if (n_unknown > 0L && !is.null(logger)) {
    log4r::warn(logger, sprintf("efile_indicator: %d / %d rows had values outside {E, P}",
                                n_unknown, nrow(dt)))
  }

  dt[, efile_indicator := as.logical(out)]
  invisible(dt)
}
