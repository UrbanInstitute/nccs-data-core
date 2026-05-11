# R/transforms/ein.R
# Normalize EINs to canonical IRS display format: XX-XXXXXXX (10 chars).
# Strips embedded hyphens and whitespace, then re-inserts the hyphen after digit 2.
# The hyphen forces character typing on CSV re-read (avoids leading-zero loss).
# Invalid -> NA, logged.

#' @param dt data.table containing an `ein` column.
#' @param logger optional log4r logger.
#' @return dt, modified in place (also returned invisibly).
transform_ein <- function(dt, logger = NULL) {
  stopifnot(data.table::is.data.table(dt), "ein" %in% names(dt))

  raw <- as.character(dt$ein)
  clean <- gsub("[^0-9]", "", trimws(raw))

  ok <- nchar(clean) >= 1L & nchar(clean) <= 9L & clean != ""
  padded9 <- ifelse(ok, formatC(as.integer(clean), width = 9L, flag = "0", format = "d"),
                    NA_character_)
  hyphenated <- ifelse(is.na(padded9), NA_character_,
                       paste0(substr(padded9, 1L, 2L), "-", substr(padded9, 3L, 9L)))

  dt[, ein := hyphenated]

  n_bad <- sum(is.na(hyphenated) & !is.na(raw))
  if (n_bad > 0L && !is.null(logger)) {
    log4r::warn(logger, sprintf("ein: %d / %d rows have invalid EIN format",
                                n_bad, nrow(dt)))
  }

  invisible(dt)
}
