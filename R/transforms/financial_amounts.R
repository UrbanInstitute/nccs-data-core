# R/transforms/financial_amounts.R
# Coerce financial columns to numeric. Strips currency formatting; parens -> negative.
# Parse failures become NA and are logged per column.

#' @param dt data.table.
#' @param cols character vector of column names to coerce.
#' @param logger optional log4r logger.
#' @return dt, modified in place (also returned invisibly).
transform_financial_amounts <- function(dt, cols, logger = NULL) {
  stopifnot(data.table::is.data.table(dt))
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols)) {
    stop(sprintf("transform_financial_amounts: columns not in dt: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  for (col in cols) {
    raw <- dt[[col]]
    if (is.numeric(raw)) next

    s <- trimws(as.character(raw))
    neg <- grepl("^\\(.*\\)$", s)
    s <- gsub("[\\$,]", "", s)
    s <- gsub("^\\((.*)\\)$", "-\\1", s)
    s[s == "" | s == "-"] <- NA_character_

    parsed <- suppressWarnings(as.numeric(s))
    parsed[neg & !is.na(parsed) & parsed > 0] <- -parsed[neg & !is.na(parsed) & parsed > 0]

    n_failed <- sum(is.na(parsed) & !is.na(s))
    if (n_failed > 0L && !is.null(logger)) {
      log4r::warn(logger, sprintf("financial_amounts: %s had %d unparseable values",
                                  col, n_failed))
    }
    data.table::set(dt, j = col, value = parsed)
  }

  invisible(dt)
}
