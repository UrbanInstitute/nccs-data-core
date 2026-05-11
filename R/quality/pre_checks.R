# R/quality/pre_checks.R
# Pre-harmonization validation for unpacked IRS SOI source files.
# Runs per (processing_year, form). Per IMPLEMENTATION_PLAN.md §7:
#   - file exists and non-empty
#   - header row present
#   - column count within ±EXPECTED_COL_COUNT_TOLERANCE of per-vintage expected
#   - row count > 0
#   - no duplicate header column names

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

VAR_MATRIX_PATH <- function(form) {
  tag <- switch(form, "990" = "990", "990ez" = "990EZ", "990pf" = "990PF",
                stop(sprintf("Unknown form '%s'", form)))
  here::here("data", "raw", "soi_dictionaries", sprintf("_var_matrix_%s.csv", tag))
}

#' Expected column count for a (processing_year, form) per the IRS dictionary's var matrix.
#' Returns NA if the matrix file or the year column is absent — caller treats NA as
#' "no expectation, skip the column-count tolerance check."
expected_col_count <- function(processing_year, form) {
  path <- VAR_MATRIX_PATH(form)
  if (!file.exists(path)) return(NA_integer_)
  # header = TRUE is required: fread's auto-detection misclassifies the
  # numeric-looking column names ("2012", "2013", ...) as a data row.
  mx <- fread(path, header = TRUE)
  col <- as.character(processing_year)
  if (!col %in% names(mx)) return(NA_integer_)
  sum(mx[[col]] == "Y", na.rm = TRUE)
}

#' Run pre-checks for one unpacked source file.
#' @param processing_year integer
#' @param form character: "990" | "990ez" | "990pf"
#' @param src_path optional explicit path to the unpacked file; if NULL, discovered
#'   under data/intermediate/unpacked/{processing_year}/{form}/
#' @param logger optional log4r logger
#' @return list(passed = bool, errors = character, warnings = character, info = list)
run_pre_checks_one <- function(processing_year, form, src_path = NULL, logger = NULL) {

  errors   <- character()
  warnings <- character()
  info     <- list(processing_year = processing_year, form = form)

  if (is.null(src_path)) {
    src_dir <- here::here("data", "intermediate", "unpacked", processing_year, form)
    files <- list.files(src_dir, pattern = "\\.(csv|dat)$", full.names = TRUE)
    if (length(files) == 0L) {
      errors <- c(errors, sprintf("No unpacked file found under %s", src_dir))
      return(list(passed = FALSE, errors = errors, warnings = warnings, info = info))
    }
    src_path <- files[1]
  }
  info$src_path <- src_path

  # 1. File exists and non-empty
  if (!file.exists(src_path)) {
    errors <- c(errors, sprintf("File does not exist: %s", src_path))
    return(list(passed = FALSE, errors = errors, warnings = warnings, info = info))
  }
  sz <- file.info(src_path)$size
  info$file_size_bytes <- sz
  if (sz == 0L) errors <- c(errors, "File is empty")

  # 2 & 5. Header row present + no duplicate header column names
  sep <- SOURCE_FORMAT_FROM_PATH(src_path)$sep
  header <- tryCatch(readLines(src_path, n = 1L, warn = FALSE),
                     error = function(e) { errors <<- c(errors, sprintf("Failed to read header: %s", conditionMessage(e))); NA_character_ })
  if (length(header) == 0L || is.na(header) || !nzchar(header)) {
    errors <- c(errors, "Header row missing or empty")
    return(list(passed = FALSE, errors = errors, warnings = warnings, info = info))
  }
  header <- sub("^\xef\xbb\xbf", "", header, useBytes = TRUE)  # strip UTF-8 BOM
  cols <- tolower(trimws(strsplit(header, sep, fixed = TRUE)[[1]]))
  # Trailing empty cols from trailing-comma source files (e.g., IRS 2020+ 990 csvs
  # have 5 trailing empty headers) are informational, not duplicates. Drop them
  # before the duplicate check; count them on `info` for visibility.
  empty_cols <- cols == ""
  info$n_cols          <- length(cols)
  info$n_empty_cols    <- sum(empty_cols)
  cols_named <- cols[!empty_cols]

  dups <- unique(cols_named[duplicated(cols_named)])
  if (length(dups)) {
    errors <- c(errors, sprintf("Duplicate header columns: %s", paste(dups, collapse = ", ")))
  }

  # 3. Column count within ±tolerance of per-vintage expected
  exp_n <- expected_col_count(processing_year, form)
  info$expected_n_cols <- exp_n
  if (is.na(exp_n)) {
    warnings <- c(warnings,
                  sprintf("No expected column count available for %s/%d; skipping tolerance check",
                          form, processing_year))
  } else {
    tol <- EXPECTED_COL_COUNT_TOLERANCE
    lo  <- exp_n * (1 - tol); hi <- exp_n * (1 + tol)
    # Compare against named cols (exclude trailing empties)
    n_named <- length(cols_named)
    if (n_named < lo || n_named > hi) {
      errors <- c(errors,
                  sprintf("Column count %d outside ±%.0f%% of expected %d for %s/%d",
                          n_named, tol * 100, exp_n, form, processing_year))
    }
  }

  # 4. Row count > 0  (cheap: wc -l minus header)
  n_rows <- tryCatch(
    as.integer(sub(" .*", "", system2("wc", args = c("-l", shQuote(src_path)), stdout = TRUE))) - 1L,
    error = function(e) NA_integer_)
  info$n_rows <- n_rows
  if (is.na(n_rows) || n_rows < 1L) errors <- c(errors, "File has no data rows")

  passed <- length(errors) == 0L
  if (!is.null(logger)) {
    log4r::info(logger, sprintf("pre_checks %s/%d: cols=%d (exp=%s) rows=%s passed=%s",
                                form, processing_year, length(cols),
                                if (is.na(exp_n)) "?" else as.character(exp_n),
                                if (is.na(n_rows)) "?" else format(n_rows, big.mark = ","),
                                passed))
    for (e in errors)   log4r::error(logger, sprintf("  ERR  %s", e))
    for (w in warnings) log4r::warn(logger,  sprintf("  WARN %s", w))
  }

  list(passed = passed, errors = errors, warnings = warnings, info = info)
}
