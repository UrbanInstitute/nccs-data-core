# R/run_harmonized_rewrite.R
#
# Builds the corrected "harmonized" CORE yearly files for a range of years
# (ADR 0056) from local mirrors of processed/core/ and processed_legacy/core/,
# into a local folder that mirrors harmonized/core/. Output file names are
# the frozen keys (HRMN-V1), so the output folder mirrors the destination.
#
# Runs every check the decision record requires and writes one summary
# JSON per year. Does NOT upload: the upload is a separate, hand-run step
# with its own order and safeguards (ADR 0056 Decision 13).
#
# Usage:
#   Rscript R/run_harmonized_rewrite.R <first_year> <last_year> <current_root> <legacy_root> <output_root>
#
#   current_root  local mirror of s3://nccsdata/processed/core/
#   legacy_root   local mirror of s3://nccsdata/processed_legacy/core/
#   output_root   local folder to write into (mirrors harmonized/core/)
#
# "File kind" means one of the four kinds of yearly file; see the top of
# R/10_harmonized_compat.R.

source(file.path("R", "10_harmonized_compat.R"))

# ---- Build one year ----

#' Select the source rows and build the file kinds for one year from 2012 on
#'
#' @param tax_year The period-end year.
#' @param current_root Local mirror of `processed/core/`.
#' @param column_map The name table rows for the year.
#' @return A list with `source_tables` (by form), `kind_tables` and
#'   `failures` (always empty here; kept for the same shape as the
#'   pre-2012 builder).
build_year_from_2012 <- function(tax_year, current_root, column_map) {
  source_tables <- list(
    "990" = select_source_rows(current_root, "990", tax_year),
    "990ez" = select_source_rows(current_root, "990ez", tax_year)
  )

  kind_tables <- build_file_kinds(source_tables[["990"]], source_tables[["990ez"]], column_map)

  return(list(source_tables = source_tables, kind_tables = kind_tables, failures = character(0)))
}

#' Select the source rows and build the file kinds for one pre-2012 year
#'
#' @param tax_year The period-end year.
#' @param legacy_root Local mirror of `processed_legacy/core/`.
#' @param column_map The name table rows for the year.
#' @return A list with `source_tables`, `kind_tables` and any `failures`
#'   from the ADR 0057 prerequisite check.
build_year_before_2012 <- function(tax_year, legacy_root, column_map) {
  source_tables <- list(
    "legacy_pz" = select_source_rows(legacy_root, "990combined", tax_year)
  )

  failures <- check_legacy_prerequisite(source_tables[["legacy_pz"]])
  kind_tables <- build_file_kinds_legacy(source_tables[["legacy_pz"]], column_map)

  return(list(source_tables = source_tables, kind_tables = kind_tables, failures = failures))
}

#' Write and check one file kind for one year
#'
#' @param kind_table The file kind's table.
#' @param file_kind The file kind.
#' @param tax_year The period-end year.
#' @param output_root Local output folder.
#' @return A list with the written file names, row count, repeated-key
#'   count and any failure messages, for the year's summary.
write_and_check_kind <- function(kind_table, file_kind, tax_year, output_root) {
  paths <- write_file_kind_year(kind_table, file_kind, tax_year, output_root)
  check_result <- check_written_file(kind_table, file_kind, paths)

  failures <- character(0)

  if (!check_result$passed) {
    failures <- paste(file_kind, check_result$failures)
  }

  kind_summary <- list(
    row_count = check_result$row_count,
    repeated_ein_tax_period = check_result$repeated_ein_tax_period,
    csv = basename(paths[["csv"]]),
    parquet = basename(paths[["parquet"]]),
    failures = failures
  )

  return(kind_summary)
}

#' Build, write and check one period-end year
#'
#' @param tax_year The year.
#' @param current_root Local mirror of `processed/core/`.
#' @param legacy_root Local mirror of `processed_legacy/core/`.
#' @param output_root Local output folder.
#' @return A list summarising the year: source row counts, per file kind
#'   row counts and repeated keys, every failure message, and `passed`,
#'   which is TRUE only when no check failed. The same list is written to
#'   `{output_root}/_build_{year}.json`.
build_harmonized_year <- function(tax_year, current_root, legacy_root, output_root) {
  column_map <- read_harmonized_column_map(tax_year)

  if (tax_year >= 2012L) {
    built <- build_year_from_2012(tax_year, current_root, column_map)
  } else {
    built <- build_year_before_2012(tax_year, legacy_root, column_map)
  }

  source_tables <- built$source_tables
  kind_tables <- built$kind_tables
  failures <- built$failures

  n_source_rows <- sum(purrr::map_int(source_tables, nrow))

  if (n_source_rows == 0) {
    failures <- c(failures, "no source rows selected for this year")
  }

  failures <- c(failures, check_file_kinds_against_source(kind_tables, source_tables, column_map))

  kind_summaries <- purrr::imap(kind_tables, write_and_check_kind, tax_year = tax_year, output_root = output_root)

  kind_failures <- purrr::map(kind_summaries, "failures")
  failures <- c(failures, unlist(kind_failures, use.names = FALSE))

  year_summary <- list(
    tax_year = tax_year,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source_rows = purrr::map(source_tables, nrow),
    source_rows_with_unknown_period = purrr::map(source_tables, attr, which = "n_unknown_period"),
    file_kinds = kind_summaries,
    failures = failures,
    passed = length(failures) == 0
  )

  summary_path <- file.path(output_root, sprintf("_build_%d.json", tax_year))
  jsonlite::write_json(year_summary, summary_path, auto_unbox = TRUE, pretty = TRUE)

  return(year_summary)
}

# ---- Command line ----

#' Report one year's result on the console
#'
#' @param year_summary The list from `build_harmonized_year()`.
#' @return TRUE when the year passed.
report_year <- function(year_summary) {
  status <- if (year_summary$passed) "passed" else "FAILED"
  message(sprintf("%d: %s", year_summary$tax_year, status))

  purrr::walk(year_summary$failures, message, "   ")

  return(year_summary$passed)
}

if (sys.nframe() == 0) {
  arguments <- commandArgs(trailingOnly = TRUE)

  first_year <- as.integer(arguments[[1]])
  last_year <- as.integer(arguments[[2]])
  current_root <- arguments[[3]]
  legacy_root <- arguments[[4]]
  output_root <- arguments[[5]]

  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  years <- seq(first_year, last_year)

  year_summaries <- purrr::map(
    years, build_harmonized_year,
    current_root = current_root, legacy_root = legacy_root, output_root = output_root
  )

  years_passed <- purrr::map_lgl(year_summaries, report_year)

  if (!all(years_passed)) {
    quit(status = 1L)
  }
}
