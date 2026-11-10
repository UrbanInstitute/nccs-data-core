# R/run_harmonized_rewrite.R
# Builds the corrected "harmonized" CORE yearly files for a range of years
# (ADR 0056), from local mirrors of processed/core/ and
# processed_legacy/core/, into a local folder that mirrors harmonized/core/.
# Runs every check the decision record requires and writes one summary
# JSON per year. Output file names are the frozen keys (HRMN-V1), so the
# output folder mirrors harmonized/core/ exactly. Does NOT upload: the upload order is a separate,
# maintainer-run step (ADR 0056 Decision 13).
#
# Usage:
#   Rscript R/run_harmonized_rewrite.R <first_year> <last_year> <current_root> <legacy_root> <output_root>
#
#   current_root  local mirror of s3://nccsdata/processed/core/
#   legacy_root   local mirror of s3://nccsdata/processed_legacy/core/
#   output_root   local folder to write into (mirrors harmonized/core/)

source(file.path("R", "10_harmonized_compat.R"))

#' Build, write and check one period-end year
#'
#' @param tax_year The year.
#' @param current_root Local mirror of `processed/core/`.
#' @param legacy_root Local mirror of `processed_legacy/core/`.
#' @param output_root Local output folder.
#' @return A list summarising the year: per family row counts, repeated
#'   `ein` plus `tax_period`, and every failure message. `passed` is TRUE
#'   only when no check failed.
build_harmonized_year <- function(tax_year, current_root, legacy_root, output_root) {
  column_map <- read_harmonized_column_map(tax_year)

  if (tax_year >= 2012L) {
    source_tables <- list(
      "990" = select_source_rows(current_root, "990", tax_year),
      "990ez" = select_source_rows(current_root, "990ez", tax_year)
    )
    failures <- character(0)
    families <- build_harmonized_families(source_tables[["990"]], source_tables[["990ez"]], column_map)
  } else {
    source_tables <- list(
      "legacy_pz" = select_source_rows(legacy_root, "990combined", tax_year)
    )
    failures <- check_legacy_prerequisite(source_tables[["legacy_pz"]])
    families <- build_harmonized_families_legacy(source_tables[["legacy_pz"]], column_map)
  }

  if (sum(vapply(source_tables, nrow, integer(1))) == 0) {
    failures <- c(failures, "no source rows selected for this year")
  }

  failures <- c(failures, check_families_against_source(families, source_tables, column_map))

  family_summaries <- list()

  for (family_token in names(families)) {
    paths <- write_family_year(families[[family_token]], family_token, tax_year, output_root)
    result <- check_family_file(families[[family_token]], family_token, paths)
    if (!result$passed) {
      failures <- c(failures, paste(family_token, result$failures))
    }
    family_summaries[[family_token]] <- list(
      row_count = result$row_count,
      repeated_ein_tax_period = result$repeated_ein_tax_period,
      csv = basename(paths[["csv"]]),
      parquet = basename(paths[["parquet"]])
    )
  }

  summary <- list(
    tax_year = tax_year,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source_rows = lapply(source_tables, nrow),
    source_rows_with_unknown_period = lapply(source_tables, function(table) attr(table, "n_unknown_period")),
    families = family_summaries,
    failures = failures,
    passed = length(failures) == 0
  )

  summary_path <- file.path(output_root, sprintf("_build_%d.json", tax_year))
  jsonlite::write_json(summary, summary_path, auto_unbox = TRUE, pretty = TRUE)

  summary
}

if (sys.nframe() == 0) {
  arguments <- commandArgs(trailingOnly = TRUE)
  first_year <- as.integer(arguments[[1]])
  last_year <- as.integer(arguments[[2]])
  current_root <- arguments[[3]]
  legacy_root <- arguments[[4]]
  output_root <- arguments[[5]]

  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  all_passed <- TRUE

  for (tax_year in seq(first_year, last_year)) {
    summary <- build_harmonized_year(tax_year, current_root, legacy_root, output_root)
    status <- if (summary$passed) "passed" else "FAILED"
    message(sprintf("%d: %s", tax_year, status))
    for (failure in summary$failures) {
      message("   ", failure)
    }
    all_passed <- all_passed && summary$passed
  }

  if (!all_passed) {
    quit(status = 1L)
  }
}
