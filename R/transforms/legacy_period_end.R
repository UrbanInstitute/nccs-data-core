# R/transforms/legacy_period_end.R
# Check, and for one file rebuild, the period end in the pre-2012 NCCS CORE
# files (backlog U13; legacy pipeline).
#
# Terms used in this file:
#   period end        The month a return's accounting period ended, written
#                     YYYYMM. The raw files carry it in the column TAXPER.
#                     The legacy pipeline sorts every row into a tax year by
#                     this value.
#   fiscal year       The year the accounting period ended. Raw column FISYR.
#   accounting month  The month the accounting period ended, 1 to 12. Raw
#                     column ACCPER.
#   rebuilt period end  The fiscal year followed by the accounting month.
#                     In a healthy file it equals the period end on every
#                     row.
#
# Why this exists: in CORE-1999-501C3-CHARITIES-PZ.csv the period end is
# blank on 11,071 of 249,886 rows and its year is wrong on most of the rest
# (it equals the fiscal year on 33,959 rows only). The fiscal year and the
# accounting month in that file are intact. In the 1998 and 2000 charity
# files and the 1999 nonprofit file the rebuilt period end equals the period
# end on every row, so the rebuilt value is a safe replacement.
#
# The check runs on every file, so a second file with the same damage stops
# the build and does not publish rows under the wrong year.

# The raw files whose period end is replaced by the rebuilt period end.
LEGACY_FILES_WITH_DAMAGED_PERIOD_END <- c("CORE-1999-501C3-CHARITIES-PZ.csv")

# The lowest share of rows whose period end may equal the rebuilt period end
# before the build stops. Measured on all 89 raw files on 2026-11-11: the
# damaged 1999 file scores 0.136 and every other file scores 0.990 or more.
LEGACY_PERIOD_END_MINIMUM_AGREEMENT <- 0.95

# The raw columns (lower case, as the legacy reader leaves them) needed to
# rebuild or check a period end.
LEGACY_PERIOD_END_COLUMNS <- c("taxper", "fisyr", "accper")

#' Rebuild a period end from the fiscal year and the accounting month
#'
#' @param fiscal_year character or integer vector: the year the accounting
#'   period ended (raw column FISYR).
#' @param accounting_month character or integer vector, the same length:
#'   the month the accounting period ended (raw column ACCPER).
#' @return character vector of period ends written YYYYMM. A row is `NA`
#'   when its fiscal year is not exactly four digits or its accounting
#'   month is not 1 to 12.
#' @details The month is padded to two digits, so "6" and "06" both give
#'   "...06".
rebuild_period_end <- function(fiscal_year, accounting_month) {
  year_text <- trimws(as.character(fiscal_year))
  month_number <- suppressWarnings(as.integer(accounting_month))

  is_usable_year <- grepl("^[0-9]{4}$", year_text)
  is_usable_month <- month_number %in% 1:12

  rebuilt_period_end <- sprintf("%s%02d", year_text, month_number)
  rebuilt_period_end[!(is_usable_year & is_usable_month)] <- NA_character_

  return(rebuilt_period_end)
}

#' Share of rows whose period end equals the rebuilt period end
#'
#' @param source_rows A data.table of raw legacy rows holding the columns
#'   `taxper`, `fisyr` and `accper`.
#' @return A single number from 0 to 1. A row with a blank period end counts
#'   as not equal. Returns 1 for a table with no rows.
#' @details This is the measure the check uses to tell a healthy file from a
#'   damaged one.
share_of_period_ends_confirmed <- function(source_rows) {
  if (nrow(source_rows) == 0L) {
    return(1)
  }

  period_end <- trimws(as.character(source_rows$taxper))
  rebuilt_period_end <- rebuild_period_end(source_rows$fisyr, source_rows$accper)

  is_confirmed <- !is.na(period_end) & !is.na(rebuilt_period_end) & period_end == rebuilt_period_end

  return(mean(is_confirmed))
}

#' Check the period end of one raw legacy file, and rebuild it if the file
#' is listed as damaged
#'
#' @param source_rows A data.table of raw legacy rows, column names in lower
#'   case, before the crosswalk renames them.
#' @param file_name The name of the raw file the rows came from, without its
#'   folder, for example "CORE-1999-501C3-CHARITIES-PZ.csv".
#' @param logger A log4r logger.
#' @return `source_rows`, invisibly. For a file listed in
#'   `LEGACY_FILES_WITH_DAMAGED_PERIOD_END` the column `taxper` now holds
#'   the rebuilt period end. Every other file is returned unchanged.
#' @details Three cases.
#'   A file listed as damaged: `taxper` is replaced on every row. A row
#'   whose fiscal year or accounting month is unusable gets a blank period
#'   end, and the pipeline then files it under the year in the file name.
#'   Any other file that has all three columns: the build stops if fewer
#'   than `LEGACY_PERIOD_END_MINIMUM_AGREEMENT` of its rows have a period
#'   end equal to the rebuilt period end.
#'   A file that lacks one of the three columns: nothing can be compared,
#'   so the function logs a warning and returns the rows unchanged.
check_or_rebuild_legacy_period_end <- function(source_rows, file_name, logger) {
  missing_columns <- setdiff(LEGACY_PERIOD_END_COLUMNS, names(source_rows))
  is_listed_as_damaged <- file_name %in% LEGACY_FILES_WITH_DAMAGED_PERIOD_END
  can_compare <- length(missing_columns) == 0L

  # A listed file must be rebuildable; failing here is better than
  # publishing its rows under the wrong year.
  if (is_listed_as_damaged && !can_compare) {
    stop(sprintf("%s is listed for a rebuilt period end but lacks: %s",
                 file_name, paste(missing_columns, collapse = ", ")))
  }

  if (!can_compare) {
    log4r::warn(logger, sprintf("%s: period end not checked, missing column(s): %s",
                                file_name, paste(missing_columns, collapse = ", ")))
    return(invisible(source_rows))
  }

  share_confirmed <- share_of_period_ends_confirmed(source_rows)

  # The damaged file: replace the period end.
  if (is_listed_as_damaged) {
    rebuilt_period_end <- rebuild_period_end(source_rows$fisyr, source_rows$accper)
    source_rows[, taxper := rebuilt_period_end]

    log4r::info(logger, sprintf(
      "%s: period end rebuilt from fiscal year and accounting month on %d rows (%.1f%% already agreed; %d could not be rebuilt and are blank)",
      file_name, nrow(source_rows), 100 * share_confirmed, sum(is.na(rebuilt_period_end))
    ))
    return(invisible(source_rows))
  }

  # Every other file: stop if it shows the same damage.
  if (share_confirmed < LEGACY_PERIOD_END_MINIMUM_AGREEMENT) {
    stop(sprintf(
      "%s: only %.1f%% of rows have a period end (TAXPER) equal to fiscal year plus accounting month (FISYR, ACCPER). Inspect the file; if TAXPER is damaged, add the file to LEGACY_FILES_WITH_DAMAGED_PERIOD_END.",
      file_name, 100 * share_confirmed
    ))
  }

  return(invisible(source_rows))
}
