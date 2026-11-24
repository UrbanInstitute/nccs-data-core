# tests/test_harmonized_all_years.R
# Tests for R/12_harmonized_all_years.R (ADR 0056 Decisions 4 and 6): the
# type each column gets in the all-years file, the all-years file and its
# checks, the dictionary, the name table and the build record. Small
# parquet files written to a temporary folder stand in for the yearly files.
# Run from repo root:  Rscript tests/test_harmonized_all_years.R
#
# "File kind", "yearly file", "all-years file", "column profile" and
# "frozen dictionary" are defined at the top of R/12_harmonized_all_years.R.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "12_harmonized_all_years.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

error_message <- function(expr) {
  tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
}

# ---- A small replacement folder: one file kind, three years ----

small_kind <- "501C3-CHARITIES-PZ"

small_file_kinds <- tibble::tribble(
  ~file_kind,  ~folder,     ~first_year,
  small_kind,  "501c3-pz",  2021L
)

small_expected_header <- c(read_frozen_columns(small_kind), CARRIED_COLUMNS)

# The one frozen column that the small yearly files leave blank in every year.
empty_column <- "F9_09_EXP_INFO_TECH_TOT"

#' Build the rows of one small yearly file with the full header
#'
#' @param tax_year The year.
#' @param eins The organizations in the file.
#' @param assets Total assets at the end of the year, one per organization.
#' @return A tibble with every expected column. Columns not set here are
#'   blank numbers, as `conform_to_frozen_header()` leaves them.
small_yearly_table <- function(tax_year, eins, assets) {
  yearly_table <- tibble::tibble(.rows = length(eins))

  for (column_name in small_expected_header) {
    yearly_table[[column_name]] <- NA_real_
  }

  yearly_table$EIN2 <- paste0("EIN-", eins)
  yearly_table$TAX_YEAR <- as.integer(tax_year)
  yearly_table$F9_10_ASSET_TOT_EOY <- assets
  yearly_table$ein <- eins
  yearly_table$tax_period <- paste0(tax_year, "12")
  yearly_table$source_form <- "990"
  yearly_table$is_amendment <- FALSE
  yearly_table$extract_year <- as.integer(tax_year) + 1L

  return(yearly_table)
}

write_small_yearly_file <- function(replacement_root, yearly_table) {
  tax_year <- yearly_table$TAX_YEAR[[1]]
  path <- file.path(
    replacement_root, "501c3-pz", "marts",
    sprintf("CORE-%d-%s-HRMN-V1.parquet", tax_year, small_kind)
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(yearly_table, path)
  return(invisible(path))
}

# 2021 holds assets as whole numbers and 2022 and 2023 as numbers with a
# fraction. 2021 has no amendment flag or extract year, like a pre-2012 file.
make_replacement_folder <- function() {
  replacement_root <- tempfile("harmonized_all_years_")

  table_2021 <- small_yearly_table(2021L, c("06-0964699", "94-1153307"), c(100L, 250L))
  table_2021$is_amendment <- NA
  table_2021$extract_year <- NA

  table_2022 <- small_yearly_table(2022L, c("06-0964699", "94-1153307", "11-1111111"), c(5576975.5, 0, 2500))
  table_2023 <- small_yearly_table(2023L, "06-0964699", 6000000)

  purrr::walk(
    list(table_2021, table_2022, table_2023),
    write_small_yearly_file,
    replacement_root = replacement_root
  )

  return(replacement_root)
}

read_small_all_years <- function(replacement_root) {
  return(arrow::read_parquet(file.path(replacement_root, all_years_key(small_kind, "501c3-pz"))))
}

read_small_dictionary <- function(replacement_root) {
  return(readr::read_csv(file.path(replacement_root, dictionary_key(small_kind, "501c3-pz")), show_col_types = FALSE))
}

# ---- The type of one column ----

check(
  "years that agree keep their type",
  choose_type_of_one_column(c("VARCHAR", "VARCHAR"), c(2, 3)) == "VARCHAR"
)

check(
  "whole numbers in one year and fractions in another become DOUBLE",
  choose_type_of_one_column(c("INTEGER", "DOUBLE", "BIGINT"), c(2, 3, 1)) == "DOUBLE"
)

check(
  "a year in which the column is blank has no say in the type",
  choose_type_of_one_column(c("BOOLEAN", "VARCHAR"), c(0, 3)) == "VARCHAR"
)

check(
  "a column blank in every year takes the type of the last year",
  choose_type_of_one_column(c("BOOLEAN", "DOUBLE"), c(0, 0)) == "DOUBLE"
)

check(
  "a column blank in every year takes the last year's type even when that is text",
  choose_type_of_one_column(c("DOUBLE", "VARCHAR"), c(0, 0)) == "VARCHAR"
)

check(
  "a year with a single value has a say in the type",
  is.na(choose_type_of_one_column(c("BOOLEAN", "VARCHAR"), c(1, 3)))
)

check(
  "a column filled in an early year only keeps that year's type",
  choose_type_of_one_column(c("VARCHAR", "DOUBLE"), c(2, 0)) == "VARCHAR"
)

check(
  "text in one year and numbers in another is not resolved",
  is.na(choose_type_of_one_column(c("VARCHAR", "DOUBLE"), c(2, 3)))
)

check(
  "a column that cannot be resolved stops the build and is named with its types",
  {
    mixed_profile <- tibble::tibble(
      tax_year = c(2021L, 2022L),
      column_name = "F9_00_ORG_NAME_L1",
      column_type = c("VARCHAR", "DOUBLE"),
      n_not_blank = c(2, 3)
    )
    mixed_message <- error_message(choose_column_types(mixed_profile))
    grepl("F9_00_ORG_NAME_L1", mixed_message) && grepl("VARCHAR and DOUBLE", mixed_message)
  }
)

# ---- Years written as text ----

check("one run of years is written as a span", format_year_coverage(2012:2023) == "2012-2023")

check(
  "single years and runs are written oldest first",
  format_year_coverage(c(2004:2023, 2000, 2002)) == "2000; 2002; 2004-2023"
)

check("no years is written as an empty string", format_year_coverage(integer(0)) == "")

# ---- Sums compared with rounding allowed ----

check("two sums that differ in the last digits agree", totals_agree(1e12, 1e12 + 0.0001))

check("two sums that differ by a dollar in a million do not agree", !totals_agree(1e6, 1e6 + 1))

check(
  "two small sums that differ by more than a billionth do not agree",
  !totals_agree(1, 1 + 1.5e-9)
)

check("two blank sums agree, and a blank sum does not agree with a number", {
  identical(totals_agree(c(NA, NA), c(NA, 5)), c(TRUE, FALSE))
})

# ---- A full build of the small folder ----

built_root <- make_replacement_folder()

build_record <- build_all_years_files(built_root, file_kinds = small_file_kinds)

all_years_table <- read_small_all_years(built_root)

check("the build passes its own checks", isTRUE(build_record$passed) && length(build_record$failures) == 0)

check("the all-years file holds the rows of every yearly file", nrow(all_years_table) == 6)

check(
  "the all-years file has the frozen header plus the carried columns, in order",
  identical(names(all_years_table), small_expected_header)
)

check(
  "assets held as whole numbers in 2021 keep their values beside the later years",
  identical(sort(all_years_table$F9_10_ASSET_TOT_EOY), c(0, 100, 250, 2500, 5576975.5, 6000000))
)

check(
  "the amendment flag is blank for the year that has none and kept for the others",
  {
    flags_2021 <- all_years_table$is_amendment[all_years_table$TAX_YEAR == 2021]
    flags_later <- all_years_table$is_amendment[all_years_table$TAX_YEAR > 2021]
    all(is.na(flags_2021)) && identical(flags_later, rep(FALSE, 4))
  }
)

check(
  "the build record lists the all-years file, the dictionary and the name table with their sha256",
  {
    recorded_keys <- purrr::map_chr(build_record$files, "key")
    recorded_sha256 <- purrr::map_chr(build_record$files, "sha256")
    sha256_on_disk <- purrr::map_chr(file.path(built_root, recorded_keys), sha256_of_file)
    setequal(
      recorded_keys,
      c(
        "501c3-pz/full/CORE-FULL-501C3-CHARITIES-PZ-HRMN-V1.parquet",
        "501c3-pz/dd/DD-CORE-501C3-CHARITIES-PZ-HRMN-V1.csv",
        "dd/HARMONIZED-COLUMN-MAP.csv"
      )
    ) && identical(recorded_sha256, sha256_on_disk)
  }
)

check(
  "the build record lists every yearly file the build read, with its sha256",
  {
    input_keys <- purrr::map_chr(build_record$inputs, "key")
    input_sha256 <- purrr::map_chr(build_record$inputs, "sha256")
    identical(input_keys, paste0(yearly_key_stem(small_kind, "501c3-pz", 2021:2023), ".parquet")) &&
      identical(input_sha256, purrr::map_chr(file.path(built_root, input_keys), sha256_of_file))
  }
)

check(
  "the build record on disk says the build passed and gives the row count",
  {
    record_on_disk <- jsonlite::fromJSON(file.path(built_root, ALL_YEARS_BUILD_RECORD))
    isTRUE(record_on_disk$passed) && record_on_disk$row_counts[[small_kind]] == 6
  }
)

check(
  "the upload plan's check accepts the files this build wrote",
  {
    built_files <- list_replacement_files(built_root)
    checked_keys <- check_all_years_evidence(built_root, built_files)
    length(checked_keys) == 6
  }
)

check(
  "the name table is a byte-for-byte copy of the one in the repository",
  sha256_of_file(file.path(built_root, NAME_TABLE_KEY)) == sha256_of_file(NAME_TABLE_SOURCE_PATH)
)

check(
  "a missing year stops the build and names the file",
  {
    short_root <- make_replacement_folder()
    unlink(file.path(short_root, "501c3-pz", "marts", "CORE-2022-501C3-CHARITIES-PZ-HRMN-V1.parquet"))
    short_message <- error_message(build_all_years_files(short_root, file_kinds = small_file_kinds))
    grepl("Yearly file(s) are absent", short_message, fixed = TRUE) && grepl("CORE-2022-", short_message)
  }
)

check(
  "a yearly file with a column out of place stops the build and names the year",
  {
    reordered_root <- make_replacement_folder()
    reordered_table <- small_yearly_table(2023L, "06-0964699", 6000000)
    write_small_yearly_file(reordered_root, reordered_table[rev(names(reordered_table))])
    reordered_message <- error_message(build_all_years_files(reordered_root, file_kinds = small_file_kinds))
    grepl("do not have the frozen header", reordered_message) && grepl("2023", reordered_message)
  }
)

check(
  "a damaged value in a yearly file fails the build, and the record names the file kind and the column",
  {
    damaged_root <- make_replacement_folder()
    write_small_yearly_file(damaged_root, small_yearly_table(2023L, "06-0964699", 1.11e-308))
    damaged_record <- build_all_years_files(damaged_root, file_kinds = small_file_kinds)
    record_on_disk <- jsonlite::fromJSON(file.path(damaged_root, ALL_YEARS_BUILD_RECORD))
    isFALSE(damaged_record$passed) &&
      isFALSE(record_on_disk$passed) &&
      any(grepl("501C3-CHARITIES-PZ damaged values present in: F9_10_ASSET_TOT_EOY", damaged_record$failures))
  }
)

# ---- The checks on the all-years file ----

small_connection <- DBI::dbConnect(duckdb::duckdb())

small_yearly_files <- list_yearly_files(small_kind, "501c3-pz", built_root, 2021L, 2023L)

small_yearly_profile <- profile_yearly_files(small_yearly_files, small_connection)

small_column_types <- choose_column_types(small_yearly_profile)

small_all_years_file <- file.path(built_root, all_years_key(small_kind, "501c3-pz"))

small_all_years_profile <- profile_parquet_file(small_all_years_file, small_connection)

small_rows_by_year <- count_rows_by_year(small_all_years_file, small_connection)

#' Run the checks with one piece of the evidence changed
check_with <- function(all_years_profile = small_all_years_profile,
                       rows_by_year = small_rows_by_year) {
  return(check_all_years_file(
    all_years_profile, rows_by_year, small_yearly_profile, small_column_types, small_expected_header
  ))
}

change_assets_measure <- function(measure, value) {
  changed_profile <- small_all_years_profile
  changed_profile[[measure]][changed_profile$column_name == "F9_10_ASSET_TOT_EOY"] <- value
  return(changed_profile)
}

check("the file as built has no failures", length(check_with()) == 0)

check(
  "the profile of the all-years file counts the values and adds them up",
  {
    assets_row <- small_all_years_profile[small_all_years_profile$column_name == "F9_10_ASSET_TOT_EOY", ]
    assets_row$n_rows == 6 && assets_row$n_not_blank == 6 && assets_row$total == 11579825.5 &&
      assets_row$column_type == "DOUBLE" && assets_row$n_damaged == 0
  }
)

check(
  "a year with a row short is a failure that names the year",
  {
    short_rows <- small_rows_by_year
    short_rows$n_rows[short_rows$tax_year == 2022] <- 2
    any(grepl("row count differs.*2022", check_with(rows_by_year = short_rows)))
  }
)

check(
  "a year absent from the all-years file is a failure",
  any(grepl("row count differs.*2023", check_with(rows_by_year = small_rows_by_year[1:2, ])))
)

check(
  "a column with a value lost is a failure that names the column",
  any(grepl("count of values differs.*F9_10_ASSET_TOT_EOY", check_with(change_assets_measure("n_not_blank", 5))))
)

check(
  "a column whose sum moved is a failure that names the column",
  any(grepl("sum differs.*F9_10_ASSET_TOT_EOY", check_with(change_assets_measure("total", 11579826.5))))
)

check(
  "a damaged value is a failure that names the column",
  any(grepl("damaged values present.*F9_10_ASSET_TOT_EOY", check_with(change_assets_measure("n_damaged", 1))))
)

check(
  "a column that does not have its chosen type is a failure",
  any(grepl("does not have its chosen type", check_with(change_assets_measure("column_type", "VARCHAR"))))
)

check(
  "a column absent from the all-years file is a failure that names the column",
  {
    profile_without_assets <- small_all_years_profile[small_all_years_profile$column_name != "F9_10_ASSET_TOT_EOY", ]
    any(grepl("count of values differs.*F9_10_ASSET_TOT_EOY", check_with(profile_without_assets)))
  }
)

check(
  "a column in the wrong place is a failure",
  any(grepl("header differs", check_with(small_all_years_profile[rev(seq_len(nrow(small_all_years_profile))), ])))
)

check(
  "the profile counts a value with the signature of the 2025 defect",
  {
    damaged_path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(tibble::tibble(amount = c(1.11e-308, 0, 5, NA)), damaged_path)
    damaged_profile <- profile_parquet_file(damaged_path, small_connection)
    damaged_profile$n_damaged == 1 && damaged_profile$n_not_blank == 3 && damaged_profile$n_rows == 4
  }
)

DBI::dbDisconnect(small_connection, shutdown = TRUE)

# ---- The dictionary ----

small_dictionary <- read_small_dictionary(built_root)

dictionary_row <- function(column_name) {
  return(small_dictionary[small_dictionary$VAR_NAME_NEW == column_name, ][1, ])
}

check(
  "the dictionary has the ten frozen columns and note, in order",
  identical(names(small_dictionary), DICTIONARY_COLUMNS)
)

check(
  "the dictionary describes every column of the file and nothing else",
  setequal(small_dictionary$VAR_NAME_NEW, small_expected_header)
)

check(
  "the first rows are EIN2, TAX_YEAR and the five carried columns",
  identical(
    unique(small_dictionary$VAR_NAME_NEW)[1:7],
    c("EIN2", "TAX_YEAR", "ein", "tax_period", "source_form", "is_amendment", "extract_year")
  )
)

check(
  "after the first rows the columns follow the file's order",
  {
    later_columns <- unique(small_dictionary$VAR_NAME_NEW)[-(1:7)]
    identical(later_columns, setdiff(small_expected_header, c(KEY_COLUMNS, CARRIED_COLUMNS)))
  }
)

check(
  "the EIN2 row says what one row is",
  grepl("One row is one filing", dictionary_row("EIN2")$note) &&
    grepl("ein plus tax_period", dictionary_row("EIN2")$note)
)

check(
  "the TAX_YEAR row says the values differ from earlier downloads",
  grepl("differ from files downloaded before", dictionary_row("TAX_YEAR")$note)
)

check(
  "a carried column has a description and is marked as added",
  grepl("XX-XXXXXXX", dictionary_row("ein")$DESCRIPTION) &&
    dictionary_row("ein")$note == NOTE_ADDED_COLUMN
)

check(
  "a column blank in every row says so, with 100 percent blank and no years",
  {
    empty_row <- dictionary_row(empty_column)
    empty_row$note == NOTE_EMPTY_COLUMN && empty_row$VAR_NA_PERCENT == 100 && is.na(empty_row$VAR_COVERAGE)
  }
)

check(
  "a filled column keeps its frozen description and gets measures from the corrected file",
  {
    assets_row <- dictionary_row("F9_10_ASSET_TOT_EOY")
    grepl("assets", assets_row$DESCRIPTION, ignore.case = TRUE) &&
      assets_row$VAR_DATA_TYPE == "DOUBLE" &&
      assets_row$VAR_NA_PERCENT == 0 &&
      assets_row$VAR_COVERAGE == "2021-2023" &&
      assets_row$VAR_CARDINALITY_PERCENT == 100 &&
      is.na(assets_row$note)
  }
)

check(
  "a column blank in one year reports the years it is filled and the share of blank rows",
  {
    amendment_row <- dictionary_row("is_amendment")
    amendment_row$VAR_COVERAGE == "2022-2023" && abs(amendment_row$VAR_NA_PERCENT - 100 * 2 / 6) < 1e-9
  }
)

check(
  "every row of a column that has several rows carries the same measures",
  {
    measures_per_column <- small_dictionary |>
      dplyr::distinct(VAR_NAME_NEW, VAR_DATA_TYPE, VAR_NA_PERCENT, VAR_COVERAGE, VAR_CARDINALITY_PERCENT) |>
      dplyr::count(VAR_NAME_NEW)
    all(measures_per_column$n == 1)
  }
)

# ---- Descriptions borrowed from another file kind ----

toy_frozen_dictionaries <- tibble::tribble(
  ~VAR_NAME_NEW, ~DESCRIPTION,           ~FORM,  ~FORM_TYPE, ~FORM_PART, ~FORM_LINE_NUMBER, ~file_kind,
  "SHARED",      "own description",      "F990", "PC",       "PART-01",  "Line 1",          "501CE-NONPROFIT-PZ",
  "SHARED",      "other description",    "F990", "PC",       "PART-01",  "Line 1",          "501C3-CHARITIES-PZ",
  "BORROWED",    "borrowed, first row",  "F990", "PC",       "PART-02",  "Line 2",          "501C3-CHARITIES-PZ",
  "BORROWED",    "borrowed, second row", "F990", "EZ",       "PART-02",  "Line 2",          "501C3-CHARITIES-PZ",
  "NOT_IN_FILE", "not wanted",           "F990", "PC",       "PART-03",  "Line 3",          "501CE-NONPROFIT-PZ"
)

toy_described <- describe_columns(
  "501CE-NONPROFIT-PZ",
  c("SHARED", "BORROWED", "UNDESCRIBED", "ein"),
  toy_frozen_dictionaries
)

check(
  "a column the file kind's own frozen dictionary describes keeps that description",
  identical(toy_described$DESCRIPTION[toy_described$VAR_NAME_NEW == "SHARED"], "own description")
)

check(
  "a column it leaves out borrows every row from another file kind",
  identical(
    toy_described$DESCRIPTION[toy_described$VAR_NAME_NEW == "BORROWED"],
    c("borrowed, first row", "borrowed, second row")
  )
)

check(
  "a column no frozen dictionary describes gets one row with a blank description",
  identical(toy_described$DESCRIPTION[toy_described$VAR_NAME_NEW == "UNDESCRIBED"], NA_character_)
)

check(
  "a described column that is not in the file gets no row",
  !"NOT_IN_FILE" %in% toy_described$VAR_NAME_NEW
)

#' List the columns of one file kind that no frozen dictionary describes
undescribed_columns <- function(file_kind, frozen_dictionaries) {
  return(setdiff(read_frozen_columns(file_kind), frozen_dictionaries$VAR_NAME_NEW))
}

real_frozen_dictionaries <- names(FILE_KIND_FOLDERS) |>
  purrr::map(read_frozen_dictionary) |>
  purrr::list_rbind()

real_undescribed <- names(FILE_KIND_FOLDERS) |>
  rlang::set_names() |>
  purrr::map(undescribed_columns, frozen_dictionaries = real_frozen_dictionaries)

check(
  "the real frozen dictionaries together describe every column of three file kinds",
  all(lengths(real_undescribed[c("501C3-CHARITIES-PC", "501C3-CHARITIES-PZ", "501CE-NONPROFIT-PC")]) == 0)
)

check(
  "four columns of 501CE-NONPROFIT-PZ are described nowhere, and no others",
  setequal(
    real_undescribed[["501CE-NONPROFIT-PZ"]],
    c("F9_01_REV_OTH_CY", "SC_02_AVE_EXP_GRASS_NONTAX_CY", "F9_03_PROG_CODE_5", "F9_03_PROG_CODE_6")
  )
)

# ---- Standalone summary ----

if (!exists("TEST_RUN_ALL")) {
  cat(sprintf("\n=== %d passed, %d failed ===\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
