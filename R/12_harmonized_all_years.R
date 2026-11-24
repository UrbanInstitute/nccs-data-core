# R/12_harmonized_all_years.R
#
# Builds the three kinds of file that complete the corrected "harmonized"
# CORE folder (ADR 0056 Decision 4 and Decision 6), from the yearly files
# that R/run_harmonized_rewrite.R has already written and checked:
#
#   1. the all-years file of each file kind,
#   2. the dictionary of each file kind,
#   3. the name table.
#
# Terms used in this file
#
#   file kind           One of the four kinds of yearly file. See the top of
#                       R/10_harmonized_compat.R.
#   replacement folder  The local folder that mirrors harmonized/core/ and
#                       holds the corrected files. See the top of
#                       R/11_harmonized_upload.R.
#   yearly file         The parquet file of one file kind for one year, for
#                       example 501c3-pz/marts/CORE-2005-501C3-CHARITIES-PZ-HRMN-V1.parquet.
#   all-years file      One parquet file per file kind that holds the rows of
#                       every yearly file of that kind, first year to 2023,
#                       for example 501c3-pz/full/CORE-FULL-501C3-CHARITIES-PZ-HRMN-V1.parquet.
#   column profile      A table with one row per column of one parquet file:
#                       its type, how many rows are not blank, the sum of its
#                       values (numeric columns only) and how many values
#                       carry the signature of the 2025 defect.
#   column type         The type one column has in the all-years file. It is
#                       chosen once from the yearly files, by a written rule,
#                       and never left to the software to guess.
#   dictionary          One CSV per file kind with one or more rows per
#                       column: what the column holds, which form line it
#                       comes from, and four measures of the all-years file.
#   frozen dictionary   The dictionary as published in April 2025, kept in
#                       data/crosswalks/harmonized_format/frozen_dictionaries/.
#                       Its descriptions are right; its four measures were
#                       taken from the damaged files.
#   name table          The table that links old column names to current
#                       ones, published as dd/HARMONIZED-COLUMN-MAP.csv.
#   build record        _build_all_years.json in the replacement folder. It
#                       lists every file this script wrote with its sha256
#                       and says whether every check passed. The upload plan
#                       is refused without it.
#
# What this file does not do: it does not change any yearly file, it does
# not read or write S3, and it does not upload anything.
#
# Usage (from the repo root, on the EC2 instance that holds the build):
#   Rscript R/12_harmonized_all_years.R <replacement_root>

source(file.path("R", "10_harmonized_compat.R"))
source(file.path("R", "11_harmonized_upload.R"))

# ---- Constants ----

FROZEN_DICTIONARY_FOLDER <- file.path(HARMONIZED_FORMAT_FOLDER, "frozen_dictionaries")

NAME_TABLE_SOURCE_PATH <- file.path("data", "crosswalks", "harmonized_column_map_FINAL.csv")

# DuckDB's names for the types that hold whole numbers and numbers with a
# fraction. A column that is a whole number in some years and a number with
# a fraction in others becomes DOUBLE in the all-years file. Every other
# mix of types stops the build.
WHOLE_NUMBER_TYPES <- c("TINYINT", "SMALLINT", "INTEGER", "BIGINT")

NUMERIC_TYPES <- c(WHOLE_NUMBER_TYPES, "DOUBLE", "FLOAT")

# Two sums of the same numbers taken in a different order can differ in the
# last digits. A difference larger than this share of the sum is a failure.
SUM_TOLERANCE <- 1e-9

# The columns of the dictionary, in order. The first ten are the columns of
# the frozen dictionary, in the capital letters it uses. `note` is added by
# this correction and follows the current rule: lower case.
DICTIONARY_COLUMNS <- c(
  "VAR_NAME_NEW", "VAR_DATA_TYPE", "VAR_NA_PERCENT", "VAR_COVERAGE",
  "VAR_CARDINALITY_PERCENT", "DESCRIPTION", "FORM", "FORM_TYPE", "FORM_PART",
  "FORM_LINE_NUMBER", "note"
)

# The columns that tell a reader what one row is. Their rows open the
# dictionary (ADR 0056 Decision 7).
KEY_COLUMNS <- c("EIN2", "TAX_YEAR")

# What the dictionary says about the five columns carried from the current
# CORE. The frozen dictionary has no rows for them.
CARRIED_COLUMN_DESCRIPTIONS <- c(
  ein = "Employer Identification Number in the form XX-XXXXXXX, as in the current CORE",
  tax_period = "The month the accounting period ends, as YYYYMM",
  source_form = "The form the row came from: 990, 990ez, or legacy_pz (a pre-2012 NCCS CORE file, which does not record the form)",
  is_amendment = "TRUE when the return is an amended return. Blank before 2012",
  extract_year = "The year of the IRS extract the row came from. Blank before 2012"
)

NOTE_ONE_ROW <- paste(
  "One row is one filing.",
  "EIN2 plus TAX_YEAR does not identify one row, because an organization can have two filings whose periods end in the same year.",
  "Use ein plus tax_period."
)

NOTE_VALUES_DIFFER <- paste(
  "The year the accounting period ends.",
  "The values in this file come from the current NCCS CORE.",
  "Row counts and some values differ from files downloaded before the November 2026 correction: see README.txt in harmonized/core/."
)

NOTE_ADDED_COLUMN <- "Added in the November 2026 correction."

NOTE_EMPTY_COLUMN <- "Blank in every row: the current CORE has no source for this column."

# ---- The yearly files ----

#' List the yearly files one all-years file is built from
#'
#' @details Stops when a year has no yearly file, so an all-years file is
#'   never built with a year left out.
#'
#' @param file_kind The file kind.
#' @param folder The file kind's folder in the replacement folder.
#' @param replacement_root The replacement folder.
#' @param first_year The first year the file kind covers.
#' @param last_year The last year to include.
#' @return A tibble with one row per year: `tax_year` and `path`.
list_yearly_files <- function(file_kind, folder, replacement_root, first_year, last_year) {
  years <- seq(first_year, last_year)

  yearly_keys <- paste0(yearly_key_stem(file_kind, folder, years), ".parquet")

  paths <- file.path(replacement_root, yearly_keys)

  absent_paths <- paths[!file.exists(paths)]

  if (length(absent_paths) > 0) {
    stop("Yearly file(s) are absent: ", paste(absent_paths, collapse = ", "))
  }

  yearly_files <- tibble::tibble(tax_year = years, path = paths)

  return(yearly_files)
}

# ---- Column profiles ----

#' Write the three measures of one column as SQL
#'
#' @param column_name The column.
#' @param column_type The column's DuckDB type.
#' @param connection A DuckDB connection, used to quote the name.
#' @return One piece of SQL that selects, for the column: the count of rows
#'   that are not blank, the sum (NULL unless the column is numeric) and the
#'   count of values with the signature of the 2025 defect (0 unless the
#'   column is numeric).
profile_sql_for_column <- function(column_name, column_type, connection) {
  quoted_name <- DBI::dbQuoteIdentifier(connection, column_name)

  sum_sql <- "NULL::DOUBLE"

  damaged_sql <- "0"

  if (column_type %in% NUMERIC_TYPES) {
    sum_sql <- sprintf("SUM(%s::DOUBLE)", quoted_name)

    damaged_sql <- sprintf(
      "COUNT(*) FILTER (WHERE %s != 0 AND ABS(%s::DOUBLE) < %s)",
      quoted_name, quoted_name, format(DAMAGED_VALUE_CEILING, scientific = TRUE)
    )
  }

  column_sql <- sprintf(
    "SELECT %s AS column_name, COUNT(%s)::DOUBLE AS n_not_blank, %s AS total, (%s)::DOUBLE AS n_damaged FROM parquet_rows",
    DBI::dbQuoteString(connection, column_name), quoted_name, sum_sql, damaged_sql
  )

  return(column_sql)
}

#' Read the columns of one parquet file and their DuckDB types
#'
#' @param path The parquet file.
#' @param connection A DuckDB connection.
#' @return A tibble with one row per column, in file order: `column_name`
#'   and `column_type`.
read_parquet_column_types <- function(path, connection) {
  describe_sql <- sprintf(
    "DESCRIBE SELECT * FROM read_parquet(%s)",
    DBI::dbQuoteString(connection, path)
  )

  described <- DBI::dbGetQuery(connection, describe_sql)

  column_types <- tibble::tibble(
    column_name = described$column_name,
    column_type = described$column_type
  )

  return(column_types)
}

#' Take the column profile of one parquet file
#'
#' @param path The parquet file.
#' @param connection A DuckDB connection.
#' @return A tibble with one row per column, in file order: `column_name`,
#'   `column_type`, `n_rows`, `n_not_blank`, `total` (NA unless numeric)
#'   and `n_damaged`.
profile_parquet_file <- function(path, connection) {
  column_types <- read_parquet_column_types(path, connection)

  column_queries <- purrr::map2_chr(
    column_types$column_name,
    column_types$column_type,
    profile_sql_for_column,
    connection = connection
  )

  profile_sql <- sprintf(
    "WITH parquet_rows AS (SELECT * FROM read_parquet(%s)) %s",
    DBI::dbQuoteString(connection, path),
    paste(column_queries, collapse = " UNION ALL ")
  )

  measures <- DBI::dbGetQuery(connection, profile_sql)

  count_sql <- sprintf(
    "SELECT COUNT(*)::DOUBLE AS n_rows FROM read_parquet(%s)",
    DBI::dbQuoteString(connection, path)
  )

  n_rows <- DBI::dbGetQuery(connection, count_sql)$n_rows

  # UNION ALL does not promise an order, so the measures are matched to the
  # columns by name.
  column_profile <- column_types |>
    dplyr::left_join(measures, by = "column_name") |>
    dplyr::mutate(n_rows = n_rows) |>
    dplyr::select(column_name, column_type, n_rows, n_not_blank, total, n_damaged)

  return(column_profile)
}

#' Take the column profile of every yearly file of one file kind
#'
#' @param yearly_files The tibble from `list_yearly_files()`.
#' @param connection A DuckDB connection.
#' @return The column profiles of all the yearly files in one tibble, with
#'   `tax_year` added.
profile_yearly_files <- function(yearly_files, connection) {
  profiles <- purrr::map(yearly_files$path, profile_parquet_file, connection = connection)

  yearly_profile <- profiles |>
    rlang::set_names(yearly_files$tax_year) |>
    purrr::list_rbind(names_to = "tax_year") |>
    dplyr::mutate(tax_year = as.integer(tax_year))

  return(yearly_profile)
}

# ---- Column types ----

#' Choose the type of one column for the all-years file
#'
#' @details
#' The rule, in order:
#' \itemize{
#'   \item Only the years in which the column holds a value count. A year
#'     in which the column is blank in every row has no say.
#'   \item When those years agree, that type is used.
#'   \item When they are all numeric types but not the same one, the
#'     column becomes DOUBLE. Dollar amounts fit in a DOUBLE exactly.
#'   \item Any other mix returns NA, and the build stops.
#'   \item When the column is blank in every year, the type of the last
#'     year is used.
#' }
#'
#' @param column_types The column's type in each year.
#' @param n_not_blank The count of rows that are not blank in each year.
#' @return One type as text, or `NA_character_` when the years disagree in
#'   a way the rule does not allow.
choose_type_of_one_column <- function(column_types, n_not_blank) {
  types_with_values <- unique(column_types[n_not_blank > 0])

  is_blank_in_every_year <- length(types_with_values) == 0

  years_agree <- length(types_with_values) == 1

  is_all_numeric <- all(types_with_values %in% NUMERIC_TYPES)

  if (is_blank_in_every_year) {
    return(column_types[[length(column_types)]])
  }

  if (years_agree) {
    return(types_with_values)
  }

  if (is_all_numeric) {
    return("DOUBLE")
  }

  return(NA_character_)
}

#' Choose the type of every column for the all-years file
#'
#' @details Applies `choose_type_of_one_column()` to each column. Stops,
#'   naming the columns and their types, when a column has types the rule
#'   does not allow to be mixed. Mixing types silently is what damaged the
#'   frozen files in 2025.
#'
#' @param yearly_profile The tibble from `profile_yearly_files()`.
#' @return A tibble with one row per column, in the order of the first
#'   yearly file: `column_name` and `column_type`.
choose_column_types <- function(yearly_profile) {
  chosen_types <- yearly_profile |>
    dplyr::arrange(tax_year) |>
    dplyr::summarise(
      types_seen = paste(unique(column_type), collapse = " and "),
      chosen_type = choose_type_of_one_column(column_type, n_not_blank),
      .by = column_name
    )

  unresolved <- dplyr::filter(chosen_types, is.na(chosen_type))

  if (nrow(unresolved) > 0) {
    stop(
      "Column(s) hold values of different types in different years: ",
      paste0(unresolved$column_name, " (", unresolved$types_seen, ")", collapse = ", ")
    )
  }

  column_types <- dplyr::select(chosen_types, column_name, column_type = chosen_type)

  return(column_types)
}

# ---- The all-years file ----

#' Stop unless every yearly file has the expected columns in the same order
#'
#' @param yearly_profile The tibble from `profile_yearly_files()`.
#' @param expected_header The frozen header plus the carried columns.
#' @return The years checked, invisibly.
check_yearly_headers <- function(yearly_profile, expected_header) {
  columns_by_year <- split(yearly_profile$column_name, yearly_profile$tax_year)

  has_expected_header <- purrr::map_lgl(columns_by_year, identical, expected_header)

  years_with_other_header <- names(columns_by_year)[!has_expected_header]

  if (length(years_with_other_header) > 0) {
    stop(
      "Yearly file(s) do not have the frozen header plus the carried columns: ",
      paste(years_with_other_header, collapse = ", ")
    )
  }

  return(invisible(names(columns_by_year)))
}

#' Write the SQL that reads one yearly file with every column cast to its type
#'
#' @param path The yearly parquet file.
#' @param column_types The tibble from `choose_column_types()`.
#' @param connection A DuckDB connection, used for quoting.
#' @return One SELECT statement as text.
select_sql_for_yearly_file <- function(path, column_types, connection) {
  quoted_names <- DBI::dbQuoteIdentifier(connection, column_types$column_name)

  cast_columns <- sprintf("CAST(%s AS %s) AS %s", quoted_names, column_types$column_type, quoted_names)

  select_sql <- sprintf(
    "SELECT %s FROM read_parquet(%s)",
    paste(cast_columns, collapse = ", "),
    DBI::dbQuoteString(connection, path)
  )

  return(select_sql)
}

#' Stack the yearly files of one file kind into its all-years file
#'
#' @details DuckDB reads each yearly file, casts every column to its
#'   chosen type and writes the rows straight to parquet, so the rows are
#'   never all in memory at once (ADR 0003).
#'
#' @param yearly_files The tibble from `list_yearly_files()`.
#' @param column_types The tibble from `choose_column_types()`.
#' @param output_path Where to write the all-years file.
#' @param connection A DuckDB connection.
#' @return `output_path`, invisibly.
write_all_years_file <- function(yearly_files, column_types, output_path, connection) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  yearly_selects <- purrr::map_chr(
    yearly_files$path,
    select_sql_for_yearly_file,
    column_types = column_types,
    connection = connection
  )

  copy_sql <- sprintf(
    "COPY (%s) TO %s (FORMAT PARQUET)",
    paste(yearly_selects, collapse = " UNION ALL "),
    DBI::dbQuoteString(connection, output_path)
  )

  DBI::dbExecute(connection, copy_sql)

  return(invisible(output_path))
}

#' Count the rows of the all-years file in each year
#'
#' @param all_years_file The all-years parquet file.
#' @param connection A DuckDB connection.
#' @return A tibble with one row per year: `tax_year` and `n_rows`.
count_rows_by_year <- function(all_years_file, connection) {
  count_sql <- sprintf(
    "SELECT \"TAX_YEAR\"::INTEGER AS tax_year, COUNT(*)::DOUBLE AS n_rows FROM read_parquet(%s) GROUP BY 1 ORDER BY 1",
    DBI::dbQuoteString(connection, all_years_file)
  )

  rows_by_year <- tibble::as_tibble(DBI::dbGetQuery(connection, count_sql))

  return(rows_by_year)
}

#' Say whether two sums are the same number, allowing for rounding
#'
#' @param first_total One sum per column; NA where the column is not numeric
#'   or holds no value.
#' @param second_total The sums to compare with.
#' @return A logical vector: TRUE where both are NA, or where the two differ
#'   by no more than `SUM_TOLERANCE` as a share of the larger one.
totals_agree <- function(first_total, second_total) {
  both_blank <- is.na(first_total) & is.na(second_total)

  largest <- pmax(abs(first_total), abs(second_total), 1)

  is_close <- abs(first_total - second_total) <= SUM_TOLERANCE * largest

  return(both_blank | dplyr::coalesce(is_close, FALSE))
}

#' Add up the yearly column profiles, one row per column
#'
#' @param yearly_profile The tibble from `profile_yearly_files()`.
#' @return A tibble with one row per column: `column_name`,
#'   `yearly_n_not_blank` and `yearly_total` (NA when no year has a sum).
sum_yearly_profile <- function(yearly_profile) {
  yearly_sums <- yearly_profile |>
    dplyr::summarise(
      yearly_n_not_blank = sum(n_not_blank),
      has_no_total = all(is.na(total)),
      yearly_total = sum(total, na.rm = TRUE),
      .by = column_name
    ) |>
    dplyr::mutate(yearly_total = dplyr::if_else(has_no_total, NA_real_, yearly_total)) |>
    dplyr::select(column_name, yearly_n_not_blank, yearly_total)

  return(yearly_sums)
}

#' Check one all-years file against the yearly files it was built from
#'
#' @details
#' The checks, each of which adds a message when it fails:
#' \itemize{
#'   \item The header is the frozen header plus the carried columns.
#'   \item Every column has its chosen type.
#'   \item Each year holds as many rows as its yearly file.
#'   \item Each column holds as many values that are not blank as the
#'     yearly files together.
#'   \item Each numeric column adds up to the same sum as the yearly files
#'     together. A value changed by a cast would move the sum.
#'   \item No value carries the signature of the 2025 defect.
#' }
#'
#' @param all_years_profile The column profile of the all-years file.
#' @param rows_by_year The tibble from `count_rows_by_year()`.
#' @param yearly_profile The tibble from `profile_yearly_files()`.
#' @param column_types The tibble from `choose_column_types()`.
#' @param expected_header The frozen header plus the carried columns.
#' @return A character vector of failure messages, empty when every check
#'   passes.
check_all_years_file <- function(all_years_profile,
                                 rows_by_year,
                                 yearly_profile,
                                 column_types,
                                 expected_header) {
  failures <- character(0)

  # Header and types
  if (!identical(all_years_profile$column_name, expected_header)) {
    failures <- c(failures, "header differs from the frozen header plus carried columns")
  }

  if (!identical(all_years_profile$column_type, column_types$column_type)) {
    failures <- c(failures, "a column does not have its chosen type")
  }

  # Rows in each year
  yearly_rows <- dplyr::distinct(yearly_profile, tax_year, n_rows)

  rows_compared <- dplyr::full_join(
    yearly_rows, rows_by_year,
    by = "tax_year", suffix = c("_yearly", "_all_years")
  )

  has_same_rows <- dplyr::coalesce(rows_compared$n_rows_yearly == rows_compared$n_rows_all_years, FALSE)

  if (!all(has_same_rows)) {
    failures <- c(
      failures,
      paste("row count differs from the yearly file in:", paste(rows_compared$tax_year[!has_same_rows], collapse = ", "))
    )
  }

  # Values in each column
  columns_compared <- dplyr::full_join(
    all_years_profile, sum_yearly_profile(yearly_profile),
    by = "column_name"
  )

  has_same_count <- dplyr::coalesce(columns_compared$n_not_blank == columns_compared$yearly_n_not_blank, FALSE)

  has_same_total <- totals_agree(columns_compared$total, columns_compared$yearly_total)

  is_damaged <- dplyr::coalesce(columns_compared$n_damaged > 0, FALSE)

  if (!all(has_same_count)) {
    failures <- c(
      failures,
      paste("count of values differs from the yearly files in:", paste(columns_compared$column_name[!has_same_count], collapse = ", "))
    )
  }

  if (!all(has_same_total)) {
    failures <- c(
      failures,
      paste("sum differs from the yearly files in:", paste(columns_compared$column_name[!has_same_total], collapse = ", "))
    )
  }

  if (any(is_damaged)) {
    failures <- c(
      failures,
      paste("damaged values present in:", paste(columns_compared$column_name[is_damaged], collapse = ", "))
    )
  }

  return(failures)
}

# ---- The dictionary ----

#' Write a set of years as text, with runs of years joined
#'
#' @param years The years, in any order.
#' @return One string, oldest first, for example "2000; 2002; 2004-2023".
#'   An empty string when there are no years.
format_year_coverage <- function(years) {
  if (length(years) == 0) {
    return("")
  }

  sorted_years <- sort(unique(years))

  # A new run starts wherever a year does not follow the one before it.
  starts_new_run <- c(TRUE, diff(sorted_years) != 1)

  first_of_run <- sorted_years[starts_new_run]

  # The last year of a run is the year before the next run starts.
  ends_run <- c(starts_new_run[-1], TRUE)

  last_of_run <- sorted_years[ends_run]

  run_text <- dplyr::if_else(
    first_of_run == last_of_run,
    as.character(first_of_run),
    paste0(first_of_run, "-", last_of_run)
  )

  return(paste(run_text, collapse = "; "))
}

#' Count the different values of one column of a parquet file
#'
#' @param column_name The column.
#' @param path The parquet file.
#' @param connection A DuckDB connection.
#' @return The count of different values, blanks not counted.
count_distinct_values <- function(column_name, path, connection) {
  count_sql <- sprintf(
    "SELECT COUNT(DISTINCT %s)::DOUBLE AS n_distinct FROM read_parquet(%s)",
    DBI::dbQuoteIdentifier(connection, column_name),
    DBI::dbQuoteString(connection, path)
  )

  return(DBI::dbGetQuery(connection, count_sql)$n_distinct)
}

#' Take the four measures the dictionary gives for every column
#'
#' @details The measures keep the meaning they have in the frozen
#'   dictionary: the type, the percent of rows that are blank, the years in
#'   which the column holds at least one value, and the count of different
#'   values as a percent of the rows.
#'
#' @param all_years_profile The column profile of the all-years file.
#' @param yearly_profile The tibble from `profile_yearly_files()`.
#' @param all_years_file The all-years parquet file.
#' @param connection A DuckDB connection.
#' @return A tibble with one row per column, in file order: `VAR_NAME_NEW`,
#'   `VAR_DATA_TYPE`, `VAR_NA_PERCENT`, `VAR_COVERAGE`,
#'   `VAR_CARDINALITY_PERCENT` and `is_empty`.
measure_columns <- function(all_years_profile, yearly_profile, all_years_file, connection) {
  n_distinct <- purrr::map_dbl(
    all_years_profile$column_name,
    count_distinct_values,
    path = all_years_file,
    connection = connection
  )

  coverage <- yearly_profile |>
    dplyr::summarise(
      VAR_COVERAGE = format_year_coverage(tax_year[n_not_blank > 0]),
      .by = column_name
    )

  column_measures <- all_years_profile |>
    dplyr::mutate(n_distinct = n_distinct) |>
    dplyr::left_join(coverage, by = "column_name") |>
    dplyr::transmute(
      VAR_NAME_NEW = column_name,
      VAR_DATA_TYPE = column_type,
      VAR_NA_PERCENT = 100 * (n_rows - n_not_blank) / n_rows,
      VAR_COVERAGE = VAR_COVERAGE,
      VAR_CARDINALITY_PERCENT = 100 * n_distinct / n_rows,
      is_empty = n_not_blank == 0
    )

  return(column_measures)
}

#' Read the frozen dictionary of one file kind
#'
#' @param file_kind The file kind.
#' @param dictionary_folder The folder that holds the frozen dictionaries.
#' @return The dictionary as a tibble of text columns, with `file_kind` added.
read_frozen_dictionary <- function(file_kind, dictionary_folder = FROZEN_DICTIONARY_FOLDER) {
  path <- file.path(dictionary_folder, sprintf("DD-CORE-%s-HRMN-V1.csv", file_kind))

  frozen_dictionary <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character())
  )

  frozen_dictionary$file_kind <- file_kind

  return(frozen_dictionary)
}

#' Give the descriptive rows of every column of one file kind
#'
#' @details
#' A column's rows come from the file kind's own frozen dictionary. The
#' frozen dictionary of 501CE-NONPROFIT-PZ leaves out 194 of its 296
#' columns, so a column with no row there takes its rows from the first
#' other file kind, in the order of `frozen_dictionaries`, that describes
#' it. An old name means the same form line in every file kind. A column
#' no frozen dictionary describes gets one row with a blank description.
#' Four columns of 501CE-NONPROFIT-PZ are in that position:
#' F9_01_REV_OTH_CY, SC_02_AVE_EXP_GRASS_NONTAX_CY, F9_03_PROG_CODE_5 and
#' F9_03_PROG_CODE_6.
#'
#' @param file_kind The file kind the dictionary is for.
#' @param file_columns The columns of the all-years file, in order.
#' @param frozen_dictionaries The frozen dictionaries of all four file
#'   kinds in one tibble, from `read_frozen_dictionary()`.
#' @return A tibble with one or more rows per column: `VAR_NAME_NEW`,
#'   `DESCRIPTION`, `FORM`, `FORM_TYPE`, `FORM_PART` and `FORM_LINE_NUMBER`.
describe_columns <- function(file_kind, file_columns, frozen_dictionaries) {
  kinds_in_order <- unique(c(file_kind, frozen_dictionaries$file_kind))

  # For each column, keep the rows of the first file kind that describes it.
  descriptions <- frozen_dictionaries |>
    dplyr::filter(VAR_NAME_NEW %in% file_columns) |>
    dplyr::mutate(kind_position = match(.data$file_kind, kinds_in_order)) |>
    dplyr::filter(kind_position == min(kind_position), .by = VAR_NAME_NEW) |>
    dplyr::select(VAR_NAME_NEW, DESCRIPTION, FORM, FORM_TYPE, FORM_PART, FORM_LINE_NUMBER)

  carried_descriptions <- tibble::tibble(
    VAR_NAME_NEW = names(CARRIED_COLUMN_DESCRIPTIONS),
    DESCRIPTION = unname(CARRIED_COLUMN_DESCRIPTIONS)
  )

  all_columns <- tibble::tibble(VAR_NAME_NEW = file_columns)

  column_descriptions <- dplyr::bind_rows(descriptions, carried_descriptions)

  # The join keeps every column of the file, described or not.
  described_columns <- dplyr::left_join(all_columns, column_descriptions, by = "VAR_NAME_NEW")

  return(described_columns)
}

#' Give the note the dictionary carries for each column
#'
#' @param column_names The column of each dictionary row.
#' @param is_empty Whether that column is blank in every row of the file.
#' @return A character vector of notes, NA where there is nothing to say.
note_for_columns <- function(column_names, is_empty) {
  is_carried <- column_names %in% CARRIED_COLUMNS

  notes <- dplyr::case_when(
    column_names == "EIN2" ~ NOTE_ONE_ROW,
    column_names == "TAX_YEAR" ~ NOTE_VALUES_DIFFER,
    is_carried ~ NOTE_ADDED_COLUMN,
    is_empty ~ NOTE_EMPTY_COLUMN,
    .default = NA_character_
  )

  return(notes)
}

#' Build the dictionary of one file kind
#'
#' @details
#' The descriptions come from the frozen dictionaries and the four measures
#' from the corrected all-years file. The rows for EIN2, TAX_YEAR and the
#' five carried columns come first, so the first lines say what one row is
#' (ADR 0056 Decision 7). The other columns follow in file order.
#'
#' @param described_columns The tibble from `describe_columns()`.
#' @param column_measures The tibble from `measure_columns()`.
#' @return The dictionary as a tibble with the columns `DICTIONARY_COLUMNS`.
build_dictionary <- function(described_columns, column_measures) {
  leading_columns <- c(KEY_COLUMNS, CARRIED_COLUMNS)

  dictionary <- described_columns |>
    dplyr::left_join(column_measures, by = "VAR_NAME_NEW") |>
    dplyr::mutate(
      note = note_for_columns(VAR_NAME_NEW, is_empty),
      leading_position = match(VAR_NAME_NEW, leading_columns),
      file_position = match(VAR_NAME_NEW, column_measures$VAR_NAME_NEW)
    ) |>
    dplyr::arrange(leading_position, file_position) |>
    dplyr::select(dplyr::all_of(DICTIONARY_COLUMNS))

  return(dictionary)
}

# ---- The name table ----

#' Copy the name table into the replacement folder
#'
#' @param replacement_root The replacement folder.
#' @param source_path The name table in this repository.
#' @return The path written. Stops if the copy fails.
write_name_table <- function(replacement_root, source_path = NAME_TABLE_SOURCE_PATH) {
  output_path <- file.path(replacement_root, NAME_TABLE_KEY)

  copy_file_creating_folder(source_path, output_path)

  return(output_path)
}

# ---- One file kind, then all of them ----

#' Describe one written file for the build record
#'
#' @param key The file's key in the replacement folder.
#' @param replacement_root The replacement folder.
#' @return A list with the file's `key`, `bytes` and `sha256`.
describe_written_file <- function(key, replacement_root) {
  path <- file.path(replacement_root, key)

  return(list(key = key, bytes = as.numeric(file.size(path)), sha256 = sha256_of_file(path)))
}

#' Build and check the all-years file and the dictionary of one file kind
#'
#' @param file_kind The file kind.
#' @param folder The file kind's folder in the replacement folder.
#' @param first_year The first year the file kind covers.
#' @param replacement_root The replacement folder.
#' @param last_year The last year to include.
#' @param frozen_dictionaries The frozen dictionaries of all file kinds.
#' @param connection A DuckDB connection.
#' @return A list for the build record: `file_kind`, `row_count`, the two
#'   `files` written and every `failures` message.
build_all_years_of_kind <- function(file_kind,
                                    folder,
                                    first_year,
                                    replacement_root,
                                    last_year,
                                    frozen_dictionaries,
                                    connection) {
  message("Building the all-years file and dictionary of ", file_kind)

  expected_header <- c(read_frozen_columns(file_kind), CARRIED_COLUMNS)

  # Read the yearly files
  yearly_files <- list_yearly_files(file_kind, folder, replacement_root, first_year, last_year)

  yearly_profile <- profile_yearly_files(yearly_files, connection)

  check_yearly_headers(yearly_profile, expected_header)

  column_types <- choose_column_types(yearly_profile)

  # Write and check the all-years file
  all_years_file <- file.path(replacement_root, all_years_key(file_kind, folder))

  write_all_years_file(yearly_files, column_types, all_years_file, connection)

  all_years_profile <- profile_parquet_file(all_years_file, connection)

  rows_by_year <- count_rows_by_year(all_years_file, connection)

  failures <- check_all_years_file(all_years_profile, rows_by_year, yearly_profile, column_types, expected_header)

  # Write the dictionary
  column_measures <- measure_columns(all_years_profile, yearly_profile, all_years_file, connection)

  described_columns <- describe_columns(file_kind, all_years_profile$column_name, frozen_dictionaries)

  dictionary <- build_dictionary(described_columns, column_measures)

  dictionary_file <- file.path(replacement_root, dictionary_key(file_kind, folder))

  dir.create(dirname(dictionary_file), recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(dictionary, dictionary_file, na = "")

  kind_summary <- list(
    file_kind = file_kind,
    row_count = sum(rows_by_year$n_rows),
    files = purrr::map(
      c(all_years_key(file_kind, folder), dictionary_key(file_kind, folder)),
      describe_written_file,
      replacement_root = replacement_root
    ),
    failures = paste(file_kind, failures, recycle0 = TRUE)
  )

  return(kind_summary)
}

#' Build the all-years files, the dictionaries and the name table
#'
#' @details Writes the build record `_build_all_years.json` last. Its
#'   `passed` is TRUE only when no check failed for any file kind.
#'
#' @param replacement_root The replacement folder.
#' @param file_kinds The file kinds to build: a tibble with `file_kind`,
#'   `folder` and `first_year`.
#' @param last_year The last year to include.
#' @param dictionary_folder The folder that holds the frozen dictionaries.
#' @return The build record, as a list.
build_all_years_files <- function(replacement_root,
                                  file_kinds = UPLOAD_FILE_KINDS,
                                  last_year = LAST_YEAR_REPLACED,
                                  dictionary_folder = FROZEN_DICTIONARY_FOLDER) {
  connection <- DBI::dbConnect(duckdb::duckdb())

  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  # The frozen dictionaries of all four file kinds, whichever are built,
  # because one kind can borrow a description from another.
  frozen_dictionaries <- names(FILE_KIND_FOLDERS) |>
    purrr::map(read_frozen_dictionary, dictionary_folder = dictionary_folder) |>
    purrr::list_rbind()

  kind_summaries <- purrr::pmap(
    list(
      file_kind = file_kinds$file_kind,
      folder = file_kinds$folder,
      first_year = file_kinds$first_year
    ),
    build_all_years_of_kind,
    replacement_root = replacement_root,
    last_year = last_year,
    frozen_dictionaries = frozen_dictionaries,
    connection = connection
  )

  write_name_table(replacement_root)

  failures <- unlist(purrr::map(kind_summaries, "failures"), use.names = FALSE)

  written_files <- c(
    purrr::list_flatten(purrr::map(kind_summaries, "files")),
    list(describe_written_file(NAME_TABLE_KEY, replacement_root))
  )

  build_record <- list(
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    last_year = last_year,
    row_counts = purrr::map(kind_summaries, "row_count") |> rlang::set_names(file_kinds$file_kind),
    files = written_files,
    failures = as.list(failures),
    passed = length(failures) == 0
  )

  jsonlite::write_json(
    build_record,
    file.path(replacement_root, ALL_YEARS_BUILD_RECORD),
    auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  return(build_record)
}

# ---- Command line ----

if (sys.nframe() == 0) {
  arguments <- commandArgs(trailingOnly = TRUE)

  build_record <- build_all_years_files(arguments[[1]])

  purrr::walk(build_record$failures, message)

  message("All-years files, dictionaries and name table: ", if (build_record$passed) "passed" else "FAILED")

  if (!build_record$passed) {
    quit(status = 1L)
  }
}
