# R/10_harmonized_compat.R
#
# Rebuilds CORE files in the older "harmonized" format (column names such
# as F9_09_EXP_INFO_TECH_TOT, key EIN2) from the current per-form CORE
# files. ADR 0056.
#
# Terms used in this file
#
#   file kind      One of the four kinds of yearly file in the older format.
#                  Each kind is a separate file per year with its own fixed
#                  list of columns:
#                    501C3-CHARITIES-PC   charities, full Form 990 only
#                    501C3-CHARITIES-PZ   charities, Form 990 or 990-EZ
#                    501CE-NONPROFIT-PC   other nonprofits, full Form 990 only
#                    501CE-NONPROFIT-PZ   other nonprofits, Form 990 or 990-EZ
#   frozen header  The exact column list of a file kind as published in the
#                  frozen files (data/crosswalks/harmonized_format/columns_*.txt).
#   old name       A column name in the older format, e.g. F9_10_ASSET_TOT_EOY.
#   current name   A column name in the current CORE, e.g. total_assets_eoy.
#   name table     The published table linking old names to current names
#                  (data/crosswalks/harmonized_column_map_FINAL.csv).
#   source row id  A text id ({form}:{folder year}:{row number}) given to each
#                  source row when it is read, so a row can be traced from the
#                  source to the written file.
#
# What this file does: reads the current Form 990 and Form 990-EZ files for
# one tax year, renames their columns to the old names using the name
# table, stacks the two forms, and splits the result into the four file
# kinds. It then checks the result against the source.
#
# What it does not do: it does not change any value, it does not touch the
# frozen files under harmonized/core/, and it does not publish.

# ---- Constants ----

# Which rows each file kind holds: charities or other nonprofits, and which
# forms. "PC" means full Form 990 only. "PZ" means Form 990 or Form 990-EZ.
FILE_KINDS <- tibble::tribble(
  ~file_kind,             ~is_501c3, ~source_forms,
  "501C3-CHARITIES-PC",   TRUE,      list("990"),
  "501C3-CHARITIES-PZ",   TRUE,      list("990", "990ez"),
  "501CE-NONPROFIT-PC",   FALSE,     list("990"),
  "501CE-NONPROFIT-PZ",   FALSE,     list("990", "990ez")
)

# Folder of each file kind under harmonized/core/.
FILE_KIND_FOLDERS <- c(
  "501C3-CHARITIES-PC" = "501c3-pc",
  "501C3-CHARITIES-PZ" = "501c3-pz",
  "501CE-NONPROFIT-PC" = "501ce-pc",
  "501CE-NONPROFIT-PZ" = "501ce-pz"
)

# Current-CORE columns copied unchanged beside the old names during the
# build. One row is one filing and amended returns are kept, so EIN2 plus
# TAX_YEAR is not a unique key; these columns let a user tell rows apart.
# `is_501c3` is used for the split into file kinds and then removed. The
# pre-2012 source has no `is_amendment` or `extract_year`; those are carried
# as missing there.
ROW_CONTEXT_COLUMNS <- c("ein", "tax_period", "is_amendment", "extract_year", "is_501c3")

# Current-CORE columns published after the frozen header, in this order.
CARRIED_COLUMNS <- c("ein", "tax_period", "source_form", "is_amendment", "extract_year")

# The internal column that traces a row back to its source. Used by the
# checks and dropped before writing.
SOURCE_ROW_ID <- "source_row_id"

# Folder holding one file per file kind listing its frozen header
# (captured 2026-11-09, identical across years within a kind).
HARMONIZED_FORMAT_FOLDER <- file.path("data", "crosswalks", "harmonized_format")

# A real dollar amount is never this close to zero without being zero.
# Values in this range are the signature of the 2025 defect (ADR 0056).
DAMAGED_VALUE_CEILING <- 1e-300

# ---- Name table ----

#' Say whether a tax year falls inside a span written as text
#'
#' @param years_text A span such as "1989-2011", a single year such as
#'   "2011", or "all".
#' @param tax_year One tax year as an integer.
#' @return TRUE when `tax_year` is inside the span.
#' @details A single year is a span of one. "all" matches every year.
year_in_span <- function(years_text, tax_year) {
  if (years_text == "all") {
    return(TRUE)
  }

  bounds <- as.integer(stringr::str_split_1(years_text, "-"))
  first_year <- bounds[[1]]
  last_year <- bounds[[length(bounds)]]

  return(tax_year >= first_year && tax_year <= last_year)
}

#' Read the rows of the name table that apply to one year
#'
#' @param tax_year The tax year (period-end year) being built.
#' @param column_map_path Path to the FINAL name table written by
#'   `scripts/draft_harmonized_column_map.R`.
#' @return A tibble with one row per (old name, source form) that is
#'   `mapped` for that year, holding `old_name`, `source_form`,
#'   `current_name` and `transform`.
#' @details Rows with status `empty` are not returned: those columns are
#'   added, with no values, by `conform_to_frozen_header()`. A row whose
#'   span of years does not include `tax_year` is left out, so an old name
#'   can take its values from different current columns in different
#'   years (the Schedule A columns do).
read_harmonized_column_map <- function(
    tax_year,
    column_map_path = file.path("data", "crosswalks", "harmonized_column_map_FINAL.csv")) {
  name_table <- readr::read_csv(
    column_map_path,
    col_types = readr::cols(.default = readr::col_character())
  )

  name_table <- dplyr::filter(name_table, status == "mapped")

  applies_to_year <- purrr::map_lgl(name_table$years, year_in_span, tax_year = tax_year)
  name_table <- name_table[applies_to_year, ]

  name_table$transform[is.na(name_table$transform)] <- ""

  name_table <- dplyr::distinct(name_table, old_name, source_form, current_name, transform)

  return(name_table)
}

# ---- Rename one form ----

#' The name-table rows that apply to one form and one table
#'
#' @param column_map The name table rows for the year.
#' @param source_form "990", "990ez" or "legacy_pz".
#' @param current_table The table being renamed.
#' @return The rows of `column_map` for `source_form` whose current column
#'   exists in `current_table`.
#' @details A current column absent from the table (for example a Form
#'   990-only item on the 990-EZ table) is simply not mapped there.
column_map_for_table <- function(column_map, source_form, current_table) {
  wanted_form <- source_form

  rows_for_form <- dplyr::filter(
    column_map,
    .data$source_form == wanted_form,
    .data$current_name %in% names(current_table)
  )

  return(rows_for_form)
}

#' Rename one current CORE table to the old column names
#'
#' @param current_table A table from `select_source_rows()`: it carries
#'   `period_end_year` and `source_row_id`.
#' @param source_form The form the table holds: "990", "990ez" or "legacy_pz".
#' @param column_map The name table from `read_harmonized_column_map()`.
#' @return A tibble with the old column names, plus `TAX_YEAR` (the
#'   period-end year, never the source's `tax_year`, whose meaning may
#'   change under ADR 0053), `EIN2`, `source_form`, `source_row_id` and the
#'   columns in `ROW_CONTEXT_COLUMNS`. Columns of `current_table` that have
#'   no old name are dropped.
#' @details When two old names point at the same current column, both
#'   old columns are created and hold the same values. `EIN2` is the
#'   current `ein` (XX-XXXXXXX) with the prefix "EIN-".
rename_to_harmonized_format <- function(current_table, source_form, column_map) {
  form_map <- column_map_for_table(column_map, source_form, current_table)

  renamed_table <- tibble::tibble(.rows = nrow(current_table))

  for (row_number in seq_len(nrow(form_map))) {
    old_name <- form_map$old_name[[row_number]]
    current_name <- form_map$current_name[[row_number]]

    renamed_table[[old_name]] <- current_table[[current_name]]
  }

  renamed_table$TAX_YEAR <- as.integer(current_table$period_end_year)
  renamed_table$EIN2 <- stringr::str_c("EIN-", current_table$ein)
  renamed_table$source_form <- source_form
  renamed_table[[SOURCE_ROW_ID]] <- current_table[[SOURCE_ROW_ID]]

  for (column_name in ROW_CONTEXT_COLUMNS) {
    renamed_table[[column_name]] <- column_or_missing(current_table, column_name)
  }

  return(renamed_table)
}

#' A column of a table, or NA for every row when the table lacks it
#'
#' @param table The table.
#' @param column_name The column wanted.
#' @return The column, or a vector of NA of the table's row count.
column_or_missing <- function(table, column_name) {
  if (column_name %in% names(table)) {
    return(table[[column_name]])
  }

  return(rep(NA, nrow(table)))
}

# ---- Frozen header ----

#' Read the frozen header of one file kind
#'
#' @param file_kind A file kind from `FILE_KINDS`, for example
#'   "501C3-CHARITIES-PZ".
#' @return A character vector of column names in the frozen files' order.
read_frozen_columns <- function(file_kind) {
  header_path <- file.path(HARMONIZED_FORMAT_FOLDER, paste0("columns_", file_kind, ".txt"))

  frozen_columns <- readLines(header_path)

  return(frozen_columns)
}

#' Give a table the exact column set and order of one file kind
#'
#' @param kind_table The rows of one file kind, renamed to old names.
#' @param file_kind The file kind from `FILE_KINDS`.
#' @return The same rows with: every frozen column present, in the frozen
#'   order (a column the source cannot fill is all NA); then the carried
#'   current-CORE columns (`CARRIED_COLUMNS`); then `source_row_id`.
#' @details Keeping the old header whole means code written against the
#'   frozen files keeps running. A column the source cannot fill is empty
#'   rather than missing, and the dictionary lists it as such. Any other
#'   column the rename produced that the frozen files did not have is
#'   dropped, so the header never grows beyond the frozen one plus the
#'   carried columns.
conform_to_frozen_header <- function(kind_table, file_kind) {
  frozen_columns <- read_frozen_columns(file_kind)
  missing_columns <- setdiff(frozen_columns, names(kind_table))

  for (column_name in missing_columns) {
    kind_table[[column_name]] <- NA_real_
  }

  final_order <- c(frozen_columns, CARRIED_COLUMNS, SOURCE_ROW_ID)
  kind_table <- dplyr::select(kind_table, dplyr::all_of(final_order))

  return(kind_table)
}

# ---- Split into the four file kinds ----

#' Take one file kind out of a renamed, stacked table
#'
#' @param stacked A renamed table that still has `is_501c3`.
#' @param file_kind The file kind wanted.
#' @param wants_501c3 TRUE for a charities kind, FALSE for other nonprofits.
#' @return The kind's rows, without `is_501c3`, in the frozen column order.
take_file_kind <- function(stacked, file_kind, wants_501c3) {
  kind_table <- dplyr::filter(stacked, .data$is_501c3 == wants_501c3)
  kind_table <- dplyr::select(kind_table, -is_501c3)
  kind_table <- conform_to_frozen_header(kind_table, file_kind)

  return(kind_table)
}

#' Build the four file kinds for one tax year from 2012 on
#'
#' @param form_990_table The selected Form 990 rows for the year.
#' @param form_990ez_table The selected Form 990-EZ rows for the year.
#' @param column_map The name table from `read_harmonized_column_map()`.
#' @return A named list of four tibbles, one per file kind in `FILE_KINDS`.
#' @details The two forms are stacked by column name. A column that only
#'   one form has is missing (NA) on the other form's rows. The split
#'   uses `is_501c3`, which is never missing in the current CORE, so every
#'   source row lands in exactly one charities or nonprofit kind.
build_file_kinds <- function(form_990_table, form_990ez_table, column_map) {
  renamed_by_form <- list(
    "990" = rename_to_harmonized_format(form_990_table, "990", column_map),
    "990ez" = rename_to_harmonized_format(form_990ez_table, "990ez", column_map)
  )

  kind_tables <- list()

  for (row_number in seq_len(nrow(FILE_KINDS))) {
    file_kind <- FILE_KINDS$file_kind[[row_number]]
    wants_501c3 <- FILE_KINDS$is_501c3[[row_number]]
    source_forms <- unlist(FILE_KINDS$source_forms[[row_number]])

    stacked <- dplyr::bind_rows(renamed_by_form[source_forms])

    kind_tables[[file_kind]] <- take_file_kind(stacked, file_kind, wants_501c3)
  }

  return(kind_tables)
}

#' Build the two PZ file kinds for one pre-2012 year
#'
#' @param legacy_table The selected `processed_legacy/core/{year}/990combined`
#'   rows for the year, from `select_source_rows()`.
#' @param column_map The name table rows for the year.
#' @return A named list of two tibbles: `501C3-CHARITIES-PZ` and
#'   `501CE-NONPROFIT-PZ`.
#' @details The split uses `source_subsection_class`, the class the row
#'   carried in the original NCCS file, because about 1% of pre-2012 rows
#'   have no subsection code. Rows of class 501C3-PRIVFOUND are left out.
#'   There are no PC kinds before 2012.
build_file_kinds_legacy <- function(legacy_table, column_map) {
  legacy_table <- dplyr::filter(legacy_table, .data$source_subsection_class != "501C3-PRIVFOUND")
  legacy_table$is_501c3 <- legacy_table$source_subsection_class == "501C3-CHARITIES"

  renamed <- rename_to_harmonized_format(legacy_table, "legacy_pz", column_map)

  kind_tables <- list(
    "501C3-CHARITIES-PZ" = take_file_kind(renamed, "501C3-CHARITIES-PZ", TRUE),
    "501CE-NONPROFIT-PZ" = take_file_kind(renamed, "501CE-NONPROFIT-PZ", FALSE)
  )

  return(kind_tables)
}

# ---- Source rows for one period-end year ----

#' Path of one source file
#'
#' @param source_root Local mirror root.
#' @param folder_year The year folder.
#' @param form The form folder: "990", "990ez" or "990combined".
#' @return The parquet path, whether or not it exists.
source_file_path <- function(source_root, folder_year, form) {
  file_name <- sprintf("core_%d_%s.parquet", folder_year, form)

  return(file.path(source_root, folder_year, form, file_name))
}

#' Read one source file and keep the rows whose period ends in one year
#'
#' @param path The parquet file.
#' @param form The form folder, used in the source row id.
#' @param folder_year The year folder, used in the source row id.
#' @param wanted_year The period-end year wanted.
#' @return The matching rows with `source_row_id` and `period_end_year` added.
#' @details A row with no `tax_period` keeps the year its producer assigned
#'   it (`tax_year`, the filename fallback of the legacy pipeline).
read_rows_ending_in_year <- function(path, form, folder_year, wanted_year) {
  source_rows <- arrow::read_parquet(path)

  source_rows$tax_period <- as.character(source_rows$tax_period)
  source_rows[[SOURCE_ROW_ID]] <- sprintf("%s:%d:%d", form, folder_year, seq_len(nrow(source_rows)))

  year_from_period <- suppressWarnings(as.integer(substr(source_rows$tax_period, 1, 4)))
  year_from_producer <- as.integer(source_rows$tax_year)
  source_rows$period_end_year <- dplyr::if_else(is.na(year_from_period), year_from_producer, year_from_period)

  ends_in_wanted_year <- !is.na(source_rows$period_end_year) & source_rows$period_end_year == wanted_year
  matching_rows <- source_rows[ends_in_wanted_year, ]

  return(matching_rows)
}

#' Select the source rows whose accounting period ends in one year
#'
#' @param source_root Local folder mirroring `processed/core/` (2012 on) or
#'   `processed_legacy/core/` (before 2012):
#'   `{root}/{year}/{form}/core_{year}_{form}.parquet`.
#' @param form The form folder: "990", "990ez" or "990combined".
#' @param tax_year The period-end year wanted.
#' @return A tibble of the selected rows with `period_end_year` and
#'   `source_row_id` added. The attribute `n_unknown_period` counts
#'   selected rows with no period.
#' @details The folder a row sits in is not trusted for the year. Today
#'   the folders are by period-end year. If ADR 0053 is accepted and rows
#'   move to folders by starting year, a return ending June 2023 sits
#'   under `2022/`. So the `tax_year - 1`, `tax_year` and `tax_year + 1`
#'   folders are all read and the rows are selected by `tax_period`.
#'   The `tax_year` folder itself must exist for the form: a missing
#'   required input stops the build rather than producing an empty file.
select_source_rows <- function(source_root, form, tax_year) {
  wanted_year <- as.integer(tax_year)
  required_path <- source_file_path(source_root, wanted_year, form)

  if (!file.exists(required_path)) {
    stop("Required source input is missing: ", required_path)
  }

  folder_years <- c(wanted_year - 1L, wanted_year, wanted_year + 1L)
  candidate_paths <- purrr::map_chr(folder_years, source_file_path, source_root = source_root, form = form)
  present <- file.exists(candidate_paths)

  pieces <- purrr::map2(
    candidate_paths[present], folder_years[present],
    read_rows_ending_in_year,
    form = form, wanted_year = wanted_year
  )

  selected_rows <- dplyr::bind_rows(pieces)
  attr(selected_rows, "n_unknown_period") <- sum(is.na(selected_rows$tax_period))

  return(selected_rows)
}

#' Check that pre-2012 input carries the ADR 0057 correction
#'
#' @param legacy_table The selected `990combined` rows for a year.
#' @return A character vector of failure messages, empty when the input
#'   is corrected: it has `source_file_year`, and no `ein` plus
#'   `tax_period` (with a known period) appears twice.
#' @details The harmonized rewrite does not deduplicate. It refuses input
#'   that the legacy pipeline has not corrected, so the rule lives in one
#'   place (ADR 0057).
check_legacy_prerequisite <- function(legacy_table) {
  failures <- character(0)

  if (!"source_file_year" %in% names(legacy_table)) {
    failures <- c(failures, "legacy input has no source_file_year: it predates the ADR 0057 correction")
  }

  rows_with_period <- legacy_table[!is.na(legacy_table$tax_period), ]
  n_repeated <- sum(duplicated(paste(rows_with_period$ein, rows_with_period$tax_period)))

  if (n_repeated > 0) {
    failures <- c(failures, sprintf("legacy input repeats ein + tax_period in %d rows: not corrected per ADR 0057", n_repeated))
  }

  return(failures)
}

# ---- Write ----

#' Write one file kind's yearly file as CSV and parquet
#'
#' @param kind_table The file kind's table for the year.
#' @param file_kind The file kind, see `FILE_KIND_FOLDERS`.
#' @param tax_year The period-end year.
#' @param output_root Local folder that mirrors `harmonized/core/`.
#' @return A named character vector with the `csv` and `parquet` paths written.
#' @details File names are exactly the frozen keys being replaced
#'   (`{folder}/marts/CORE-{year}-{KIND}-HRMN-V1.csv`, ADR 0056), so a
#'   mirror of `output_root` lands on the damaged files. The internal
#'   `source_row_id` column is dropped here. The CSV is written with
#'   `data.table::fwrite`, the same writer the frozen files used, with NA
#'   written as empty.
write_file_kind_year <- function(kind_table, file_kind, tax_year, output_root) {
  marts_folder <- file.path(output_root, FILE_KIND_FOLDERS[[file_kind]], "marts")
  dir.create(marts_folder, recursive = TRUE, showWarnings = FALSE)

  file_stem <- file.path(marts_folder, sprintf("CORE-%d-%s-HRMN-V1", tax_year, file_kind))
  csv_path <- paste0(file_stem, ".csv")
  parquet_path <- paste0(file_stem, ".parquet")

  published_table <- dplyr::select(kind_table, -dplyr::all_of(SOURCE_ROW_ID))

  data.table::fwrite(published_table, csv_path, na = "")
  arrow::write_parquet(published_table, parquet_path)

  return(c(csv = csv_path, parquet = parquet_path))
}

# ---- Checks on the written file ----

#' Count values that carry the signature of the 2025 defect
#'
#' @param table One file kind's table.
#' @return A named integer vector: for each numeric column with at least
#'   one such value, the number of values that are not zero but are
#'   smaller in size than `DAMAGED_VALUE_CEILING`. Length zero when the
#'   table is clean.
#' @details The publish step must stop when this returns anything.
count_damaged_values <- function(table) {
  is_double_column <- purrr::map_lgl(table, is.double)
  numeric_columns <- names(table)[is_double_column]

  damaged_counts <- purrr::map_int(table[numeric_columns], count_damaged_in_column)
  damaged_counts <- damaged_counts[damaged_counts > 0]

  return(damaged_counts)
}

#' Count the damaged values in one numeric column
#'
#' @param values A numeric vector.
#' @return How many values are not NA, not zero, and smaller in size than
#'   `DAMAGED_VALUE_CEILING`.
count_damaged_in_column <- function(values) {
  is_damaged <- !is.na(values) & values != 0 & abs(values) < DAMAGED_VALUE_CEILING

  return(sum(is_damaged))
}

#' The CSV reading type for one column
#'
#' @param column A column of the built table.
#' @return "logical", "integer", "numeric" or "character".
csv_type_of <- function(column) {
  if (is.logical(column)) {
    return("logical")
  }

  if (is.integer(column)) {
    return("integer")
  }

  if (is.numeric(column)) {
    return("numeric")
  }

  return("character")
}

#' Say whether one column read back from CSV equals the built column
#'
#' @param built The column as built.
#' @param read_back The same column read back from the CSV.
#' @return TRUE when equal, treating NA text as an empty field.
csv_column_matches <- function(built, read_back) {
  if (is.character(built)) {
    built[is.na(built)] <- ""
    read_back[is.na(read_back)] <- ""
  }

  return(isTRUE(all.equal(built, read_back, check.attributes = FALSE)))
}

#' Compare a written CSV with the table it was written from, every column
#'
#' @param csv_path The CSV file.
#' @param table The table that was written.
#' @return A character vector of failure messages, empty when every
#'   column, text and logical ones included, reads back equal.
#' @details The CSV is read back with each column typed as in `table`, so
#'   a changed identifier or flag is caught, not only a changed number.
compare_csv_to_table <- function(csv_path, table) {
  column_classes <- purrr::map_chr(table, csv_type_of)

  from_csv <- data.table::fread(csv_path, colClasses = column_classes, na.strings = "")
  from_csv <- tibble::as_tibble(from_csv)

  same_shape <- nrow(from_csv) == nrow(table) && identical(names(from_csv), names(table))

  if (!same_shape) {
    return("CSV read-back has different rows or columns")
  }

  column_matches <- purrr::map2_lgl(table, from_csv[names(table)], csv_column_matches)
  differing_columns <- names(table)[!column_matches]

  if (length(differing_columns) > 0) {
    return(paste("CSV read-back differs in", differing_columns[[1]]))
  }

  return(character(0))
}

#' Run the checks on one written file of one file kind
#'
#' @param kind_table The file kind's table as built (still carrying
#'   `source_row_id`).
#' @param file_kind The file kind.
#' @param paths The `csv` and `parquet` paths from `write_file_kind_year()`.
#' @return A named list: `passed` (TRUE or FALSE), `failures` (a character
#'   vector naming each failed check), `row_count` and
#'   `repeated_ein_tax_period`.
#' @details Checks: the header equals the frozen header plus the carried
#'   columns; no damaged values; the parquet and the CSV read back with
#'   the same rows and values. The value-for-value check against the
#'   source is `check_file_kinds_against_source()`.
check_written_file <- function(kind_table, file_kind, paths) {
  failures <- character(0)
  published_table <- dplyr::select(kind_table, -dplyr::all_of(SOURCE_ROW_ID))

  expected_header <- c(read_frozen_columns(file_kind), CARRIED_COLUMNS)

  if (!identical(names(published_table), expected_header)) {
    failures <- c(failures, "header differs from the frozen header plus carried columns")
  }

  if (length(count_damaged_values(published_table)) > 0) {
    failures <- c(failures, "damaged values present")
  }

  from_parquet <- arrow::read_parquet(paths[["parquet"]])
  parquet_matches <- isTRUE(all.equal(as.data.frame(published_table), as.data.frame(from_parquet), check.attributes = FALSE))

  if (!parquet_matches) {
    failures <- c(failures, "parquet read-back differs from the built table")
  }

  failures <- c(failures, compare_csv_to_table(paths[["csv"]], published_table))

  result <- list(
    passed = length(failures) == 0,
    failures = failures,
    row_count = nrow(published_table),
    repeated_ein_tax_period = sum(duplicated(paste(published_table$ein, published_table$tax_period)))
  )

  return(result)
}

# ---- Checks against the source ----

#' Label one source table with its form
#'
#' @param source_table A selected source table.
#' @param form_name Its form: "990", "990ez" or "legacy_pz".
#' @return The table with a `source_form` column.
label_source_form <- function(source_table, form_name) {
  source_table$source_form <- form_name

  return(source_table)
}

#' The source row ids held by a set of file kinds
#'
#' @param kind_tables A named list of file kind tables.
#' @param kind_pattern A pattern on the kind name, "-PZ$" or "-PC$".
#' @return A character vector of source row ids, repeats included.
source_row_ids_of_kinds <- function(kind_tables, kind_pattern) {
  chosen_kinds <- kind_tables[grepl(kind_pattern, names(kind_tables))]
  ids_per_kind <- purrr::map(chosen_kinds, SOURCE_ROW_ID)

  return(unlist(ids_per_kind, use.names = FALSE))
}

#' Check one file kind's values against its source rows
#'
#' @param kind_table The file kind's table.
#' @param file_kind The file kind.
#' @param matched_source The source rows in the same order as
#'   `kind_table`, with `source_form`.
#' @param column_map The name table rows for the year.
#' @return A character vector of failure messages, empty when every
#'   expected column is present and equal to its source column.
#' @details Only old names in this kind's frozen header are checked: a
#'   mapped name outside the header is dropped on purpose by
#'   `conform_to_frozen_header()`.
check_kind_values <- function(kind_table, file_kind, matched_source, column_map) {
  failures <- character(0)
  frozen_columns <- read_frozen_columns(file_kind)

  for (form_name in unique(matched_source$source_form)) {
    rows_of_form <- matched_source$source_form == form_name

    form_map <- column_map_for_table(column_map, form_name, matched_source)
    form_map <- dplyr::filter(form_map, .data$old_name %in% frozen_columns)

    for (row_number in seq_len(nrow(form_map))) {
      old_name <- form_map$old_name[[row_number]]
      current_name <- form_map$current_name[[row_number]]

      if (!old_name %in% names(kind_table)) {
        failures <- c(failures, paste(file_kind, "is missing expected column", old_name))
        next
      }

      built_values <- kind_table[[old_name]][rows_of_form]
      source_values <- matched_source[[current_name]][rows_of_form]

      if (!isTRUE(all.equal(built_values, source_values, check.attributes = FALSE))) {
        failures <- c(failures, paste(file_kind, old_name, "differs from its source column", current_name))
      }
    }
  }

  return(failures)
}

#' Check one file kind's rows: each has a source row, in the right kind
#'
#' @param kind_table The file kind's table.
#' @param file_kind The file kind.
#' @param source_by_id Every source row of the year, with `source_form`.
#' @param column_map The name table rows for the year.
#' @return A character vector of failure messages.
check_one_kind <- function(kind_table, file_kind, source_by_id, column_map) {
  matched_source <- dplyr::left_join(
    dplyr::select(kind_table, dplyr::all_of(SOURCE_ROW_ID)),
    source_by_id,
    by = SOURCE_ROW_ID
  )

  if (any(is.na(matched_source$source_form))) {
    return(paste(file_kind, "holds rows with no source row"))
  }

  failures <- character(0)
  wants_501c3 <- grepl("^501C3", file_kind)

  if (!all(matched_source$is_501c3 == wants_501c3)) {
    failures <- c(failures, paste(file_kind, "holds rows that belong to the other kind"))
  }

  failures <- c(failures, check_kind_values(kind_table, file_kind, matched_source, column_map))

  return(failures)
}

#' Check the final file kind tables against the source rows they came from
#'
#' @param kind_tables The file kind tables as they will be written (still
#'   carrying `source_row_id`).
#' @param source_tables A named list of selected source tables by form
#'   ("990", "990ez" or "legacy_pz"), from `select_source_rows()`.
#' @param column_map The name table rows for the year.
#' @return A character vector of failure messages, empty when all is well.
#' @details Three things are checked on the final tables, not on an
#'   intermediate: (1) every expected old column is present and equals its
#'   source column row for row, matched by `source_row_id`; (2) no kind
#'   holds a row without a source row or a row that belongs to the other
#'   kind; (3) every source row appears exactly once among the PZ kinds,
#'   and every Form 990 source row exactly once among the PC kinds.
check_file_kinds_against_source <- function(kind_tables, source_tables, column_map) {
  source_by_id <- purrr::imap(source_tables, label_source_form)
  source_by_id <- dplyr::bind_rows(source_by_id)

  failures <- purrr::imap(kind_tables, check_one_kind, source_by_id = source_by_id, column_map = column_map)
  failures <- unlist(failures, use.names = FALSE)

  pz_ids <- source_row_ids_of_kinds(kind_tables, "-PZ$")
  pz_holds_every_source_row_once <- setequal(pz_ids, source_by_id[[SOURCE_ROW_ID]]) && !any(duplicated(pz_ids))

  if (!pz_holds_every_source_row_once) {
    failures <- c(failures, "PZ kinds do not hold every source row exactly once")
  }

  has_pc_kinds <- "990" %in% names(source_tables) && any(grepl("-PC$", names(kind_tables)))

  if (has_pc_kinds) {
    pc_ids <- source_row_ids_of_kinds(kind_tables, "-PC$")
    form_990_ids <- source_tables[["990"]][[SOURCE_ROW_ID]]
    pc_holds_every_990_row_once <- setequal(pc_ids, form_990_ids) && !any(duplicated(pc_ids))

    if (!pc_holds_every_990_row_once) {
      failures <- c(failures, "PC kinds do not hold every Form 990 source row exactly once")
    }
  }

  return(failures)
}
