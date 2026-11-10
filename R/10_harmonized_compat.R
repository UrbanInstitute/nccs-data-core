# R/10_harmonized_compat.R
#
# Rebuilds CORE files in the older "harmonized" format (column names such
# as F9_09_EXP_INFO_TECH_TOT, key EIN2) from the current per-form CORE
# files. ADR 0056.
#
# What this does: reads the current Form 990 and Form 990-EZ files for one
# tax year, renames their columns to the older names using the published
# name table, stacks the two forms, and splits the result into the four
# older file families.
#
# What this does not do: it does not change any value, it does not touch
# the frozen files under harmonized/core/, and it does not publish. The
# values are the current CORE's values under the older names.

# ---- Constants ----

# Each older file family is a filter on subsection and form.
# "PC" files hold full Form 990 filers only. "PZ" files hold Form 990 and
# Form 990-EZ filers together.
HARMONIZED_FAMILIES <- tibble::tribble(
  ~family_token,          ~is_501c3, ~source_forms,
  "501C3-CHARITIES-PC",   TRUE,      list("990"),
  "501C3-CHARITIES-PZ",   TRUE,      list("990", "990ez"),
  "501CE-NONPROFIT-PC",   FALSE,     list("990"),
  "501CE-NONPROFIT-PZ",   FALSE,     list("990", "990ez")
)

# Current-CORE columns carried unchanged beside the older names. One row is
# one filing and amended returns are kept, so EIN2 plus TAX_YEAR is not a
# unique key. These columns let a user tell rows apart. `is_501c3` is used
# for the family split and then removed. The pre-2012 source has no
# `is_amendment` or `extract_year`; those are carried as missing there.
ROW_CONTEXT_COLUMNS <- c("ein", "tax_period", "is_amendment", "extract_year", "is_501c3")

# Folder holding one file per family listing the exact column order of
# the frozen files (columns_{FAMILY}.txt, captured 2026-11-09, identical
# across years within a family).
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
year_in_span <- function(years_text, tax_year) {
  if (years_text == "all") {
    return(TRUE)
  }

  bounds <- as.integer(stringr::str_split_1(years_text, "-"))

  tax_year >= bounds[[1]] && tax_year <= bounds[[length(bounds)]]
}

#' Read the rows of the old-name to current-name table that apply to one year
#'
#' @param tax_year The tax year (period-end year) being built.
#' @param column_map_path Path to the FINAL name table written by
#'   `scripts/draft_harmonized_column_map.R`.
#' @return A tibble with one row per (old name, source form) that is
#'   `mapped` for that year, holding `old_name`, `source_form`,
#'   `current_name` and `transform`.
#' @details Rows with status `empty` are not returned: those columns are
#'   added, with no values, by `conform_to_frozen_columns()`. A row whose
#'   span of years does not include `tax_year` is left out, so an old name
#'   can take its values from different current columns in different
#'   years (the Schedule A columns do).
read_harmonized_column_map <- function(
    tax_year,
    column_map_path = file.path("data", "crosswalks", "harmonized_column_map_FINAL.csv")) {
  column_map <- readr::read_csv(
    column_map_path,
    col_types = readr::cols(.default = readr::col_character())
  )

  column_map <- dplyr::filter(column_map, status == "mapped")

  applies <- vapply(column_map$years, year_in_span, logical(1), tax_year = tax_year)
  column_map <- column_map[applies, ]

  column_map$transform[is.na(column_map$transform)] <- ""

  dplyr::distinct(column_map, old_name, source_form, current_name, transform)
}

# ---- Rename one form ----

#' The name-table rows that apply to one form and one table
#'
#' @param column_map The name table rows for the year.
#' @param source_form "990", "990ez" or "legacy_pz".
#' @param current_table The table being renamed.
#' @return The rows of `column_map` for `source_form` whose current column
#'   exists in `current_table`.
column_map_for_table <- function(column_map, source_form, current_table) {
  wanted_form <- source_form
  dplyr::filter(
    column_map,
    .data$source_form == wanted_form,
    .data$current_name %in% names(current_table)
  )
}

#' Rename one current CORE table to the older column names
#'
#' @param current_table A data frame read from a current per-form CORE
#'   file (for example `core_2022_990.parquet`).
#' @param source_form The form the table holds: "990" or "990ez".
#' @param column_map The name table from `read_harmonized_column_map()`.
#' @return A tibble with the older column names, plus `TAX_YEAR`, `EIN2`,
#'   `source_form` and the columns in `ROW_CONTEXT_COLUMNS`. Columns of
#'   `current_table` that have no older name are dropped.
#' @details When two older names point at the same current column, both
#'   older columns are created and hold the same values. `EIN2` is the
#'   current `ein` (XX-XXXXXXX) with the prefix "EIN-".
rename_to_harmonized_format <- function(current_table, source_form, column_map) {
  form_map <- column_map_for_table(column_map, source_form, current_table)

  renamed_table <- tibble::tibble(.rows = nrow(current_table))

  for (row_number in seq_len(nrow(form_map))) {
    old_name <- form_map$old_name[[row_number]]
    current_name <- form_map$current_name[[row_number]]
    renamed_table[[old_name]] <- current_table[[current_name]]
  }

  renamed_table$TAX_YEAR <- as.integer(current_table$tax_year)
  renamed_table$EIN2 <- stringr::str_c("EIN-", current_table$ein)
  renamed_table$source_form <- source_form

  for (column_name in ROW_CONTEXT_COLUMNS) {
    if (column_name %in% names(current_table)) {
      renamed_table[[column_name]] <- current_table[[column_name]]
    } else {
      renamed_table[[column_name]] <- NA
    }
  }

  renamed_table
}

# ---- Build the families for one year ----

#' Build the four older-format file families for one tax year
#'
#' @param form_990_table The current Form 990 table for the year.
#' @param form_990ez_table The current Form 990-EZ table for the year.
#' @param column_map The name table from `read_harmonized_column_map()`.
#' @return A named list of four tibbles, one per family token in
#'   `HARMONIZED_FAMILIES`.
#' @details The two forms are stacked by column name. A column that only
#'   one form has is missing (NA) on the other form's rows. The split
#'   uses `is_501c3`, which is never missing in the current CORE, so every
#'   source row lands in exactly one charities or nonprofit family.
#'   `is_501c3` is then removed. Each family then takes the exact column
#'   order of the frozen files, see `conform_to_frozen_columns()`.
build_harmonized_families <- function(form_990_table, form_990ez_table, column_map) {
  renamed_990 <- rename_to_harmonized_format(form_990_table, "990", column_map)
  renamed_990ez <- rename_to_harmonized_format(form_990ez_table, "990ez", column_map)

  renamed_by_form <- list("990" = renamed_990, "990ez" = renamed_990ez)

  families <- list()

  for (row_number in seq_len(nrow(HARMONIZED_FAMILIES))) {
    family_token <- HARMONIZED_FAMILIES$family_token[[row_number]]
    source_forms <- unlist(HARMONIZED_FAMILIES$source_forms[[row_number]])
    stacked <- dplyr::bind_rows(renamed_by_form[source_forms])
    families[[family_token]] <- take_family(stacked, family_token, HARMONIZED_FAMILIES$is_501c3[[row_number]])
  }

  families
}

#' Take one family out of a renamed, stacked table
#'
#' @param stacked A renamed table that still has `is_501c3`.
#' @param family_token The family's token.
#' @param wants_501c3 TRUE for a charities family, FALSE for the other
#'   nonprofits.
#' @return The family's rows, without `is_501c3`, in the frozen column order.
take_family <- function(stacked, family_token, wants_501c3) {
  family_table <- dplyr::filter(stacked, .data$is_501c3 == wants_501c3)
  family_table <- dplyr::select(family_table, -is_501c3)
  conform_to_frozen_columns(family_table, family_token)
}

# ---- Frozen column order ----

#' Read the column list of one frozen file family
#'
#' @param family_token A family token from `HARMONIZED_FAMILIES`, for
#'   example "501C3-CHARITIES-PZ".
#' @return A character vector of column names in the frozen files' order.
read_frozen_columns <- function(family_token) {
  readLines(file.path(HARMONIZED_FORMAT_FOLDER, paste0("columns_", family_token, ".txt")))
}

#' Give a family table the exact column set and order of the frozen files
#'
#' @param family_table One family table built by `build_harmonized_families()`.
#' @param family_token The family's token from `HARMONIZED_FAMILIES`.
#' @return The same rows with: every frozen column present, in the frozen
#'   order (a column the source cannot fill is all NA); then the carried
#'   current-CORE columns (`ein`, `tax_period`, `source_form`,
#'   `is_amendment`, `extract_year`) at the end.
#' @details Keeping the old header whole means code written against the
#'   frozen files keeps running. A column the source cannot fill is empty
#'   rather than missing, and the dictionary lists it as such. Any other
#'   column the rename produced that the frozen files did not have is
#'   dropped, so the header never grows beyond the frozen one plus the
#'   carried columns.
conform_to_frozen_columns <- function(family_table, family_token) {
  frozen_columns <- read_frozen_columns(family_token)
  carried_columns <- c("ein", "tax_period", "source_form", "is_amendment", "extract_year")

  for (column_name in setdiff(frozen_columns, names(family_table))) {
    family_table[[column_name]] <- NA_real_
  }

  dplyr::select(family_table, dplyr::all_of(c(frozen_columns, carried_columns)))
}

# ---- Check ----

#' Count values that carry the signature of the 2025 defect
#'
#' @param family_table One older-format table from
#'   `build_harmonized_families()`.
#' @return A named integer vector: for each numeric column with at least
#'   one such value, the number of values that are not zero but are
#'   smaller in size than `DAMAGED_VALUE_CEILING`. Length zero when the
#'   table is clean.
#' @details The publish step must stop when this returns anything.
count_damaged_values <- function(family_table) {
  numeric_columns <- names(family_table)[vapply(family_table, is.double, logical(1))]

  damaged_counts <- integer(0)

  for (column_name in numeric_columns) {
    values <- family_table[[column_name]]
    is_damaged <- !is.na(values) & values != 0 & abs(values) < DAMAGED_VALUE_CEILING
    n_damaged <- sum(is_damaged)

    if (n_damaged > 0) {
      damaged_counts[[column_name]] <- n_damaged
    }
  }

  damaged_counts
}

#' List output columns whose values differ from their source column
#'
#' @param current_table The current per-form CORE table that was renamed.
#' @param renamed_table The result of `rename_to_harmonized_format()` for
#'   that same table, with rows in the same order.
#' @param source_form The form the table holds: "990" or "990ez".
#' @param column_map The name table from `read_harmonized_column_map()`.
#' @return A character vector of older column names whose values are not
#'   identical, row for row, to the current column the name table assigns
#'   them. Length zero when every column agrees.
#' @details This catches two columns being swapped, which row counts and
#'   missing-value counts cannot. The publish step must stop when this
#'   returns anything.
find_columns_differing_from_source <- function(current_table, renamed_table,
                                               source_form, column_map) {
  form_map <- column_map_for_table(column_map, source_form, current_table)

  differing_columns <- character(0)

  for (row_number in seq_len(nrow(form_map))) {
    old_name <- form_map$old_name[[row_number]]
    current_name <- form_map$current_name[[row_number]]

    if (!identical(renamed_table[[old_name]], current_table[[current_name]])) {
      differing_columns <- c(differing_columns, old_name)
    }
  }

  differing_columns
}

# ---- Source rows for one period-end year ----

#' Read the current CORE rows whose accounting period ends in one year
#'
#' @param source_root Local folder mirroring `processed/core/` (2012 on) or
#'   `processed_legacy/core/` (before 2012): `{root}/{year}/{form}/core_{year}_{form}.parquet`.
#' @param form The form folder: "990", "990ez" or "990combined".
#' @param tax_year The period-end year wanted.
#' @return A tibble of the rows whose `tax_period` ends in `tax_year`,
#'   gathered from the `tax_year` and `tax_year + 1` folders when they
#'   exist.
#' @details The folder a row sits in is not trusted for the year. Today
#'   the folders are by period-end year, so only the `tax_year` folder
#'   holds matches. If ADR 0053 is accepted and rows move to folders by
#'   starting year, a return ending June 2023 sits under `2022/`, which is
#'   the `tax_year - 1` folder for 2023. Reading `tax_year - 1`, `tax_year`
#'   and `tax_year + 1` and filtering on `tax_period` covers both layouts.
read_source_rows_for_year <- function(source_root, form, tax_year) {
  candidate_years <- c(tax_year - 1L, tax_year, tax_year + 1L)

  pieces <- list()

  for (folder_year in candidate_years) {
    path <- file.path(
      source_root, folder_year, form,
      sprintf("core_%d_%s.parquet", folder_year, form)
    )

    if (!file.exists(path)) {
      next
    }

    piece <- arrow::read_parquet(path)
    piece$tax_period <- as.character(piece$tax_period)
    piece <- dplyr::filter(piece, substr(.data$tax_period, 1, 4) == as.character(tax_year))

    pieces[[as.character(folder_year)]] <- piece
  }

  if (length(pieces) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(pieces)
}

# ---- Pre-2012 years ----

#' Build the two PZ families for one pre-2012 year
#'
#' @param legacy_table The `processed_legacy/core/{year}/990combined` rows
#'   for the year, from `read_source_rows_for_year()`.
#' @param column_map The name table rows for the year, from
#'   `read_harmonized_column_map()`.
#' @return A named list of two tibbles: `501C3-CHARITIES-PZ` and
#'   `501CE-NONPROFIT-PZ`.
#' @details The split uses `source_subsection_class`, the class the row
#'   carried in the original NCCS file, because about 1% of pre-2012 rows
#'   have no subsection code. Rows of class 501C3-PRIVFOUND are not in
#'   these files. There are no PC families before 2012.
build_harmonized_families_legacy <- function(legacy_table, column_map) {
  legacy_table$is_501c3 <- legacy_table$source_subsection_class == "501C3-CHARITIES"
  legacy_table <- dplyr::filter(legacy_table, .data$source_subsection_class != "501C3-PRIVFOUND")

  renamed <- rename_to_harmonized_format(legacy_table, "legacy_pz", column_map)

  list(
    "501C3-CHARITIES-PZ" = take_family(renamed, "501C3-CHARITIES-PZ", TRUE),
    "501CE-NONPROFIT-PZ" = take_family(renamed, "501CE-NONPROFIT-PZ", FALSE)
  )
}

# ---- Write ----

# Folder each family lives in under harmonized/core/.
FAMILY_FOLDERS <- c(
  "501C3-CHARITIES-PC" = "501c3-pc",
  "501C3-CHARITIES-PZ" = "501c3-pz",
  "501CE-NONPROFIT-PC" = "501ce-pc",
  "501CE-NONPROFIT-PZ" = "501ce-pz"
)

#' Write one family's yearly file as CSV and parquet
#'
#' @param family_table The family table for the year.
#' @param family_token The family's token, see `FAMILY_FOLDERS`.
#' @param tax_year The period-end year.
#' @param output_root Local folder that mirrors `harmonized/core/`.
#' @return A named character vector with the `csv` and `parquet` paths written.
#' @details File names follow the frozen pattern with the version token
#'   V2: `{folder}/marts/CORE-{year}-{FAMILY}-HRMN-V2.csv`. The CSV is
#'   written with `data.table::fwrite`, the same writer the frozen files
#'   used, with NA written as empty.
write_family_year <- function(family_table, family_token, tax_year, output_root) {
  marts_folder <- file.path(output_root, FAMILY_FOLDERS[[family_token]], "marts")
  dir.create(marts_folder, recursive = TRUE, showWarnings = FALSE)

  stem <- file.path(marts_folder, sprintf("CORE-%d-%s-HRMN-V2", tax_year, family_token))
  csv_path <- paste0(stem, ".csv")
  parquet_path <- paste0(stem, ".parquet")

  data.table::fwrite(family_table, csv_path, na = "")
  arrow::write_parquet(family_table, parquet_path)

  c(csv = csv_path, parquet = parquet_path)
}

# ---- Checks before publish ----

#' Run the checks the decision record requires on one family's yearly file
#'
#' @param family_table The family table as built.
#' @param family_token The family's token.
#' @param paths The `csv` and `parquet` paths from `write_family_year()`.
#' @return A named list: `passed` (TRUE or FALSE) and `failures`, a
#'   character vector naming each failed check. Also `row_count` and
#'   `repeated_ein_tax_period` for the manifest.
#' @details Checks: the header equals the frozen header plus the five
#'   carried columns; no damaged values; the CSV and the parquet read back
#'   with the same rows and the same values. The value-for-value check
#'   against the source runs earlier, in `check_renamed_against_source()`,
#'   because it needs the source table.
check_family_file <- function(family_table, family_token, paths) {
  failures <- character(0)

  expected_header <- c(
    read_frozen_columns(family_token),
    "ein", "tax_period", "source_form", "is_amendment", "extract_year"
  )

  if (!identical(names(family_table), expected_header)) {
    failures <- c(failures, "header differs from the frozen header plus carried columns")
  }

  if (length(count_damaged_values(family_table)) > 0) {
    failures <- c(failures, "damaged values present")
  }

  from_parquet <- arrow::read_parquet(paths[["parquet"]])

  if (!isTRUE(all.equal(as.data.frame(family_table), as.data.frame(from_parquet), check.attributes = FALSE))) {
    failures <- c(failures, "parquet read-back differs from the built table")
  }

  from_csv <- data.table::fread(paths[["csv"]], colClasses = list(character = c("EIN2", "ein", "tax_period")))
  from_csv <- tibble::as_tibble(from_csv)

  if (nrow(from_csv) != nrow(family_table) || !identical(names(from_csv), names(family_table))) {
    failures <- c(failures, "CSV read-back has different rows or columns")
  } else {
    numeric_columns <- names(family_table)[vapply(family_table, is.numeric, logical(1))]
    for (column_name in numeric_columns) {
      if (!isTRUE(all.equal(as.numeric(family_table[[column_name]]), as.numeric(from_csv[[column_name]])))) {
        failures <- c(failures, paste("CSV read-back differs in", column_name))
        break
      }
    }
  }

  list(
    passed = length(failures) == 0,
    failures = failures,
    row_count = nrow(family_table),
    repeated_ein_tax_period = sum(duplicated(paste(family_table$ein, family_table$tax_period)))
  )
}

#' Check that every renamed column equals its source, and that the split keeps every row
#'
#' @param source_tables A named list of source tables by form ("990",
#'   "990ez" or "legacy_pz").
#' @param families The family tables built from them.
#' @param column_map The name table rows for the year.
#' @return A character vector of failure messages, empty when all is well.
check_renamed_against_source <- function(source_tables, families, column_map) {
  failures <- character(0)

  for (source_form in names(source_tables)) {
    renamed <- rename_to_harmonized_format(source_tables[[source_form]], source_form, column_map)
    differing <- find_columns_differing_from_source(source_tables[[source_form]], renamed, source_form, column_map)
    if (length(differing) > 0) {
      failures <- c(failures, paste(source_form, "columns differ from source:", paste(differing, collapse = ", ")))
    }
  }

  source_rows <- sum(vapply(source_tables, nrow, integer(1)))
  pz_rows <- sum(vapply(families[grepl("-PZ$", names(families))], nrow, integer(1)))

  if (pz_rows != source_rows) {
    failures <- c(failures, sprintf("PZ families hold %d rows, sources hold %d", pz_rows, source_rows))
  }

  if ("990" %in% names(source_tables)) {
    pc_rows <- sum(vapply(families[grepl("-PC$", names(families))], nrow, integer(1)))
    if (pc_rows != nrow(source_tables[["990"]])) {
      failures <- c(failures, sprintf("PC families hold %d rows, Form 990 source holds %d", pc_rows, nrow(source_tables[["990"]])))
    }
  }

  failures
}
