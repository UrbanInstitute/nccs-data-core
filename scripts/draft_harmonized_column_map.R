# scripts/draft_harmonized_column_map.R
#
# Drafts the table that links column names in the older "harmonized" CORE
# format (names such as F9_09_EXP_INFO_TECH_TOT) to column names in the
# current CORE (names such as information_technology). ADR 0056.
#
# How the link is made: both formats were built by renaming the same IRS
# source columns. The old pipeline used XWALK-990-V2.csv. The current
# pipeline uses the *_crosswalk_FINAL tables. Joining the two on the IRS
# source name gives old name -> current name, separately for each source
# form (Form 990, Form 990-EZ, and the pre-2012 legacy files).
#
# Output: data/crosswalks/harmonized_column_map_BASELINE.csv
#   One row per (old name, source form, current name). The status column
#   says whether the link is ready to use:
#     mapped        exactly one current column for this old name and form
#     needs_review  more than one current column; a person must pick one
#     not_carried   no current column in any form
#
# Usage: Rscript scripts/draft_harmonized_column_map.R

# ---- Paths ----

crosswalk_folder <- file.path("data", "crosswalks")

old_format_table_path <- file.path(
  crosswalk_folder, "harmonized_format", "XWALK-990-V2.csv"
)

output_path <- file.path(crosswalk_folder, "harmonized_column_map_BASELINE.csv")

# ---- Functions ----

#' Read one current-CORE crosswalk as source name and current name
#'
#' @param file_name File name of the crosswalk inside `data/crosswalks/`.
#' @param source_name_column Name of the column that holds the IRS (or
#'   legacy NCCS) source column name in that file.
#' @param source_form Label for the form this crosswalk covers: "990",
#'   "990ez" or "legacy_pz".
#' @return A tibble with columns `source_form`, `source_name_upper` and
#'   `current_name`, one row per distinct pair.
#' @details Source names are upper-cased because the old pipeline
#'   upper-cased every source column before renaming it.
read_current_crosswalk <- function(file_name, source_name_column, source_form) {
  crosswalk <- readr::read_csv(
    file.path(crosswalk_folder, file_name),
    col_types = readr::cols(.default = readr::col_character())
  )

  crosswalk <- dplyr::transmute(
    crosswalk,
    source_form = source_form,
    source_name_upper = stringr::str_to_upper(.data[[source_name_column]]),
    current_name = harmonized_name
  )

  crosswalk <- dplyr::filter(crosswalk, !is.na(current_name), current_name != "")

  dplyr::distinct(crosswalk)
}

# ---- Read the old-format table ----

old_format_table <- readr::read_csv(
  old_format_table_path,
  col_types = readr::cols(.default = readr::col_character())
)

old_format_table <- dplyr::transmute(
  old_format_table,
  source_name_upper = stringr::str_to_upper(VAR_NAME_OLD),
  old_name = VAR_NAME_NEW
)

# The old pipeline skipped rows carrying these two placeholder values.
old_format_table <- dplyr::filter(
  old_format_table,
  !old_name %in% c("NOT IN CONCORDANCE", "NOT IN DD")
)

old_format_table <- dplyr::distinct(old_format_table)

# ---- Read the current crosswalks ----

current_crosswalks <- dplyr::bind_rows(
  read_current_crosswalk("soi_990_crosswalk_FINAL.csv", "source_var", "990"),
  read_current_crosswalk("soi_990ez_crosswalk_FINAL.csv", "source_var", "990ez"),
  read_current_crosswalk("legacy_pz_crosswalk_FINAL.csv", "source_column", "legacy_pz")
)

# ---- Link old names to current names through the source name ----

linked <- dplyr::inner_join(
  old_format_table,
  current_crosswalks,
  by = "source_name_upper",
  relationship = "many-to-many"
)

linked <- dplyr::group_by(linked, old_name, source_form)

linked <- dplyr::mutate(
  linked,
  n_current_names = dplyr::n_distinct(current_name),
  status = dplyr::if_else(n_current_names == 1, "mapped", "needs_review")
)

linked <- dplyr::ungroup(linked)

# ---- Old names with no link in any form ----

not_carried <- dplyr::anti_join(
  dplyr::distinct(old_format_table, old_name),
  linked,
  by = "old_name"
)

not_carried <- dplyr::mutate(
  not_carried,
  source_form = NA_character_,
  source_name_upper = NA_character_,
  current_name = NA_character_,
  n_current_names = 0L,
  status = "not_carried"
)

# ---- Write ----

column_map <- dplyr::bind_rows(linked, not_carried)

column_map <- dplyr::select(
  column_map,
  old_name, source_form, current_name, source_name_upper,
  n_current_names, status
)

column_map <- dplyr::arrange(column_map, old_name, source_form, current_name)

readr::write_csv(column_map, output_path, na = "")

# ---- Summary ----

summary_by_form <- dplyr::count(
  dplyr::distinct(column_map, old_name, source_form, status),
  source_form, status
)

print(as.data.frame(summary_by_form))

message("Wrote ", nrow(column_map), " rows to ", output_path)

# ---- Fold in the reviewed overrides and write the FINAL table ----
#
# The FINAL table is what the generator reads. One row per (old name,
# source form, span of years). Status is one of:
#   mapped   copy the current column (after `transform`, blank = unchanged)
#   empty    the column is present in the output and holds no values
#
# Rows marked needs_review in the BASELINE are replaced by the rows in
# harmonized_column_map_OVERRIDES.csv (reviewed by the maintainer).
# Rows marked not_carried become status empty.

overrides_path <- file.path(crosswalk_folder, "harmonized_column_map_OVERRIDES.csv")
final_path <- file.path(crosswalk_folder, "harmonized_column_map_FINAL.csv")

overrides <- readr::read_csv(
  overrides_path,
  col_types = readr::cols(.default = readr::col_character())
)

overrides <- dplyr::transmute(
  overrides,
  old_name, source_form, current_name, years,
  transform = "",
  status,
  note = stringr::str_c(reason, " Rejected: ", rejected)
)

# Years each source form covers in the current CORE.
form_years <- c("990" = "2012-2024", "990ez" = "2012-2024", "legacy_pz" = "1989-2011")

mapped_rows <- dplyr::filter(column_map, status == "mapped")

mapped_rows <- dplyr::transmute(
  mapped_rows,
  old_name, source_form, current_name,
  years = unname(form_years[source_form]),
  transform = "",
  status = "mapped",
  note = ""
)

empty_rows <- dplyr::filter(column_map, status == "not_carried")

empty_rows <- dplyr::transmute(
  empty_rows,
  old_name,
  source_form = "all",
  current_name = "",
  years = "all",
  transform = "",
  status = "empty",
  note = "No column in the current CORE holds this. Present in the file, no values."
)

final_table <- dplyr::bind_rows(mapped_rows, overrides, empty_rows)

# The BASELINE has one row per IRS source name; several source names can
# feed the same (old name, form, current name). One row is enough here.
final_table <- dplyr::distinct(final_table)

final_table <- dplyr::arrange(final_table, old_name, source_form, years)

readr::write_csv(final_table, final_path, na = "")

print(as.data.frame(dplyr::count(final_table, source_form, status)))

message("Wrote ", nrow(final_table), " rows to ", final_path)
