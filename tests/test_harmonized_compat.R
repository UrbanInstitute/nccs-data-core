# tests/test_harmonized_compat.R
# Tests for R/10_harmonized_compat.R (ADR 0056): the name table by year,
# renaming to the old column names, the split into the four file kinds,
# source selection by period end, the written file, and the checks against
# the source. Run from repo root:  Rscript tests/test_harmonized_compat.R
#
# "File kind" means one of the four kinds of yearly file; see the top of
# R/10_harmonized_compat.R.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "10_harmonized_compat.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

errors <- function(expr) {
  tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
}

# ---- Small tables used by several tests ----

toy_column_map <- tibble::tribble(
  ~old_name,                 ~source_form, ~current_name,            ~transform,
  "F9_10_ASSET_TOT_EOY",     "990",        "total_assets_eoy",       "",
  "F9_09_EXP_INFO_TECH_TOT", "990",        "information_technology", "",
  "F9_10_ASSET_TOT_EOY",     "990ez",      "total_assets_eoy",       "",
  "F9_99_NOT_IN_FILE",       "990",        "column_the_file_lacks",  ""
)

toy_990 <- tibble::tibble(
  ein = c("06-0964699", "94-1153307"),
  tax_year = c(2022L, 2022L),
  tax_period = c("202206", "202212"),
  is_amendment = c(FALSE, FALSE),
  extract_year = c(2023L, 2023L),
  is_501c3 = c(TRUE, FALSE),
  period_end_year = c(2022L, 2022L),
  source_row_id = c("990:2022:1", "990:2022:2"),
  total_assets_eoy = c(5576975, 100),
  information_technology = c(52670, 0),
  column_with_no_old_name = c("a", "b")
)

toy_990ez <- tibble::tibble(
  ein = "11-1111111",
  tax_year = 2022L,
  tax_period = "202212",
  is_amendment = FALSE,
  extract_year = 2023L,
  is_501c3 = TRUE,
  period_end_year = 2022L,
  source_row_id = "990ez:2022:1",
  total_assets_eoy = 2500
)

# ---- A small source mirror on disk: three year folders, two forms ----

#' Write one small source file for a year and form
#'
#' @param root The mirror root.
#' @param year The year folder.
#' @param form "990" or "990ez".
#' @return The path written.
write_toy_source_file <- function(root, year, form) {
  folder <- file.path(root, year, form)
  dir.create(folder, recursive = TRUE)

  rows <- tibble::tibble(
    ein = sprintf("%02d-%07d", year - 2000L, seq_len(3)),
    tax_year = rep(year, 3),
    tax_period = c(sprintf("%d06", year), sprintf("%d12", year), sprintf("%d12", year)),
    is_amendment = FALSE,
    extract_year = year + 1L,
    is_501c3 = c(TRUE, TRUE, FALSE),
    total_assets_eoy = as.numeric(seq_len(3)) * year
  )

  if (form == "990") {
    rows$information_technology <- as.numeric(seq_len(3))
  }

  path <- file.path(folder, sprintf("core_%d_%s.parquet", year, form))
  arrow::write_parquet(rows, path)

  return(path)
}

#' Make a mirror with the years 2021 to 2023 for both forms
#'
#' @return The mirror root.
make_source_mirror <- function() {
  root <- tempfile("mirror_")

  for (year in 2021:2023) {
    write_toy_source_file(root, year, "990")
    write_toy_source_file(root, year, "990ez")
  }

  return(root)
}

#' Select 2022 from a mirror for both forms
#'
#' @param root The mirror root.
#' @return A named list of the two selected tables.
select_2022_sources <- function(root) {
  sources <- list(
    "990" = select_source_rows(root, "990", 2022L),
    "990ez" = select_source_rows(root, "990ez", 2022L)
  )

  return(sources)
}

# ---- Name table ----

cat("\n[year_in_span and read_harmonized_column_map: spans of years]\n")
{
  check("inside a span", year_in_span("1997-2010", 2005))
  check("outside a span", !year_in_span("1997-2010", 2011))
  check("single year", year_in_span("2011", 2011) && !year_in_span("2011", 2010))
  check("all", year_in_span("all", 1989))

  map_2005 <- read_harmonized_column_map(2005)
  map_2011 <- read_harmonized_column_map(2011)

  support_2005 <- map_2005$current_name[map_2005$old_name == "SA_03_TOT_SUPPORT_TOT"]
  support_2011 <- map_2011$current_name[map_2011$old_name == "SA_03_TOT_SUPPORT_TOT"]

  check("Schedule A support uses the combined figure before 2011", identical(support_2005, "total_support_sec170"))
  check("Schedule A support uses the separated line in 2011", identical(support_2011, "total_support_sec509"))
  check("one source per old name and form in 2005", !any(duplicated(map_2005[, c("old_name", "source_form")])))
  check("no empty rows returned", !any(map_2005$current_name == ""))
}

# ---- Rename ----

cat("\n[rename_to_harmonized_format: old names, values unchanged]\n")
{
  renamed <- rename_to_harmonized_format(toy_990, "990", toy_column_map)

  check("old asset name present", "F9_10_ASSET_TOT_EOY" %in% names(renamed))
  check("values carried unchanged", identical(renamed$F9_09_EXP_INFO_TECH_TOT, c(52670, 0)))
  check("EIN2 built from ein", identical(renamed$EIN2, c("EIN-06-0964699", "EIN-94-1153307")))
  check("TAX_YEAR carried", identical(renamed$TAX_YEAR, c(2022L, 2022L)))
  check("column with no old name dropped", !("column_with_no_old_name" %in% names(renamed)))
  check("mapped column absent from file is skipped", !("F9_99_NOT_IN_FILE" %in% names(renamed)))
  check("row count preserved", nrow(renamed) == 2L)
}

cat("\n[rename_to_harmonized_format: TAX_YEAR comes from the period end, not the source tax_year]\n")
{
  shifted <- toy_990
  shifted$tax_year <- c(2021L, 2021L)

  renamed <- rename_to_harmonized_format(shifted, "990", toy_column_map)

  check("TAX_YEAR equals the period-end year", identical(renamed$TAX_YEAR, c(2022L, 2022L)))
}

# ---- Split into the four file kinds ----

cat("\n[build_file_kinds: split by charity status and form; frozen header kept]\n")
{
  kind_tables <- build_file_kinds(toy_990, toy_990ez, toy_column_map)
  charities_pz <- kind_tables[["501C3-CHARITIES-PZ"]]

  check("four file kinds", length(kind_tables) == 4L)
  check("charities PC holds the Form 990 charity only", identical(kind_tables[["501C3-CHARITIES-PC"]]$ein, "06-0964699"))
  check("charities PZ holds the Form 990 and 990-EZ charities", nrow(charities_pz) == 2L)
  check("nonprofit PC holds the 501(c)(4)", identical(kind_tables[["501CE-NONPROFIT-PC"]]$ein, "94-1153307"))
  check("Form 990-only column is NA on the 990-EZ row", is.na(charities_pz$F9_09_EXP_INFO_TECH_TOT[[2]]))
  check("is_501c3 removed", !("is_501c3" %in% names(charities_pz)))
  check("source_form marks each row's form", identical(charities_pz$source_form, c("990", "990ez")))

  frozen_pz_columns <- read_frozen_columns("501C3-CHARITIES-PZ")
  pz_names <- names(charities_pz)

  check("frozen column order kept exactly", identical(pz_names[seq_along(frozen_pz_columns)], frozen_pz_columns))
  check("column the source cannot fill is present and empty", all(is.na(charities_pz$F9_10_ASSET_TOT_BOY)))
  check("carried columns follow the frozen ones, then the internal row id",
        identical(tail(pz_names, 6), c(CARRIED_COLUMNS, "source_row_id")))

  n_pz_rows <- nrow(charities_pz) + nrow(kind_tables[["501CE-NONPROFIT-PZ"]])
  check("the two PZ kinds together hold every input row", n_pz_rows == 3L)
}

# ---- Damaged values ----

cat("\n[count_damaged_values: catches the 2025 defect signature]\n")
{
  clean_table <- tibble::tibble(F9_10_ASSET_TOT_EOY = c(0, 100, -5, NA))

  damaged_table <- tibble::tibble(
    F9_10_ASSET_TOT_EOY = c(1.11253692925505e-308, 100),
    F9_09_EXP_TOT_TOT = c(1, 2)
  )

  damaged_counts <- count_damaged_values(damaged_table)

  check("clean table reports nothing", length(count_damaged_values(clean_table)) == 0L)
  check("damaged column named", identical(names(damaged_counts), "F9_10_ASSET_TOT_EOY"))
  check("damaged value counted", damaged_counts[["F9_10_ASSET_TOT_EOY"]] == 1L)
}

# ---- Source selection ----

cat("\n[select_source_rows: only the requested period-end year, from any folder]\n")
{
  root <- make_source_mirror()
  selected <- select_source_rows(root, "990", 2022L)

  check("every selected row ends in 2022", all(substr(selected$tax_period, 1, 4) == "2022"))
  check("rows from neighbouring years are not selected", nrow(selected) == 3L)
  check("each selected row carries a unique source row id", !any(duplicated(selected$source_row_id)))
  check("period-end year is carried", all(selected$period_end_year == 2022L))

  # A return ending June 2022 that sits in the 2021 folder (starting-year layout) is still found.
  path_2021 <- file.path(root, 2021, "990", "core_2021_990.parquet")
  rows_2021 <- arrow::read_parquet(path_2021)
  rows_2021$tax_period[[1]] <- "202206"
  arrow::write_parquet(rows_2021, path_2021)

  check("a 2022 period end sitting in the 2021 folder is selected", nrow(select_source_rows(root, "990", 2022L)) == 4L)
}

cat("\n[select_source_rows: a missing required input stops the build]\n")
{
  check("nonexistent mirror errors", errors(select_source_rows(tempfile("none_"), "990", 2022L)))

  root <- make_source_mirror()
  unlink(file.path(root, 2022, "990ez"), recursive = TRUE)

  check("missing year folder for a form errors even when neighbours exist", errors(select_source_rows(root, "990ez", 2022L)))
}

cat("\n[select_source_rows: rows with no period are kept by their source year and counted]\n")
{
  root <- make_source_mirror()
  path <- file.path(root, 2022, "990", "core_2022_990.parquet")

  rows <- arrow::read_parquet(path)
  rows$tax_period[[2]] <- NA_character_
  arrow::write_parquet(rows, path)

  selected <- select_source_rows(root, "990", 2022L)

  check("row with no period is kept", nrow(selected) == 3L)
  check("count of no-period rows is reported", attr(selected, "n_unknown_period") == 1L)
}

# ---- Checks against the source ----

cat("\n[check_file_kinds_against_source: a changed final value, a lost row, a misplaced row]\n")
{
  root <- make_source_mirror()
  column_map <- toy_column_map[toy_column_map$old_name != "F9_99_NOT_IN_FILE", ]
  sources <- select_2022_sources(root)
  kind_tables <- build_file_kinds(sources[["990"]], sources[["990ez"]], column_map)

  check("faithful file kinds pass", length(check_file_kinds_against_source(kind_tables, sources, column_map)) == 0L)

  tampered <- kind_tables
  tampered[["501C3-CHARITIES-PZ"]]$F9_10_ASSET_TOT_EOY[[1]] <- 999
  check("a changed value in a final table is reported", length(check_file_kinds_against_source(tampered, sources, column_map)) > 0L)

  dropped <- kind_tables
  dropped[["501C3-CHARITIES-PZ"]] <- dropped[["501C3-CHARITIES-PZ"]][-1, ]
  check("a missing row is reported", length(check_file_kinds_against_source(dropped, sources, column_map)) > 0L)

  misplaced <- kind_tables
  misplaced[["501CE-NONPROFIT-PZ"]] <- dplyr::bind_rows(misplaced[["501CE-NONPROFIT-PZ"]], kind_tables[["501C3-CHARITIES-PZ"]][1, ])
  check("a row in the wrong file kind is reported", length(check_file_kinds_against_source(misplaced, sources, column_map)) > 0L)
}

cat("\n[check_file_kinds_against_source: full committed name table; a mapped column absent from a header]\n")
{
  full_map <- read_harmonized_column_map(2022L)
  root <- tempfile("full_")

  # A source holding every current column the name table knows, for both forms.
  for (form in c("990", "990ez")) {
    folder <- file.path(root, 2022, form)
    dir.create(folder, recursive = TRUE)

    rows <- tibble::tibble(
      ein = c("11-1111111", "22-2222222"),
      tax_year = 2022L,
      tax_period = c("202206", "202212"),
      is_amendment = FALSE,
      extract_year = 2023L,
      is_501c3 = c(TRUE, FALSE)
    )

    current_names <- unique(full_map$current_name[full_map$source_form == form])
    for (column_name in setdiff(current_names, names(rows))) {
      rows[[column_name]] <- c(1, 2)
    }

    arrow::write_parquet(rows, file.path(folder, sprintf("core_2022_%s.parquet", form)))
  }

  sources <- select_2022_sources(root)
  kind_tables <- build_file_kinds(sources[["990"]], sources[["990ez"]], full_map)
  charities_pz_names <- names(kind_tables[["501C3-CHARITIES-PZ"]])

  check("a mapped Form 990 column is absent from the charities PZ header (the case under test)",
        "F9_08_REV_MISC_OTH_EXCL" %in% full_map$old_name && !("F9_08_REV_MISC_OTH_EXCL" %in% charities_pz_names))
  check("faithful build with the full name table passes", length(check_file_kinds_against_source(kind_tables, sources, full_map)) == 0L)

  tampered <- kind_tables
  tampered[["501C3-CHARITIES-PC"]]$F9_10_ASSET_TOT_EOY[[1]] <- 999
  check("a changed value is still caught with the full name table", length(check_file_kinds_against_source(tampered, sources, full_map)) > 0L)

  missing_column <- kind_tables
  missing_column[["501C3-CHARITIES-PC"]]$F9_10_ASSET_TOT_EOY <- NULL
  check("an expected column missing from a file kind is reported", length(check_file_kinds_against_source(missing_column, sources, full_map)) > 0L)
}

# ---- The written file ----

cat("\n[write_file_kind_year and check_written_file: V1 names; every CSV column compared]\n")
{
  root <- make_source_mirror()
  column_map <- toy_column_map[toy_column_map$old_name == "F9_10_ASSET_TOT_EOY", ]
  sources <- select_2022_sources(root)
  kind_tables <- build_file_kinds(sources[["990"]], sources[["990ez"]], column_map)
  charities_pz <- kind_tables[["501C3-CHARITIES-PZ"]]

  output_root <- tempfile("out_")
  paths <- write_file_kind_year(charities_pz, "501C3-CHARITIES-PZ", 2022L, output_root)

  check("file name matches the frozen V1 key", basename(paths[["csv"]]) == "CORE-2022-501C3-CHARITIES-PZ-HRMN-V1.csv")
  check("written files have no source_row_id column", !("source_row_id" %in% names(arrow::read_parquet(paths[["parquet"]]))))
  check("clean file passes", check_written_file(charities_pz, "501C3-CHARITIES-PZ", paths)$passed)

  csv_lines <- readLines(paths[["csv"]])
  csv_lines[[2]] <- sub("EIN-22-0000001", "EIN-99-9999999", csv_lines[[2]])
  writeLines(csv_lines, paths[["csv"]])

  check("a changed text value in the CSV is caught", !check_written_file(charities_pz, "501C3-CHARITIES-PZ", paths)$passed)
}

# ---- Pre-2012 input ----

cat("\n[check_legacy_prerequisite: input must carry the ADR 0057 correction]\n")
{
  legacy <- tibble::tibble(
    ein = c("1", "1", "2"),
    tax_year = 2005L,
    tax_period = c("200512", "200512", "200506"),
    period_end_year = 2005L,
    source_row_id = c("legacy_pz:1", "legacy_pz:2", "legacy_pz:3"),
    source_subsection_class = "501C3-CHARITIES",
    subsection_cd = 3L,
    is_501c3 = TRUE,
    source_file_year = c(2005L, 2006L, 2005L),
    total_assets_eoy = c(1, 2, 3)
  )

  corrected <- legacy[-2, ]
  without_source_year <- corrected[, names(corrected) != "source_file_year"]

  check("repeated return in legacy input is a failure", length(check_legacy_prerequisite(legacy)) > 0L)
  check("corrected legacy input passes", length(check_legacy_prerequisite(corrected)) == 0L)
  check("missing source_file_year is a failure", length(check_legacy_prerequisite(without_source_year)) > 0L)
}

cat("\n[pre-2012 rows: the NCCS file class decides the kind, even when is_501c3 disagrees]\n")
{
  legacy <- tibble::tibble(
    ein = c("1", "2", "3"),
    tax_year = 2005L,
    tax_period = c("200512", "200506", "200512"),
    period_end_year = 2005L,
    source_row_id = c("legacy_pz:2005:1", "legacy_pz:2005:2", "legacy_pz:2005:3"),
    source_subsection_class = c("501C3-CHARITIES", "501CE-NONPROFIT", "501CE-NONPROFIT"),
    subsection_cd = c(3L, 4L, 3L),
    is_501c3 = c(TRUE, FALSE, TRUE),
    source_file_year = 2005L,
    total_assets_eoy = c(1, 2, 3)
  )
  column_map <- tibble::tribble(
    ~old_name,             ~source_form, ~current_name,      ~transform,
    "F9_10_ASSET_TOT_EOY", "legacy_pz",  "total_assets_eoy", ""
  )

  kind_tables <- build_file_kinds_legacy(legacy, column_map)
  sources <- list("legacy_pz" = legacy)

  check("row 3 lands in the nonprofit kind by its NCCS file class", "3" %in% kind_tables[["501CE-NONPROFIT-PZ"]]$ein)
  check("the check agrees with the builder", length(check_file_kinds_against_source(kind_tables, sources, column_map)) == 0L)
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
