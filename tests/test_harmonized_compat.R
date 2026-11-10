# tests/test_harmonized_compat.R
# Tests for R/10_harmonized_compat.R: renaming current CORE columns to the
# older "harmonized" names, the family split, and the damaged-value check
# (ADR 0056). Run from repo root:  Rscript tests/test_harmonized_compat.R

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

toy_column_map <- tibble::tribble(
  ~old_name,                 ~source_form, ~current_name,
  "F9_10_ASSET_TOT_EOY",     "990",        "total_assets_eoy",
  "F9_09_EXP_INFO_TECH_TOT", "990",        "information_technology",
  "F9_10_ASSET_TOT_EOY",     "990ez",      "total_assets_eoy",
  "F9_99_NOT_IN_FILE",       "990",        "column_the_file_lacks"
)

toy_990 <- tibble::tibble(
  ein = c("06-0964699", "94-1153307"),
  tax_year = c(2022L, 2022L),
  tax_period = c("202206", "202212"),
  is_amendment = c(FALSE, FALSE),
  extract_year = c(2023L, 2023L),
  is_501c3 = c(TRUE, FALSE),
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
  total_assets_eoy = 2500
)

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
  check("one source per old name and form in 2005",
        !any(duplicated(map_2005[, c("old_name", "source_form")])))
  check("no empty rows returned", !any(map_2005$current_name == ""))
}

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

cat("\n[build_harmonized_families: split by subsection and form]\n")
{
  families <- build_harmonized_families(toy_990, toy_990ez, toy_column_map)
  check("four families", length(families) == 4L)
  frozen_pz_columns <- read_frozen_columns("501C3-CHARITIES-PZ")
  pz_names <- names(families[["501C3-CHARITIES-PZ"]])
  check("frozen column order kept exactly", identical(pz_names[seq_along(frozen_pz_columns)], frozen_pz_columns))
  check("column the source cannot fill is present and empty",
        "F9_10_ASSET_TOT_BOY" %in% pz_names && all(is.na(families[["501C3-CHARITIES-PZ"]]$F9_10_ASSET_TOT_BOY)))
  check("carried columns follow the frozen ones", identical(tail(pz_names, 5), c("ein", "tax_period", "source_form", "is_amendment", "extract_year")))
  check("c3 PC holds the 990 charity only", identical(families[["501C3-CHARITIES-PC"]]$ein, "06-0964699"))
  check("c3 PZ holds 990 and 990-EZ charities", nrow(families[["501C3-CHARITIES-PZ"]]) == 2L)
  check("other-subsection PC holds the c4", identical(families[["501CE-NONPROFIT-PC"]]$ein, "94-1153307"))
  check("990-only column is NA on the 990-EZ row",
        is.na(families[["501C3-CHARITIES-PZ"]]$F9_09_EXP_INFO_TECH_TOT[[2]]))
  check("is_501c3 removed", !("is_501c3" %in% names(families[["501C3-CHARITIES-PZ"]])))
  check("source_form marks each row's form",
        identical(families[["501C3-CHARITIES-PZ"]]$source_form, c("990", "990ez")))
  total_rows <- nrow(families[["501C3-CHARITIES-PZ"]]) + nrow(families[["501CE-NONPROFIT-PZ"]])
  check("PZ families together hold every input row", total_rows == 3L)
}

cat("\n[count_damaged_values: catches the 2025 defect signature]\n")
{
  clean_table <- tibble::tibble(F9_10_ASSET_TOT_EOY = c(0, 100, -5, NA))
  damaged_table <- tibble::tibble(
    F9_10_ASSET_TOT_EOY = c(1.11253692925505e-308, 100),
    F9_09_EXP_TOT_TOT = c(1, 2)
  )
  check("clean table reports nothing", length(count_damaged_values(clean_table)) == 0L)
  damaged_counts <- count_damaged_values(damaged_table)
  check("damaged column named", identical(names(damaged_counts), "F9_10_ASSET_TOT_EOY"))
  check("damaged value counted", damaged_counts[["F9_10_ASSET_TOT_EOY"]] == 1L)
}

cat("\n[find_columns_differing_from_source: catches swapped columns]\n")
{
  renamed <- rename_to_harmonized_format(toy_990, "990", toy_column_map)
  check("faithful rename reports nothing",
        length(find_columns_differing_from_source(toy_990, renamed, "990", toy_column_map)) == 0L)
  swapped <- renamed
  swapped$F9_10_ASSET_TOT_EOY <- renamed$F9_09_EXP_INFO_TECH_TOT
  swapped$F9_09_EXP_INFO_TECH_TOT <- renamed$F9_10_ASSET_TOT_EOY
  differing <- find_columns_differing_from_source(toy_990, swapped, "990", toy_column_map)
  check("both swapped columns reported",
        setequal(differing, c("F9_10_ASSET_TOT_EOY", "F9_09_EXP_INFO_TECH_TOT")))
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
