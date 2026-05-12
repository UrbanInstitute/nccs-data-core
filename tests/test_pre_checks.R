# tests/test_pre_checks.R
# Tests for run_pre_checks_one() in R/quality/pre_checks.R.
# Use year=1980 (not in any var_matrix) to bypass the column-count tolerance
# check when crafting synthetic source files; expected_col_count() is tested
# separately as its own helper.
# Run from repo root:  Rscript tests/test_pre_checks.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "quality", "pre_checks.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

test_logger <- create_logger(tempfile(pattern = "test_pre_checks_", fileext = ".log"))

#' Write `content` to a tempfile with the given extension; return the path.
mk_src <- function(content, ext = ".csv") {
  path <- tempfile(pattern = "test_src_", fileext = ext)
  writeLines(content, path)
  path
}

cat("\n[run_pre_checks_one: missing file path errors]\n")
{
  res <- run_pre_checks_one(1980, "990",
                            src_path = "/nonexistent/path.csv",
                            logger   = test_logger)
  check("passed = FALSE", !res$passed)
  check("error mentions does not exist",
        any(grepl("does not exist", res$errors, fixed = TRUE)))
}

cat("\n[run_pre_checks_one: empty file fails]\n")
{
  src <- mk_src(character(0))   # 0-byte file
  res <- run_pre_checks_one(1980, "990", src_path = src, logger = test_logger)
  check("passed = FALSE on empty file", !res$passed)
  check("error mentions empty",
        any(grepl("empty", res$errors, ignore.case = TRUE)))
}

cat("\n[run_pre_checks_one: header-only file fails (no data rows)]\n")
{
  src <- mk_src("ein,tax_period,subseccd")
  res <- run_pre_checks_one(1980, "990", src_path = src, logger = test_logger)
  check("passed = FALSE on header-only file", !res$passed)
  check("error mentions no data rows",
        any(grepl("no data rows", res$errors, fixed = TRUE)))
}

cat("\n[run_pre_checks_one: header + 1 data row passes]\n")
{
  src <- mk_src(c("ein,tax_period,subseccd",
                  "12-3456789,202312,3"))
  res <- run_pre_checks_one(1980, "990", src_path = src, logger = test_logger)
  check("passed = TRUE",       res$passed)
  check("n_rows = 1",          res$info$n_rows == 1L)
  check("n_cols = 3",          res$info$n_cols == 3L)
  check("expected_n_cols = NA (year 1980 not in var_matrix)",
        is.na(res$info$expected_n_cols))
}

cat("\n[run_pre_checks_one: duplicate header columns fail]\n")
{
  src <- mk_src(c("ein,tax_period,ein",
                  "12-3456789,202312,98-7654321"))
  res <- run_pre_checks_one(1980, "990", src_path = src, logger = test_logger)
  check("passed = FALSE on duplicate headers", !res$passed)
  check("error mentions duplicates",
        any(grepl("Duplicate header", res$errors, fixed = TRUE)))
  check("error names the duplicate column",
        any(grepl("ein", res$errors, fixed = TRUE)))
}

cat("\n[run_pre_checks_one: trailing empty headers don't count as duplicates]\n")
{
  # Regression test for the documented IRS-2020+-CSV trailing-comma quirk.
  # Headers `ein,tax_period,,,,` produce empty-name fields after the named
  # two; pre-check should treat those as informational, not as duplicate ""
  # entries. R's strsplit drops the final trailing empty by default, so
  # five commas after the named cols → three "" fields, not four.
  src <- mk_src(c("ein,tax_period,,,,",
                  "12-3456789,202312,,,,"))
  res <- run_pre_checks_one(1980, "990", src_path = src, logger = test_logger)
  check("passed = TRUE despite trailing empty cols", res$passed)
  check("n_empty_cols = 3",  res$info$n_empty_cols == 3L)
}

cat("\n[run_pre_checks_one: BOM-prefixed header parses cleanly]\n")
{
  # Some IRS files have a UTF-8 BOM at the very start of the header. The
  # function strips it before splitting fields.
  bom <- "\xef\xbb\xbf"
  src <- mk_src(c(paste0(bom, "ein,tax_period,subseccd"),
                  "12-3456789,202312,3"))
  res <- run_pre_checks_one(1980, "990", src_path = src, logger = test_logger)
  check("passed = TRUE with BOM-prefixed header", res$passed)
  check("BOM stripped: col count = 3 not 4", res$info$n_cols == 3L)
}

cat("\n[expected_col_count: returns NA for unknown year]\n")
{
  # Year 1980 was never published; var_matrix has no column for it.
  n <- expected_col_count(1980, "990")
  check("year not in matrix -> NA", is.na(n))
}

cat("\n[expected_col_count: matches the IRS var_matrix Y-counts]\n")
{
  # 990pf 2022 was the case the pipeline-run footnote-filter fix addressed;
  # expected count should now be 180 (not 181) after the matrix regeneration.
  n_pf_2022 <- expected_col_count(2022, "990pf")
  check("990pf 2022 expects 180 cols", isTRUE(n_pf_2022 == 180L))
  n_990_2024 <- expected_col_count(2024, "990")
  check("990 2024 expects 246 cols", isTRUE(n_990_2024 == 246L))
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
