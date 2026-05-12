# tests/test_combined.R
# Tests for R/04_derive_combined.R's pure helpers. The orchestrator
# run_derive_combined() is integration-only (file I/O) and not unit-tested.
# Run from repo root:  Rscript tests/test_combined.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "04_derive_combined.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

cat("\n[project_to_shared: keeps shared cols, drops the rest]\n")
{
  dt <- data.table(ein = "12-3456789", tax_period = "202312",
                   total_revenue = 100, form990_only_field = "X")
  shared <- c("ein", "tax_period", "total_revenue")
  out <- project_to_shared(copy(dt), shared)
  check("shared cols kept", all(shared %in% names(out)))
  check("non-shared col dropped", !("form990_only_field" %in% names(out)))
  check("row count preserved",     nrow(out) == 1L)
}

cat("\n[project_to_shared: NA-pads cols missing from input]\n")
{
  dt <- data.table(ein = "12-3456789", tax_period = "202312")
  shared <- c("ein", "tax_period", "total_revenue", "total_expenses")
  out <- project_to_shared(copy(dt), shared)
  check("padded total_revenue exists",  "total_revenue"  %in% names(out))
  check("padded total_expenses exists", "total_expenses" %in% names(out))
  check("padded col is NA",             is.na(out$total_revenue[1]))
}

cat("\n[project_to_shared: keeps COMBINED_UNIVERSAL_COLS when present]\n")
{
  dt <- data.table(ein = "12-3456789", tax_period = "202312",
                   tax_year = 2023L, tax_month = 12L,
                   is_501c3 = TRUE, extract_year = 2024L, is_amendment = FALSE)
  shared <- c("ein", "tax_period")
  out <- project_to_shared(copy(dt), shared)
  for (col in c("tax_year", "tax_month", "is_501c3", "extract_year", "is_amendment")) {
    check(sprintf("universal col '%s' preserved", col), col %in% names(out))
  }
}

cat("\n[project_to_shared: doesn't fabricate universal cols if absent]\n")
{
  # Universal cols are preserved if present, but not invented if the input
  # doesn't have them — keeps the function pure.
  dt <- data.table(ein = "12-3456789", tax_period = "202312")
  shared <- c("ein", "tax_period")
  out <- project_to_shared(copy(dt), shared)
  check("tax_year not added when absent",     !("tax_year" %in% names(out)))
  check("extract_year not added when absent", !("extract_year" %in% names(out)))
}

cat("\n[shared_990_990ez_cols: matches the documented 53-col shared schema]\n")
{
  # Integration check against the actual production crosswalks. The repo's
  # README / 04-crosswalks.qmd document a 53-column shared schema between
  # 990 and 990-EZ — drift in either crosswalk should fail this check, which
  # is exactly the regression signal we want.
  shared <- shared_990_990ez_cols()
  check("shared schema is non-empty",   length(shared) > 0L)
  check("shared schema is 53 cols",     length(shared) == 53L)
  check("ein is in shared schema",      "ein" %in% shared)
  check("tax_period is in shared schema","tax_period" %in% shared)
  check("subsection_cd is in shared schema", "subsection_cd" %in% shared)
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
