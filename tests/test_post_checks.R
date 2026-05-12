# tests/test_post_checks.R
# Tests for the pure validators in R/quality/post_checks.R.
# Skips the orchestrator run_post_checks() and the analysis helpers
# (financial_summary, subsection_distribution, etc.) — those are reporting
# outputs, not regression-critical.
# Run from repo root:  Rscript tests/test_post_checks.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "utils.R"))
source(here("R", "create_logger.R"))
source(here("R", "quality", "post_checks.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

cat("\n[parse_years_present: single year]\n")
{
  out <- parse_years_present("2015")
  check("returns one integer", identical(out, 2015L))
}

cat("\n[parse_years_present: range]\n")
{
  out <- parse_years_present("2013-2016")
  check("expands inclusive range", identical(out, 2013:2016))
}

cat("\n[parse_years_present: comma-separated mix]\n")
{
  out <- parse_years_present("2012, 2017-2019, 2024")
  check("handles mixed range + scalars",
        identical(sort(out), c(2012L, 2017L, 2018L, 2019L, 2024L)))
}

cat("\n[parse_years_present: empty / NA / NULL → integer(0)]\n")
{
  check("empty string -> integer(0)", identical(parse_years_present(""), integer(0)))
  check("NA -> integer(0)",           identical(parse_years_present(NA), integer(0)))
  check("NULL -> integer(0)",         identical(parse_years_present(NULL), integer(0)))
}

cat("\n[expected_harmonized_cols: filters by extract_year]\n")
{
  xw <- data.table(
    source_var      = c("a", "b", "c"),
    harmonized_name = c("ein", "tax_period", "filed_form_990t_cd"),
    years_present   = c("2013-2024", "2013-2024", "2015-2018")
  )
  in_2015 <- expected_harmonized_cols(xw, 2015L)
  in_2024 <- expected_harmonized_cols(xw, 2024L)
  check("2015 includes filed_form_990t_cd",  "filed_form_990t_cd" %in% in_2015)
  check("2024 excludes filed_form_990t_cd", !("filed_form_990t_cd" %in% in_2024))
  check("ein in both",
        "ein" %in% in_2015 && "ein" %in% in_2024)
}

cat("\n[check_ein_format: passes on well-formed EINs]\n")
{
  dt <- data.table(ein = c("12-3456789", "98-7654321", "00-0000001"))
  res <- check_ein_format(dt)
  check("passed = TRUE",      isTRUE(res$passed))
  check("malformed = 0",      res$malformed == 0L)
  check("null_count = 0",     res$null_count == 0L)
}

cat("\n[check_ein_format: fails on malformed or NA EINs]\n")
{
  dt <- data.table(ein = c("12-3456789", "1234567890", NA, "12-345"))
  res <- check_ein_format(dt)
  check("passed = FALSE",     !isTRUE(res$passed))
  check("malformed = 2",      res$malformed == 2L)
  check("null_count = 1",     res$null_count == 1L)
}

cat("\n[check_ein_format: duplicates reported but don't fail (amendments are legit)]\n")
{
  # Same EIN + different tax_period is a legitimate amendment scenario.
  dt <- data.table(ein = c("12-3456789", "12-3456789", "98-7654321"))
  res <- check_ein_format(dt)
  check("passed = TRUE despite dup",     isTRUE(res$passed))
  check("duplicate_count > 0 reported",  res$duplicate_count >= 1L)
}

cat("\n[check_tax_period: passes well-formed 6-char YYYYMM]\n")
{
  dt <- data.table(tax_period = c("202312", "202206"))
  res <- check_tax_period(dt)
  check("passed = TRUE",                 isTRUE(res$passed))
  check("malformed_count = 0",           res$malformed_count == 0L)
  check("out_of_range_count = 0",        res$out_of_range_count == 0L)
}

cat("\n[check_tax_period: catches malformed format and out-of-range years]\n")
{
  dt <- data.table(tax_period = c("202312", "abc", "180001", "999912"))
  res <- check_tax_period(dt)
  check("passed = FALSE",          !isTRUE(res$passed))
  check("malformed = 1 ('abc')",   res$malformed_count == 1L)
  # 1800 + 9999 are both out-of-range (bounds are [1989, current+1]).
  check("out_of_range >= 1",       res$out_of_range_count >= 1L)
}

cat("\n[check_subsection_cd: passes whitelist values, fails unknown]\n")
{
  dt <- data.table(subsection_cd = c(3L, 4L, 92L))
  res <- check_subsection_cd(dt)
  check("passed on whitelist values", isTRUE(res$passed))

  dt <- data.table(subsection_cd = c(3L, 999L, 4L))
  res <- check_subsection_cd(dt)
  check("fails on unknown code",      !isTRUE(res$passed))
  check("unknown_count = 1",          res$unknown_count == 1L)
}

cat("\n[check_column_types: passes when all identity cols have the right types]\n")
{
  dt <- data.table(
    ein           = "12-3456789",
    tax_period    = "202312",
    tax_year      = 2023L,
    tax_month     = 12L,
    subsection_cd = 3L,
    is_501c3      = TRUE
  )
  res <- check_column_types(dt)
  check("passed = TRUE",         isTRUE(res$passed))
  check("no issues",             length(res$issues) == 0L)
}

cat("\n[check_column_types: catches wrong types]\n")
{
  dt <- data.table(
    ein           = 123456789L,    # wrong: integer
    tax_period    = 202312L,        # wrong: integer
    tax_year      = "2023",         # wrong: character
    tax_month     = 12L,
    subsection_cd = 3L,
    is_501c3      = TRUE
  )
  res <- check_column_types(dt)
  check("passed = FALSE",        !isTRUE(res$passed))
  check("flags ein wrong type",  any(grepl("ein is not character", res$issues)))
  check("flags tax_year wrong type",
        any(grepl("tax_year is not integer", res$issues)))
}

cat("\n[check_column_types: catches NA in is_501c3 (must be strict boolean)]\n")
{
  dt <- data.table(
    ein           = "12-3456789",
    tax_period    = "202312",
    tax_year      = 2023L,
    tax_month     = 12L,
    subsection_cd = 3L,
    is_501c3      = NA   # NA not allowed by design
  )
  res <- check_column_types(dt)
  check("passed = FALSE on NA is_501c3", !isTRUE(res$passed))
  check("error mentions is_501c3 NA",
        any(grepl("is_501c3 contains NA", res$issues)))
}

cat("\n[check_row_count_vs_prior: no baseline → no_baseline status]\n")
{
  res <- check_row_count_vs_prior(1000L, "/nonexistent/path.rds")
  check("status = no_baseline",         res$status == "no_baseline")
  check("delta_pct = NA",               is.na(res$delta_pct))
  check("within_bounds = NA",           is.na(res$within_bounds))
}

cat("\n[check_row_count_vs_prior: within ±20% → ok]\n")
{
  baseline <- tempfile(fileext = ".rds")
  saveRDS(list(row_count = 1000L), baseline)
  # +15% should be ok
  res <- check_row_count_vs_prior(1150L, baseline)
  check("status = ok within 20%",       res$status == "ok")
  check("within_bounds = TRUE",         isTRUE(res$within_bounds))
  # delta_pct is reported as a percentage (15.0 for +15%), not a fraction.
  check("delta_pct ~ 15.0 (percent units)", abs(res$delta_pct - 15) < 1e-6)
  file.remove(baseline)
}

cat("\n[check_row_count_vs_prior: outside ±20% → outside_bounds]\n")
{
  baseline <- tempfile(fileext = ".rds")
  saveRDS(list(row_count = 1000L), baseline)
  # +30% is outside the ±20% band
  res <- check_row_count_vs_prior(1300L, baseline)
  check("status = outside_bounds",      res$status == "outside_bounds")
  check("within_bounds = FALSE",        isFALSE(res$within_bounds))
  file.remove(baseline)
}

cat("\n[check_row_count_vs_prior: prior of 0 → invalid_baseline]\n")
{
  baseline <- tempfile(fileext = ".rds")
  saveRDS(list(row_count = 0L), baseline)
  res <- check_row_count_vs_prior(1000L, baseline)
  check("status = invalid_baseline",    res$status == "invalid_baseline")
  file.remove(baseline)
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
