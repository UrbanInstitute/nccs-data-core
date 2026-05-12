# tests/test_transforms.R
# Lightweight unit tests for R/transforms/*.R. No testthat — stopifnot + counters.
# Run from repo root:  Rscript tests/test_transforms.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "transforms", "tax_period.R"))
source(here("R", "transforms", "ein.R"))
source(here("R", "transforms", "subsection.R"))
source(here("R", "transforms", "financial_amounts.R"))
source(here("R", "transforms", "indicators.R"))
source(here("R", "transforms", "efile_indicator.R"))

# Counters are kept module-global so the run_all.R harness can accumulate
# results across files. When this file is run standalone via Rscript the
# counters start fresh; when sourced from the harness they extend whatever
# tally is already in the global env.
if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

# ----- tax_period -----
cat("\n[tax_period]\n")
dt <- data.table(tax_period = c("201012", "202306", "201913", "20191", "abcdef", NA))
transform_tax_period(dt)
check("year parsed",     identical(dt$tax_year,  c(2010L, 2023L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)))
check("month parsed",    identical(dt$tax_month, c(12L,   6L,    NA_integer_, NA_integer_, NA_integer_, NA_integer_)))
check("tax_period char", is.character(dt$tax_period))

# ----- ein -----
cat("\n[ein]\n")
dt <- data.table(ein = c("123456789", "12-3456789", "  300002108  ", "0", "", "abc", NA))
transform_ein(dt)
check("9-digit -> XX-XXXXXXX",  identical(dt$ein[1], "12-3456789"))
check("hyphen normalized",      identical(dt$ein[2], "12-3456789"))
check("trimmed + hyphenated",   identical(dt$ein[3], "30-0002108"))
check("short -> 00-0000000",    identical(dt$ein[4], "00-0000000"))
check("empty -> NA",             is.na(dt$ein[5]))
check("non-numeric -> NA",       is.na(dt$ein[6]))
check("NA in -> NA out",         is.na(dt$ein[7]))

# ----- subsection_cd -----
cat("\n[subsection_cd]\n")
dt <- data.table(subsection_cd = c("03", "3", "4", "92", "99", "", NA))
transform_subsection_cd(dt)
check("3 -> integer 3",            identical(dt$subsection_cd[1], 3L))
check("4 -> integer 4",            identical(dt$subsection_cd[3], 4L))
check("92 known",                  identical(dt$subsection_cd[4], 92L))
check("99 unknown -> NA",          is.na(dt$subsection_cd[5]))
check("is_501c3 TRUE for code 3",  identical(dt$is_501c3[1:2], c(TRUE, TRUE)))
check("is_501c3 FALSE for code 4", identical(dt$is_501c3[3], FALSE))
check("is_501c3 FALSE for NA sub", identical(dt$is_501c3[7], FALSE))

# ----- financial_amounts -----
cat("\n[financial_amounts]\n")
dt <- data.table(rev = c("1000", "$1,234.50", "(500)", "", "bad", NA),
                 exp = c("0", "100", "200", "300", "400", "500"))
transform_financial_amounts(dt, c("rev", "exp"))
check("plain int parsed",   identical(dt$rev[1], 1000))
check("currency stripped",  identical(dt$rev[2], 1234.50))
check("parens -> negative", identical(dt$rev[3], -500))
check("empty -> NA",        is.na(dt$rev[4]))
check("garbage -> NA",      is.na(dt$rev[5]))
check("all-numeric col ok", identical(dt$exp, c(0, 100, 200, 300, 400, 500)))

# ----- indicators -----
cat("\n[indicators]\n")
dt <- data.table(efile_cd = c("Y", "N", "1", "0", "T", "F", "true", "false", "maybe", "", NA))
transform_indicators(dt, "efile_cd")
expected <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA, NA, NA)
check("indicator mapping", identical(dt$efile_cd, expected))

# ----- efile_indicator -----
cat("\n[efile_indicator]\n")
dt <- data.table(efile_indicator = c("E", "P", "e", "p", "Y", "N", "1", "0", "X", "", NA))
transform_efile_indicator(dt)
check("E -> TRUE",        identical(dt$efile_indicator[1], TRUE))
check("P -> FALSE",       identical(dt$efile_indicator[2], FALSE))
check("lowercase e/p ok", identical(dt$efile_indicator[3:4], c(TRUE, FALSE)))
check("Y -> TRUE",        identical(dt$efile_indicator[5], TRUE))
check("N -> FALSE",       identical(dt$efile_indicator[6], FALSE))
check("'1' -> TRUE",      identical(dt$efile_indicator[7], TRUE))
check("'0' -> FALSE",     identical(dt$efile_indicator[8], FALSE))
check("unknown 'X' -> NA",is.na(dt$efile_indicator[9]))
check("empty -> NA",      is.na(dt$efile_indicator[10]))
check("NA in -> NA out",  is.na(dt$efile_indicator[11]))

# ----- boundary: missing-column errors -----
cat("\n[boundary]\n")
dt <- data.table(x = 1:3)
check("financial errors on missing col",
      inherits(tryCatch(transform_financial_amounts(dt, "nope"), error = identity), "error"))
check("indicators errors on missing col",
      inherits(tryCatch(transform_indicators(dt, "nope"), error = identity), "error"))

# Print the standalone summary + exit only when not running under the
# run_all.R harness (which manages its own aggregate output and exit code).
if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
