# tests/test_harmonize.R
# Tests for apply_crosswalk() in R/03_harmonize.R.
# Same lightweight check() framework as tests/test_transforms.R.
# Run from repo root:  Rscript tests/test_harmonize.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "03_harmonize.R"))

# Test-local PASS/FAIL only initialized if running standalone. The run_all.R
# harness reuses the same names so a child test file inherits the running
# tally.
if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

# A quiet logger that writes to a tempfile so tests don't pollute data/logs/.
test_logger <- create_logger(tempfile(pattern = "test_harmonize_", fileext = ".log"))

#' Build a minimal crosswalk data.table with the columns load_crosswalk() adds.
mk_xwalk <- function(source_var, harmonized_name) {
  x <- data.table(source_var = source_var, harmonized_name = harmonized_name)
  x[, source_var_lc := tolower(source_var)]
  x
}

cat("\n[apply_crosswalk: basic rename]\n")
{
  dt <- data.table(ein = "12-3456789", tax_period = "202312", subseccd = "3")
  x  <- mk_xwalk(c("ein", "tax_period", "subseccd"),
                 c("ein", "tax_period", "subsection_cd"))
  apply_crosswalk(dt, x, test_logger)
  check("source 'subseccd' renamed to 'subsection_cd'", "subsection_cd" %in% names(dt))
  check("ein preserved",         "ein" %in% names(dt))
  check("tax_period preserved",  "tax_period" %in% names(dt))
  check("3 columns total",       ncol(dt) == 3L)
}

cat("\n[apply_crosswalk: drop unmapped source cols]\n")
{
  dt <- data.table(ein = "12-3456789", tax_period = "202312", subseccd = "3",
                   random_col = "junk")
  x  <- mk_xwalk(c("ein", "tax_period", "subseccd"),
                 c("ein", "tax_period", "subsection_cd"))
  apply_crosswalk(dt, x, test_logger)
  check("unmapped col dropped",  !("random_col" %in% names(dt)))
  check("3 columns after drop",  ncol(dt) == 3L)
}

cat("\n[apply_crosswalk: coalesce synonyms (first non-empty wins)]\n")
{
  # Two source vars both map to tax_period — current-vintage and synonym.
  dt <- data.table(
    ein        = c("12-3456789", "98-7654321", "11-1111111"),
    subseccd   = c("3", "3", "3"),
    tax_pd     = c("202312", "",   NA),
    tax_prd    = c("",       "202206", "202112")
  )
  x <- mk_xwalk(
    c("ein", "subseccd",      "tax_pd",     "tax_prd"),
    c("ein", "subsection_cd", "tax_period", "tax_period")
  )
  apply_crosswalk(dt, x, test_logger)
  check("tax_period column present after coalesce", "tax_period" %in% names(dt))
  check("only one tax_period col (synonym collapsed)",
        sum(names(dt) == "tax_period") == 1L)
  check("row 1: tax_pd value wins (non-empty first)",
        identical(dt$tax_period[1], "202312"))
  check("row 2: tax_prd value wins (tax_pd empty)",
        identical(dt$tax_period[2], "202206"))
  check("row 3: tax_prd value wins (tax_pd NA)",
        identical(dt$tax_period[3], "202112"))
}

cat("\n[apply_crosswalk: NA-pad harmonized cols absent from vintage]\n")
{
  # Crosswalk knows about 5 harmonized cols; source has only 3.
  dt <- data.table(ein = "12-3456789", tax_period = "202312", subseccd = "3")
  x <- mk_xwalk(
    c("ein", "tax_period", "subseccd",      "totrev",       "totexp"),
    c("ein", "tax_period", "subsection_cd", "total_revenue", "total_expenses")
  )
  apply_crosswalk(dt, x, test_logger)
  check("padded total_revenue exists",   "total_revenue"  %in% names(dt))
  check("padded total_expenses exists",  "total_expenses" %in% names(dt))
  check("padded col is NA",              is.na(dt$total_revenue[1]))
  check("5 columns total after pad",     ncol(dt) == 5L)
}

cat("\n[apply_crosswalk: missing required identity col errors]\n")
{
  # Crosswalk maps ein but data is missing it entirely — should stop().
  dt <- data.table(tax_period = "202312", subseccd = "3")
  x  <- mk_xwalk(c("ein", "tax_period", "subseccd"),
                 c("ein", "tax_period", "subsection_cd"))
  err <- tryCatch(apply_crosswalk(dt, x, test_logger), error = identity)
  check("errors when ein is missing", inherits(err, "error"))
  check("error message names the missing col",
        is.character(conditionMessage(err)) &&
        grepl("ein", conditionMessage(err), fixed = TRUE))
}

cat("\n[apply_crosswalk: case-insensitive source match]\n")
{
  # Real IRS headers vary in case; read_source() lowercases before this step,
  # but apply_crosswalk's lookup is keyed off source_var_lc so it's robust on
  # its own too.
  dt <- data.table(EIN = "12-3456789", TAX_PERIOD = "202312", SUBSECCD = "3")
  # Simulate post-read_source lowercasing
  setnames(dt, tolower(names(dt)))
  x <- mk_xwalk(
    c("EIN", "TAX_PERIOD", "SUBSECCD"),
    c("ein", "tax_period", "subsection_cd")
  )
  apply_crosswalk(dt, x, test_logger)
  check("uppercase-source xwalk entries match lowercased data",
        all(c("ein", "tax_period", "subsection_cd") %in% names(dt)))
}

# Print the standalone summary + exit only when not running under the
# run_all.R harness (which manages its own aggregate output and exit code).
if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
