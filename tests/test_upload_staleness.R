# tests/test_upload_staleness.R
# Unit tests for R/08_upload.R::check_dictionary_staleness().
#
# Covers three cases per CSV:
#   - dictionary missing entirely     -> flagged
#   - dictionary present, older       -> flagged
#   - dictionary present, newer/same  -> not flagged
#
# Run standalone:  Rscript tests/test_upload_staleness.R
# Run via harness: sourced by tests/run_all.R

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "create_logger.R"))
source(here("R", "aws_s3_sync.R"))
source(here("R", "08_upload.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

# Build a synthetic processed/ tree with 3 partitions in 3 different states.
make_fixture <- function() {
  root <- tempfile("processed_")
  dir.create(root)
  # Partition 1: csv + fresh dictionary (newer mtime). Should pass.
  d1 <- file.path(root, "2011", "990combined"); dir.create(d1, recursive = TRUE)
  csv1  <- file.path(d1, "core_2011_990combined.csv")
  dict1 <- file.path(d1, "core_2011_990combined_dictionary.csv")
  writeLines("ein", csv1); Sys.sleep(0.05); writeLines("name", dict1)

  # Partition 2: csv only, no dictionary. Should flag "missing".
  d2 <- file.path(root, "2012", "990pf"); dir.create(d2, recursive = TRUE)
  csv2 <- file.path(d2, "core_2012_990pf.csv")
  writeLines("ein", csv2)

  # Partition 3: csv with OLDER dictionary. Should flag "older-than-csv".
  d3 <- file.path(root, "2013", "990ez"); dir.create(d3, recursive = TRUE)
  csv3  <- file.path(d3, "core_2013_990ez.csv")
  dict3 <- file.path(d3, "core_2013_990ez_dictionary.csv")
  writeLines("name", dict3); Sys.sleep(0.05); writeLines("ein", csv3)

  list(root = root, csv1 = csv1, csv2 = csv2, csv3 = csv3)
}

silent_logger <- function() create_logger(tempfile(fileext = ".log"))

cat("\n[check_dictionary_staleness: detects missing + older-than-csv, ignores fresh]\n")
{
  fx <- make_fixture()
  stale <- check_dictionary_staleness(fx$root, logger = silent_logger())
  check("returns 2 stale paths",       length(stale) == 2L)
  check("fresh partition not flagged", !(fx$csv1 %in% stale))
  check("missing-dict partition flagged",  fx$csv2 %in% stale)
  check("older-dict partition flagged",    fx$csv3 %in% stale)
  unlink(fx$root, recursive = TRUE)
}

cat("\n[check_dictionary_staleness: all-fresh tree returns empty]\n")
{
  root <- tempfile("processed_")
  d <- file.path(root, "2020", "990"); dir.create(d, recursive = TRUE)
  csv  <- file.path(d, "core_2020_990.csv")
  dict <- file.path(d, "core_2020_990_dictionary.csv")
  writeLines("ein", csv); Sys.sleep(0.05); writeLines("name", dict)
  stale <- check_dictionary_staleness(root, logger = silent_logger())
  check("no stale partitions returned", length(stale) == 0L)
  unlink(root, recursive = TRUE)
}

cat("\n[check_dictionary_staleness: empty processed_root returns empty]\n")
{
  root <- tempfile("processed_"); dir.create(root)
  stale <- check_dictionary_staleness(root, logger = silent_logger())
  check("empty root -> empty stale", length(stale) == 0L)
  unlink(root, recursive = TRUE)
}

cat("\n[check_dictionary_staleness: dictionary files are NOT treated as data CSVs]\n")
{
  # Regression guard: the dictionary's own *.csv suffix must not get a sibling
  # *_dictionary_dictionary.csv lookup.
  root <- tempfile("processed_")
  d <- file.path(root, "2020", "990"); dir.create(d, recursive = TRUE)
  writeLines("ein",  file.path(d, "core_2020_990.csv"))
  Sys.sleep(0.05)
  writeLines("name", file.path(d, "core_2020_990_dictionary.csv"))
  stale <- check_dictionary_staleness(root, logger = silent_logger())
  check("dictionary not double-counted", length(stale) == 0L)
  unlink(root, recursive = TRUE)
}

if (!isTRUE(exists("TEST_RUN_ALL") && TEST_RUN_ALL)) {
  cat(sprintf("\n=== test_upload_staleness: %d passed, %d failed ===\n", PASS, FAIL))
  if (!interactive() && FAIL > 0L) quit(status = 1L)
}
