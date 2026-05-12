# tests/test_dictionary.R
# Tests for build_dictionary_one() in R/06_dictionary.R.
# Run from repo root:  Rscript tests/test_dictionary.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "utils.R"))
source(here("R", "create_logger.R"))
source(here("R", "06_dictionary.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

#' Write a tiny crosswalk CSV to a tempfile; return the path.
mk_xwalk_csv <- function(rows) {
  path <- tempfile(pattern = "test_xwalk_", fileext = ".csv")
  fwrite(rbindlist(rows), path)
  path
}

cat("\n[build_dictionary_one: per-column stats — character col]\n")
{
  dt <- data.table(ein = c("12-3456789", "98-7654321", "", NA, "00-1111111"))
  xw <- mk_xwalk_csv(list(
    list(source_var = "ein", harmonized_name = "ein",
         description = "Employer Identification Number",
         location = "Header", years_present = "2013-2024")
  ))
  out <- build_dictionary_one(dt, form = "990", tax_year = 2023, xwalk_path = xw)
  ein_row <- out[harmonized_name == "ein"]
  check("dictionary has 1 row",       nrow(out) == 1L)
  check("description copied",         ein_row$description == "Employer Identification Number")
  check("source_var copied",          ein_row$source_var == "ein")
  check("source_location copied",     ein_row$source_location == "Header")
  check("years_present copied",       ein_row$years_present == "2013-2024")
  check("data_type = character",      ein_row$data_type == "character")
  check("n_rows = 5",                 ein_row$n_rows == 5L)
  check("n_nonnull = 3 ('' and NA are blank)", ein_row$n_nonnull == 3L)
  check("null_pct = 40",              ein_row$null_pct == 40)
  check("n_distinct = 3",             ein_row$n_distinct == 3L)
  check("character col has NA min",   is.na(ein_row$min_value))
  check("character col has NA max",   is.na(ein_row$max_value))
}

cat("\n[build_dictionary_one: per-column stats — numeric col]\n")
{
  dt <- data.table(total_revenue = c(100, 200, NA, 50, 300))
  xw <- mk_xwalk_csv(list(
    list(source_var = "totrev", harmonized_name = "total_revenue",
         description = "Total revenue", location = "Pt I-12",
         years_present = "2013-2024")
  ))
  out <- build_dictionary_one(dt, "990", 2023, xw)
  r <- out[harmonized_name == "total_revenue"]
  check("data_type = numeric",        r$data_type == "numeric")
  check("n_nonnull = 4",              r$n_nonnull == 4L)
  check("min_value = 50",             r$min_value == 50)
  check("max_value = 300",            r$max_value == 300)
}

cat("\n[build_dictionary_one: synonyms collapsed into one row]\n")
{
  # Two source vars map to the same harmonized name; the dictionary should
  # show one row with source_var = 'a|b' (pipe-joined).
  dt <- data.table(tax_period = c("202312", "202206", "202112"))
  xw <- mk_xwalk_csv(list(
    list(source_var = "tax_pd",  harmonized_name = "tax_period",
         description = "Tax period", location = "Header", years_present = "2013-2024"),
    list(source_var = "tax_prd", harmonized_name = "tax_period",
         description = "Tax period", location = "Header", years_present = "2012")
  ))
  out <- build_dictionary_one(dt, "990", 2023, xw)
  r <- out[harmonized_name == "tax_period"]
  check("one row per harmonized name", nrow(out[harmonized_name == "tax_period"]) == 1L)
  check("source_var pipe-joined",      grepl("\\|", r$source_var))
  check("source_var contains tax_pd",  grepl("tax_pd", r$source_var))
  check("source_var contains tax_prd", grepl("tax_prd", r$source_var))
}

cat("\n[build_dictionary_one: numeric all-NA collapses Inf min/max to NA]\n")
{
  dt <- data.table(total_revenue = NA_real_)
  xw <- mk_xwalk_csv(list(
    list(source_var = "totrev", harmonized_name = "total_revenue",
         description = "Total revenue", location = "Pt I-12",
         years_present = "2013-2024")
  ))
  out <- build_dictionary_one(dt, "990", 2023, xw)
  r <- out[harmonized_name == "total_revenue"]
  # Without the Inf->NA cleanup, min() of an all-NA numeric returns +Inf.
  check("all-NA numeric: min is NA, not Inf", is.na(r$min_value))
  check("all-NA numeric: max is NA, not -Inf", is.na(r$max_value))
}

cat("\n[build_dictionary_one: universal col description from UNIVERSAL_DICTIONARY_ROWS]\n")
{
  # tax_year is a universal pipeline-derived column; its description comes from
  # UNIVERSAL_DICTIONARY_ROWS() in R/06_dictionary.R, NOT the crosswalk.
  dt <- data.table(ein = "12-3456789", tax_year = 2023L)
  xw <- mk_xwalk_csv(list(
    list(source_var = "ein", harmonized_name = "ein",
         description = "Employer Identification Number",
         location = "Header", years_present = "2013-2024")
  ))
  out <- build_dictionary_one(dt, "990", 2023, xw)
  tx_row <- out[harmonized_name == "tax_year"]
  check("tax_year row present in dict",      nrow(tx_row) == 1L)
  check("tax_year source_location is pipeline-derived",
        tx_row$source_location == "pipeline-derived")
  check("tax_year description is non-NA from UNIVERSAL_DICTIONARY_ROWS",
        !is.na(tx_row$description) && nchar(tx_row$description) > 0L)
}

cat("\n[build_dictionary_one: universal cols not in dt are omitted]\n")
{
  # If a universal col like soi_year isn't present in this dt (e.g., 990 not
  # 990-PF), it shouldn't appear in the dictionary — the function restricts
  # universal rows to those actually present in dt.
  dt <- data.table(ein = "12-3456789")
  xw <- mk_xwalk_csv(list(
    list(source_var = "ein", harmonized_name = "ein",
         description = "EIN", location = "Header", years_present = "2013-2024")
  ))
  out <- build_dictionary_one(dt, "990", 2023, xw)
  check("soi_year omitted when absent from dt", !("soi_year" %in% out$harmonized_name))
  check("source_form omitted when absent from dt", !("source_form" %in% out$harmonized_name))
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
