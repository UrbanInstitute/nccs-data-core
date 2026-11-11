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
source(here("R", "transforms", "repeated_returns.R"))
source(here("R", "transforms", "legacy_period_end.R"))
source(here("R", "create_logger.R"))

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

# ----- additive EIN renderings (ADR 0036) -----
cat("\n[ein renderings]\n")
# Twins of nccs-data-bmf/R/ein.R::ein_to_prefixed / ein_to_ein2.
check("ein_to_prefixed lowercase ein- prefix",
      identical(ein_to_prefixed("38-2787387"), "ein-38-2787387"))
check("ein_to_ein2 uppercase EIN- prefix",
      identical(ein_to_ein2("38-2787387"), "EIN-38-2787387"))
check("ein_to_prefixed preserves NA",  is.na(ein_to_prefixed(NA_character_)))
check("ein_to_ein2 preserves NA",      is.na(ein_to_ein2(NA_character_)))
dt <- data.table(ein = c("123456789", "", NA))
transform_ein(dt)
add_ein_renderings(dt)
check("add_ein_renderings adds both cols",
      all(c("ein_prefixed", "EIN2") %in% names(dt)))
check("ein_prefixed = ein- + canonical ein",
      identical(dt$ein_prefixed, c("ein-12-3456789", NA, NA)))
check("EIN2 = EIN- + canonical ein",
      identical(dt$EIN2, c("EIN-12-3456789", NA, NA)))
check("renderings character-typed",
      is.character(dt$ein_prefixed) && is.character(dt$EIN2))

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
cat("\n[drop_repeated_returns: one row per return, newest NCCS file wins]\n")
{
  dt <- data.table(
    ein              = c("11-1111111", "11-1111111", "22-2222222", "33-3333333", "33-3333333", "44-4444444"),
    tax_period       = c("200506",     "200506",     "200512",     NA,           NA,           "200512"),
    source_file_year = c(2005L,        2006L,        2005L,        2004L,        2005L,        2006L),
    total_revenue    = c(100,          150,          7,            1,            2,            9)
  )
  out <- drop_repeated_returns(copy(dt))
  check("repeated return kept once",
        sum(out$ein == "11-1111111") == 1L)
  check("the row from the newest NCCS file is the one kept",
        out[ein == "11-1111111", total_revenue] == 150)
  check("a return seen once is kept",
        "22-2222222" %in% out$ein && "44-4444444" %in% out$ein)
  check("rows with no tax_period are never treated as repeats",
        sum(out$ein == "33-3333333") == 2L)
  check("dropped count recorded",
        attr(out, "n_dropped") == 1L)
  check("errors without source_file_year",
        tryCatch({ drop_repeated_returns(dt[, !"source_file_year"]); FALSE }, error = function(e) TRUE))
}

cat("\n[legacy period end: rebuilt for the damaged 1999 file, checked on every other file]\n")
{
  period_end_log_path <- tempfile(fileext = ".log")
  period_end_logger <- create_logger(period_end_log_path)
  log_has <- function(text) any(grepl(text, readLines(period_end_log_path), fixed = TRUE))
  damaged_file_name <- "CORE-1999-501C3-CHARITIES-PZ.csv"
  healthy_file_name <- "CORE-1998-501C3-CHARITIES-PZ.csv"

  check("rebuilt period end pads the month",
        identical(rebuild_period_end(c("1999", "1999"), c("6", "12")), c("199906", "199912")))
  check("rebuilt period end accepts the first and last month",
        identical(rebuild_period_end(c("1999", "1999"), c("1", "12")), c("199901", "199912")))
  check("rebuilt period end is blank for a month outside 1 to 12",
        all(is.na(rebuild_period_end(c("1999", "1999", "1999"), c("0", "13", NA)))))
  check("rebuilt period end is blank for a year that is not four digits",
        all(is.na(rebuild_period_end(c(NA, "99", "19990", "199X"), c("6", "6", "6", "6")))))

  # Period end one year behind, blank, and correct; the last row has no
  # usable accounting month.
  damaged_rows <- data.table(
    ein    = c("11-1111111", "22-2222222", "33-3333333", "44-4444444"),
    taxper = c("199806",     NA,           "199912",     "199812"),
    fisyr  = c("1999",       "1999",       "1999",       "1999"),
    accper = c("06",         "12",         "12",         "00")
  )
  check("share confirmed counts a blank period end as not equal",
        share_of_period_ends_confirmed(damaged_rows) == 0.25)

  check("share confirmed is 1 for a table with no rows",
        share_of_period_ends_confirmed(damaged_rows[0]) == 1)
  check("share confirmed is 0 for one row that does not agree",
        share_of_period_ends_confirmed(damaged_rows[1]) == 0)

  rebuilt_rows <- copy(damaged_rows)
  check_or_rebuild_legacy_period_end(rebuilt_rows, damaged_file_name, period_end_logger)
  check("damaged file: period end one year behind is moved to the fiscal year",
        rebuilt_rows[ein == "11-1111111", taxper] == "199906")
  check("damaged file: blank period end is filled",
        rebuilt_rows[ein == "22-2222222", taxper] == "199912")
  check("damaged file: a correct period end is unchanged",
        rebuilt_rows[ein == "33-3333333", taxper] == "199912")
  check("damaged file: unusable accounting month gives a blank period end",
        is.na(rebuilt_rows[ein == "44-4444444", taxper]))
  check("damaged file: the log says how many rows were rebuilt and how many already agreed",
        log_has("period end rebuilt from fiscal year and accounting month on 4 rows (25.0% already agreed; 1 could not be rebuilt"))

  healthy_rows <- data.table(
    ein    = c("11-1111111", "22-2222222"),
    taxper = c("199806",     "199812"),
    fisyr  = c("1998",       "1998"),
    accper = c("06",         "12")
  )
  unchanged_rows <- copy(healthy_rows)
  check_or_rebuild_legacy_period_end(unchanged_rows, healthy_file_name, period_end_logger)
  check("healthy file passes and is left unchanged",
        identical(unchanged_rows, healthy_rows))

  check("another file with the same damage stops the build and reports the share",
        tryCatch({ check_or_rebuild_legacy_period_end(copy(damaged_rows), healthy_file_name, period_end_logger); FALSE },
                 error = function(e) grepl("only 25.0% of rows", conditionMessage(e), fixed = TRUE)))

  # 19 of 20 rows agree: exactly the lowest share that is allowed.
  rows_at_the_limit <- data.table(
    taxper = c(rep("199812", 19), "199712"),
    fisyr  = rep("1998", 20),
    accper = rep("12", 20)
  )
  check("a file exactly at the lowest allowed share passes",
        tryCatch({ check_or_rebuild_legacy_period_end(rows_at_the_limit, healthy_file_name, period_end_logger); TRUE },
                 error = function(e) FALSE))
  check("a file without the accounting month is passed through with a warning",
        identical(check_or_rebuild_legacy_period_end(healthy_rows[, !"accper"], healthy_file_name, period_end_logger),
                  healthy_rows[, !"accper"]))
  check("the log names the column that was missing",
        log_has("CORE-1998-501C3-CHARITIES-PZ.csv: period end not checked, missing column(s): accper"))
  check("the damaged file without the accounting month stops the build",
        tryCatch({ check_or_rebuild_legacy_period_end(damaged_rows[, !"accper"], damaged_file_name, period_end_logger); FALSE },
                 error = function(e) TRUE))
}

if (!exists("TEST_RUN_ALL", envir = globalenv())) {
  cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
  if (FAIL > 0L) quit(status = 1L)
}
