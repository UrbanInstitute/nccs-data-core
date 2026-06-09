# tests/test_parquet.R
# Unit + end-to-end tests for R/09_parquet.R cross-vintage schema pinning
# (ADR 0027, route A).
#
# Covers:
#   - .schema_group_key collapses vintages of one form, keeps forms/dicts apart
#   - .rank_of_class / .arrow_type_for_rank widening lattice
#   - .pin_family_types widens drifting columns (INT+DOUBLE -> double,
#     numeric/character conflict -> string) and leaves stable columns alone
#   - run_parquet end-to-end: every vintage of a form ends up with an IDENTICAL
#     schema; an all-stable family is left untouched
#   - idempotency: up-to-date conforming parquet is skipped; a stale-typed
#     parquet is rewritten even when newer than its CSV
#
# Run standalone:  Rscript tests/test_parquet.R
# Run via harness: sourced by tests/run_all.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(arrow)
})

source(here("R", "config.R"))
source(here("R", "utils.R"))
source(here("R", "create_logger.R"))
source(here("R", "09_parquet.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

# ---- .schema_group_key -------------------------------------------------------
cat("\n[.schema_group_key: collapses vintages, separates forms + dictionaries]\n")
{
  check("2012/2013 990 collapse to one key",
        .schema_group_key("a/2012/990/core_2012_990.csv") ==
        .schema_group_key("b/2013/990/core_2013_990.csv"))
  check("990 != 990pf",
        .schema_group_key("core_2012_990.csv") !=
        .schema_group_key("core_2012_990pf.csv"))
  check("990 != 990combined",
        .schema_group_key("core_2012_990.csv") !=
        .schema_group_key("core_2012_990combined.csv"))
  check("data file != dictionary sidecar",
        .schema_group_key("core_2012_990.csv") !=
        .schema_group_key("core_2012_990_dictionary.csv"))
}

# ---- widening lattice --------------------------------------------------------
cat("\n[widening lattice: rank order + arrow type mapping]\n")
{
  check("logical < integer < integer64 < numeric < character",
        .rank_of_class("logical") < .rank_of_class("integer") &&
        .rank_of_class("integer") < .rank_of_class("integer64") &&
        .rank_of_class("integer64") < .rank_of_class("numeric") &&
        .rank_of_class("numeric") < .rank_of_class("character"))
  check("double ranks with numeric",
        .rank_of_class("double") == .rank_of_class("numeric"))
  check("unknown class string-widens (top rank)",
        .rank_of_class("Date") == .rank_of_class("character"))
  check("rank 1 -> int32",  .arrow_type_for_rank(1L)$Equals(int32()))
  check("rank 2 -> int64",  .arrow_type_for_rank(2L)$Equals(int64()))
  check("rank 3 -> float64",.arrow_type_for_rank(3L)$Equals(float64()))
  check("rank 4 -> string", .arrow_type_for_rank(4L)$Equals(utf8()))
}

# ---- .pin_family_types -------------------------------------------------------
cat("\n[.pin_family_types: widen drift, ignore stable]\n")
{
  cvs <- list(
    c(a = "integer", b = "integer", c = "numeric"),    # 2012
    c(a = "integer", b = "integer", c = "character"),   # 2013
    c(a = "numeric", b = "integer", c = "numeric")      # 2015
  )
  pinned <- .pin_family_types(cvs)
  check("INT+DOUBLE column pinned to double", pinned$a$Equals(float64()))
  check("stable integer column NOT pinned",   is.null(pinned$b))
  check("numeric/character conflict -> string", pinned$c$Equals(utf8()))
  check("only drifting columns are pinned",   length(pinned) == 2L)
}

# ---- end-to-end run_parquet --------------------------------------------------
cat("\n[run_parquet: every vintage of a form gets an identical pinned schema]\n")
make_tree <- function() {
  root <- tempfile("pq_")
  mk <- function(year, form, dt) {
    d <- file.path(root, as.character(year), form)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    fwrite(dt, file.path(d, sprintf("core_%d_%s.csv", year, form)))
  }
  # 990 family: gross_income_other INT in 2012/2013, DOUBLE (out of INT32
  # range) in 2015; stable_int constant; mixed numeric/character conflict.
  mk(2012, "990", data.table(gross_income_other = c(10L, 20L), stable_int = c(1L, 2L), mixed = c(1.5, 2.5)))
  mk(2013, "990", data.table(gross_income_other = 30L,         stable_int = 3L,        mixed = "N/A"))
  mk(2015, "990", data.table(gross_income_other = 5e9,         stable_int = 4L,        mixed = 9.0))
  # 990pf family: all-stable -> must be untouched.
  mk(2012, "990pf", data.table(invest_inc = 7L))
  mk(2013, "990pf", data.table(invest_inc = 8L))
  root
}
schema_of <- function(p) {
  s <- open_dataset(p, format = "parquet")$schema
  setNames(vapply(seq_len(s$num_fields),
                  function(i) s$field(i - 1)$type$ToString(), character(1L)),
           vapply(seq_len(s$num_fields),
                  function(i) s$field(i - 1)$name, character(1L)))
}
{
  root <- make_tree()
  run_parquet(processed_root = root, overwrite = TRUE, workers = 1L)
  p12 <- file.path(root, "2012", "990", "core_2012_990.parquet")
  p13 <- file.path(root, "2013", "990", "core_2013_990.parquet")
  p15 <- file.path(root, "2015", "990", "core_2015_990.parquet")
  s12 <- schema_of(p12); s13 <- schema_of(p13); s15 <- schema_of(p15)

  check("all three 990 parquets exist", all(file.exists(p12, p13, p15)))
  check("990 schemas identical across vintages (12==13)", identical(s12, s13))
  check("990 schemas identical across vintages (12==15)", identical(s12, s15))
  check("gross_income_other pinned to double", s12[["gross_income_other"]] == "double")
  check("stable_int left as int32",            s12[["stable_int"]] == "int32")
  check("mixed numeric/char widened to string",s12[["mixed"]] == "string")

  # 990pf is its own family: untouched int32.
  pf <- schema_of(file.path(root, "2012", "990pf", "core_2012_990pf.parquet"))
  check("990pf invest_inc stays int32 (independent family)", pf[["invest_inc"]] == "int32")

  # No data lost: the out-of-INT32 value survives as a double.
  v <- as.data.frame(read_parquet(p15))$gross_income_other
  check("out-of-INT32 value preserved (5e9)", isTRUE(all.equal(v, 5e9)))
}

# ---- idempotency + self-healing ----------------------------------------------
cat("\n[run_parquet: skips conforming up-to-date, rewrites stale-typed]\n")
{
  root <- make_tree()
  run_parquet(processed_root = root, overwrite = TRUE, workers = 1L)
  p12 <- file.path(root, "2012", "990", "core_2012_990.parquet")

  # Re-run without overwrite: parquet is newer + conforms -> must be skipped
  # (mtime unchanged).
  mt_before <- file.info(p12)$mtime
  Sys.sleep(1.05)
  run_parquet(processed_root = root, overwrite = FALSE, workers = 1L)
  check("conforming up-to-date parquet is skipped (mtime unchanged)",
        file.info(p12)$mtime == mt_before)

  # Corrupt one vintage to a non-conforming (narrow) type and make it newer
  # than its CSV; self-healing must rewrite it back to the pinned schema.
  bad <- data.table::fread(sub("\\.parquet$", ".csv", p12))
  bad$gross_income_other <- as.integer(bad$gross_income_other)  # int32, violates pin
  arrow::write_parquet(arrow::arrow_table(bad), p12)
  check("hand-written stale parquet is int32 (pre-heal)",
        schema_of(p12)[["gross_income_other"]] == "int32")
  run_parquet(processed_root = root, overwrite = FALSE, workers = 1L)
  check("stale-typed parquet rewritten to pinned double (self-heal)",
        schema_of(p12)[["gross_income_other"]] == "double")
}

# ---- standalone summary (skipped under run_all.R) ----------------------------
if (!exists("TEST_RUN_ALL")) {
  cat(sprintf("\n=== test_parquet: %d passed, %d failed ===\n", PASS, FAIL))
  if (!interactive() && FAIL > 0L) quit(status = 1L)
}
