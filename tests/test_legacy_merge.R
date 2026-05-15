# tests/test_legacy_merge.R
# Unit tests for R/04_legacy_merge.R's merge_partition() and helpers.
# Covers tests 1-5 of the 2026-05-14 pause memo's 9-item checklist; tests
# 6-9 are integration-only (real 1999 + 2011 data) and live in a separate
# script not run by the harness.
#
# Run standalone:        Rscript tests/test_legacy_merge.R
# Run via harness:       sourced by tests/run_all.R

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "create_logger.R"))
source(here("R", "04_legacy_merge.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L
check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

# Silent logger for tests.
silent_logger <- function() {
  tmp <- tempfile(fileext = ".log")
  create_logger(tmp)
}

# ---- Test 1: synthetic 4-row merge (1 overlap + 3 unique) ------------------
cat("\n[merge_partition: synthetic overlap + unique rows]\n")
{
  logger <- silent_logger()
  legacy <- data.table(
    ein         = c("11-0000001", "11-0000002"),
    tax_period  = c("201112",     "201112"),
    total_revenue = c("100",      "200"),
    legacy_only_col = c("L1",     "L2")
  )
  soi <- data.table(
    ein         = c("11-0000001", "11-0000003"),
    tax_period  = c("201112",     "201112"),
    total_revenue = c("105",      "300"),
    soi_only_col  = c("S1",       "S3")
  )
  out <- merge_partition(legacy, soi, 2011L, "990combined", logger)
  m <- out$merged

  check("merged row count == 3 (1 overlap + 2 unique)", nrow(m) == 3L)
  check("schema is union of inputs + 2 tag cols",
        setequal(names(m),
                 c("ein", "tax_period", "total_revenue", "legacy_only_col",
                   "soi_only_col", "source_pipeline", "has_legacy_augment")))

  overlap_row <- m[ein == "11-0000001" & tax_period == "201112"]
  check("overlap row exists exactly once", nrow(overlap_row) == 1L)
  check("overlap takes SOI value on shared col (105 not 100)",
        overlap_row$total_revenue == "105")
  check("overlap inherits legacy-only column value",
        overlap_row$legacy_only_col == "L1")
  check("overlap inherits SOI-only column value",
        overlap_row$soi_only_col == "S1")
  check("overlap source_pipeline == soi_current",
        overlap_row$source_pipeline == "soi_current")
  check("overlap has_legacy_augment == TRUE",
        isTRUE(as.logical(overlap_row$has_legacy_augment)))

  legacy_only_row <- m[ein == "11-0000002"]
  check("legacy-only row tagged source_pipeline=legacy",
        legacy_only_row$source_pipeline == "legacy")
  check("legacy-only row has_legacy_augment == FALSE",
        isFALSE(as.logical(legacy_only_row$has_legacy_augment)))
  check("legacy-only row NA on soi-only col",
        is.na(legacy_only_row$soi_only_col))

  soi_only_row <- m[ein == "11-0000003"]
  check("soi-only row tagged source_pipeline=soi_current",
        soi_only_row$source_pipeline == "soi_current")
  check("soi-only row has_legacy_augment == FALSE",
        isFALSE(as.logical(soi_only_row$has_legacy_augment)))
  check("soi-only row NA on legacy-only col",
        is.na(soi_only_row$legacy_only_col))
}

# ---- Test 2: NA defers to legacy on shared col -----------------------------
cat("\n[merge_partition: SOI NA defers to legacy non-NA on shared col]\n")
{
  logger <- silent_logger()
  legacy <- data.table(
    ein = "22-0000001", tax_period = "201112",
    total_revenue = "500", another_shared = "L"
  )
  soi <- data.table(
    ein = "22-0000001", tax_period = "201112",
    total_revenue = NA_character_, another_shared = NA_character_
  )
  out <- merge_partition(legacy, soi, 2011L, "990combined", logger)
  m <- out$merged
  check("overlap row exists", nrow(m) == 1L)
  check("SOI NA defers to legacy value (500)", m$total_revenue == "500")
  check("SOI NA + legacy non-NA -> legacy value", m$another_shared == "L")

  # Both NA case
  legacy2 <- data.table(ein = "22-0000002", tax_period = "201112",
                        total_revenue = NA_character_)
  soi2    <- data.table(ein = "22-0000002", tax_period = "201112",
                        total_revenue = NA_character_)
  out2 <- merge_partition(legacy2, soi2, 2011L, "990combined", logger)
  check("both NA -> output NA", is.na(out2$merged$total_revenue))
}

# ---- Test 3: value disagreement audited, SOI wins in merged output --------
cat("\n[merge_partition: value disagreement -> SOI wins, audit row emitted]\n")
{
  logger <- silent_logger()
  legacy <- data.table(ein = "33-0000001", tax_period = "201112",
                       total_revenue = "1000000")
  soi    <- data.table(ein = "33-0000001", tax_period = "201112",
                       total_revenue = "1050000")
  out <- merge_partition(legacy, soi, 2011L, "990combined", logger)
  check("merged takes SOI value 1050000", out$merged$total_revenue == "1050000")
  check("disagreement has 1 row", nrow(out$disagreements) == 1L)
  check("disagreement.harmonized_name == total_revenue",
        out$disagreements$harmonized_name == "total_revenue")
  check("disagreement.legacy_value == 1000000",
        out$disagreements$legacy_value == "1000000")
  check("disagreement.soi_value == 1050000",
        out$disagreements$soi_value == "1050000")
}

# ---- Test 4: duplicate (ein, tax_period) on either side -------------------
cat("\n[merge_partition: duplicate keys on legacy side -> keep first, log warning]\n")
{
  logger <- silent_logger()
  legacy <- data.table(
    ein         = c("44-0000001", "44-0000001"),
    tax_period  = c("201112",     "201112"),
    total_revenue = c("100", "200")
  )
  soi <- data.table(ein = "44-0000002", tax_period = "201112",
                    total_revenue = "300")
  out <- merge_partition(legacy, soi, 2011L, "990combined", logger)
  m <- out$merged
  check("dedupe drops 1 dup row from legacy", nrow(m) == 2L)
  legacy_row <- m[ein == "44-0000001"]
  check("kept first occurrence (value 100)", legacy_row$total_revenue == "100")
}

# ---- Test 5: pass-through when one side absent ----------------------------
cat("\n[merge_partition: legacy-only pass-through (1999-style)]\n")
{
  logger <- silent_logger()
  legacy <- data.table(ein = c("55-0000001", "55-0000002"),
                       tax_period = c("199912", "199912"),
                       total_revenue = c("100", "200"),
                       legacy_only_col = c("A", "B"))
  out <- merge_partition(legacy, soi_dt = NULL, 1999L, "990combined", logger)
  m <- out$merged
  check("legacy-only pass-through preserves row count", nrow(m) == 2L)
  check("all rows tagged source_pipeline=legacy",
        all(m$source_pipeline == "legacy"))
  check("all rows has_legacy_augment=FALSE",
        all(isFALSE(as.logical(m$has_legacy_augment)) | m$has_legacy_augment == FALSE))
  check("no disagreements in pass-through", nrow(out$disagreements) == 0L)
}

cat("\n[merge_partition: soi-only pass-through (e.g., 2024)]\n")
{
  logger <- silent_logger()
  soi <- data.table(ein = "66-0000001", tax_period = "202412",
                    total_revenue = "500", soi_only_col = "X")
  out <- merge_partition(legacy_dt = NULL, soi, 2024L, "990combined", logger)
  m <- out$merged
  check("soi-only pass-through preserves row count", nrow(m) == 1L)
  check("tagged source_pipeline=soi_current", m$source_pipeline == "soi_current")
  check("has_legacy_augment FALSE",
        isFALSE(as.logical(m$has_legacy_augment)) || m$has_legacy_augment == FALSE)
}

# ---- coalesce_chr helper unit ---------------------------------------------
cat("\n[coalesce_chr: takes primary where non-NA & non-empty]\n")
{
  out <- coalesce_chr(c("a", NA, "", "d"), c("A", "B", "C", "D"))
  check("primary='a' kept",  out[1] == "a")
  check("NA  -> secondary",   out[2] == "B")
  check("''  -> secondary",   out[3] == "C")
  check("primary='d' kept",  out[4] == "d")
}

# ---- Standalone summary (skipped under harness) ---------------------------
if (!isTRUE(exists("TEST_RUN_ALL") && TEST_RUN_ALL)) {
  cat(sprintf("\n=== test_legacy_merge: %d passed, %d failed ===\n", PASS, FAIL))
  if (!interactive() && FAIL > 0L) quit(status = 1L)
}
