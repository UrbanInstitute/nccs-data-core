# tests/run_all.R
# Top-level test harness. Sources every tests/test_*.R file, accumulates
# PASS / FAIL counts across them, and exits nonzero if any test failed.
#
# Designed for two invocation styles:
#
#   1. From RStudio or an interactive R terminal:
#        source("tests/run_all.R")
#      Prints per-file output and a combined summary; does not call quit().
#
#   2. From a shell (cron, CI, or local one-shot):
#        Rscript tests/run_all.R
#      Same output; exits with status 1 if any test failed.
#
# Each test_*.R file is harness-aware: it guards its PASS / FAIL counters
# with `if (!exists("PASS")) ...` so a sourced run extends the harness's
# tally rather than resetting it, and skips its own standalone summary +
# quit() when the harness has set TEST_RUN_ALL.

suppressPackageStartupMessages({
  library(here)
})

# Sentinel that tells each test file we're running under the harness.
TEST_RUN_ALL <- TRUE

# Fresh aggregate counters owned by the harness.
PASS <- 0L
FAIL <- 0L

TEST_FILES <- c(
  "test_transforms.R",
  "test_harmonize.R",
  "test_combined.R",
  "test_legacy_merge.R",
  "test_dictionary.R",
  "test_pre_checks.R",
  "test_post_checks.R",
  "test_render.R"
)

t0 <- Sys.time()
for (f in TEST_FILES) {
  path <- here::here("tests", f)
  if (!file.exists(path)) {
    cat(sprintf("\n##### SKIP %s (file not found) #####\n", f))
    next
  }
  cat(sprintf("\n##### %s #####\n", f))
  pass_before <- PASS
  fail_before <- FAIL
  err <- tryCatch(source(path, local = FALSE), error = identity)
  if (inherits(err, "error")) {
    cat(sprintf("  ##### ERROR sourcing %s: %s #####\n", f, conditionMessage(err)))
    FAIL <- FAIL + 1L
  }
  cat(sprintf("  ---- %s: %d passed, %d failed ----\n",
              f, PASS - pass_before, FAIL - fail_before))
}

elapsed <- as.numeric(Sys.time() - t0, units = "secs")
cat(sprintf("\n=== Total: %d passed, %d failed (%.1fs) ===\n", PASS, FAIL, elapsed))

# Only quit() when run via Rscript (interactive sessions shouldn't be killed).
if (!interactive() && FAIL > 0L) {
  quit(status = 1L)
}
