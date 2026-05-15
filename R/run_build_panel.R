# R/run_build_panel.R
# Builds the merged CORE panel: legacy ∪ SOI-current via Option D column-merge.
#
# Preconditions:
#   - SOI-current pipeline has produced data/intermediate/harmonized/
#     (run R/run_pipeline.R first, at least through phase 4 derive_combined).
#   - Legacy pipeline has produced data/intermediate/harmonized_legacy/
#     (run R/run_legacy_pipeline.R first, at least through phase 3 harmonize).
#
# Phases:
#   4 merge      — column-level merge into data/intermediate/harmonized_merged/
#   5 quality    — run quality checks against the merged tree
#   6 dictionary — build per-(year, form) dictionaries from the merged tree
#                  -> data/processed_merged/
#   8 upload     — (stub) push processed_merged/ to s3://nccsdata/processed_merged/core/
#
# Usage (from repo root):
#   Rscript R/run_build_panel.R
#   Rscript R/run_build_panel.R --no-merge        # re-use existing merged tree
#   Rscript R/run_build_panel.R --no-quality      # skip quality phase
#
# Design rationale: this is a SEPARATE entry point from the legacy and
# SOI-current orchestrators because it depends on outputs from BOTH. Wiring
# it into either as a tail step would force the user to run both pipelines
# in a specific order just to get this phase to fire. Standalone keeps the
# precondition explicit.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "create_logger.R"))
source(here("R", "04_legacy_merge.R"))
source(here("R", "05_quality.R"))
source(here("R", "06_dictionary.R"))
source(here("R", "07_render_report.R"))
source(here("R", "08_upload.R"))
source(here("R", "09_parquet.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- CLI parsing (mirrors run_pipeline.R / run_legacy_pipeline.R) ----

get_arg <- function(args, key, default = NULL) {
  eq_pat <- sprintf("^%s=", key)
  hit_eq <- grep(eq_pat, args)
  if (length(hit_eq)) return(sub(eq_pat, "", args[hit_eq[1]]))
  hit <- which(args == key)
  if (length(hit) && hit[1] + 1L <= length(args)) return(args[hit[1] + 1L])
  default
}
has_flag <- function(args, key) key %in% args

apply_cli_overrides <- function(args) {
  if (has_flag(args, "--no-merge"))      CONFIG$ENABLE_MERGE         <<- FALSE
  if (has_flag(args, "--no-quality"))    CONFIG$ENABLE_QUALITY       <<- FALSE
  if (has_flag(args, "--no-dictionary")) CONFIG$ENABLE_DICTIONARY    <<- FALSE
  if (has_flag(args, "--no-render"))     CONFIG$ENABLE_RENDER_REPORT <<- FALSE
  if (has_flag(args, "--parquet"))       CONFIG$ENABLE_PARQUET       <<- TRUE
  if (has_flag(args, "--no-parquet"))    CONFIG$ENABLE_PARQUET       <<- FALSE
  if (has_flag(args, "--no-upload"))     CONFIG$ENABLE_S3_UPLOAD     <<- FALSE
  if (has_flag(args, "--upload"))        CONFIG$ENABLE_S3_UPLOAD     <<- TRUE
  if (has_flag(args, "--strict"))        CONFIG$STRICT_QUALITY_GATES <<- TRUE
  if (has_flag(args, "--no-strict"))     CONFIG$STRICT_QUALITY_GATES <<- FALSE
  invisible(NULL)
}

phase <- function(name, enabled, logger, fn) {
  if (!isTRUE(enabled)) {
    log4r::info(logger, sprintf("=== SKIP %s (disabled) ===", name))
    return(invisible(NULL))
  }
  log4r::info(logger, sprintf("=== START %s ===", name))
  t0 <- Sys.time()
  result <- tryCatch(fn(), error = function(e) {
    log4r::error(logger, sprintf("=== FAIL %s after %.1fs: %s ===",
                                 name, as.numeric(Sys.time() - t0, units = "secs"),
                                 conditionMessage(e)))
    stop(e)
  })
  log4r::info(logger, sprintf("=== OK %s (%.1fs) ===",
                              name, as.numeric(Sys.time() - t0, units = "secs")))
  invisible(result)
}

check_preconditions <- function(logger) {
  ok_soi    <- dir.exists(PATHS$harmonized)
  ok_legacy <- dir.exists(PATHS$harmonized_legacy)
  if (!ok_soi && !ok_legacy) {
    log4r::error(logger,
                 sprintf("Neither harmonized tree exists: %s, %s",
                         PATHS$harmonized, PATHS$harmonized_legacy))
    stop("Run SOI-current and/or legacy harmonize phases before run_build_panel.")
  }
  if (!ok_soi) {
    log4r::warn(logger,
                sprintf("SOI-current harmonized tree missing (%s); merge will pass legacy through.",
                        PATHS$harmonized))
  }
  if (!ok_legacy) {
    log4r::warn(logger,
                sprintf("Legacy harmonized tree missing (%s); merge will pass SOI-current through.",
                        PATHS$harmonized_legacy))
  }
}

run_build_panel <- function() {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "run_build_panel_log.txt"))

  run_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  log4r::info(logger, sprintf("=== run_build_panel start: timestamp=%s ===",
                              run_timestamp))
  log4r::info(logger, sprintf("    flags: ENABLE_MERGE=%s STRICT_QUALITY_GATES=%s ENABLE_S3_UPLOAD=%s",
                              CONFIG$ENABLE_MERGE,
                              CONFIG$STRICT_QUALITY_GATES,
                              CONFIG$ENABLE_S3_UPLOAD))

  check_preconditions(logger)

  phase("4 legacy merge", CONFIG$ENABLE_MERGE, logger,
        function() run_legacy_merge(legacy_root = PATHS$harmonized_legacy,
                                    soi_root    = PATHS$harmonized,
                                    dest_root   = PATHS$harmonized_merged))

  phase("5 quality (merged)", CONFIG$ENABLE_QUALITY, logger,
        function() run_quality(harmonized_root = PATHS$harmonized_merged,
                               forms    = c("990combined", "990pf"),
                               strict   = CONFIG$STRICT_QUALITY_GATES,
                               logs_dir = PATHS$logs_merged))

  phase("6 dictionary (merged)", CONFIG$ENABLE_DICTIONARY, logger,
        function() run_dictionary(harmonized_root = PATHS$harmonized_merged,
                                  processed_root  = PATHS$processed_merged,
                                  forms           = c("990combined", "990pf")))

  phase("7 render (merged)", CONFIG$ENABLE_RENDER_REPORT, logger,
        function() run_render_reports(logs_dir     = PATHS$logs_merged,
                                      reports_root = PATHS$quality_reports_merged))

  phase("9 parquet (merged)", CONFIG$ENABLE_PARQUET, logger,
        function() run_parquet(processed_root = PATHS$processed_merged))

  phase("8 upload (merged)", CONFIG$ENABLE_S3_UPLOAD, logger,
        function() run_upload_merged(run_timestamp = run_timestamp))

  log4r::info(logger, sprintf("=== run_build_panel complete: timestamp=%s ===",
                              run_timestamp))
  invisible(NULL)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  apply_cli_overrides(args)
  run_build_panel()
}
