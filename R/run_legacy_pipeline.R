# R/run_legacy_pipeline.R
# Orchestrator for the legacy CORE pipeline (1989-2011 PZ + PF).
#
# Mirrors R/run_pipeline.R (SOI-current) in shape: phased structure, ENABLE_*
# config toggles, CLI flag overrides. Differences vs the SOI-current side:
#
#   - Source: s3://nccsdata/legacy/core/ (NCCS-curated CSVs), not IRS SOI.
#   - No phase 4 (derive_combined): 990combined IS the legacy PZ output,
#     not a 990+990ez stack derivation.
#   - No phase 2 (unpack): legacy files are flat CSV, nothing to unzip.
#   - Crosswalks: legacy_pz / legacy_pf, authored against SOI-current
#     vocabulary so the panels stack at the 2011/2012 boundary.
#
# Usage (from repo root):
#   Rscript R/run_legacy_pipeline.R                       # full pipeline, defaults from CONFIG
#   Rscript R/run_legacy_pipeline.R --no-download         # re-use existing local mirror
#   Rscript R/run_legacy_pipeline.R --parquet --no-upload # write parquet locally, skip S3
#
# Phases 2-8 are currently stubs that log "not yet implemented" and skip.
# Phase 9 (parquet) is shared with R/run_pipeline.R via R/09_parquet.R.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "create_logger.R"))
source(here("R", "01_legacy_download.R"))
source(here("R", "03_legacy_harmonize.R"))
source(here("R", "05_quality.R"))
source(here("R", "06_dictionary.R"))
source(here("R", "07_render_report.R"))
source(here("R", "08_upload.R"))
source(here("R", "09_parquet.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- CLI parsing (mirrors run_pipeline.R) ----

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
  override <- list()

  if (has_flag(args, "--no-download"))   CONFIG$ENABLE_DOWNLOAD      <<- FALSE
  if (has_flag(args, "--no-harmonize"))  CONFIG$ENABLE_HARMONIZE     <<- FALSE
  if (has_flag(args, "--no-quality"))    CONFIG$ENABLE_QUALITY       <<- FALSE
  if (has_flag(args, "--no-dictionary")) CONFIG$ENABLE_DICTIONARY    <<- FALSE
  if (has_flag(args, "--no-render"))     CONFIG$ENABLE_RENDER_REPORT <<- FALSE
  if (has_flag(args, "--parquet"))       CONFIG$ENABLE_PARQUET       <<- TRUE
  if (has_flag(args, "--no-parquet"))    CONFIG$ENABLE_PARQUET       <<- FALSE
  if (has_flag(args, "--no-upload"))     CONFIG$ENABLE_S3_UPLOAD     <<- FALSE
  if (has_flag(args, "--upload"))        CONFIG$ENABLE_S3_UPLOAD     <<- TRUE
  if (has_flag(args, "--strict"))        CONFIG$STRICT_QUALITY_GATES <<- TRUE
  if (has_flag(args, "--no-strict"))     CONFIG$STRICT_QUALITY_GATES <<- FALSE

  override$dry_run <- has_flag(args, "--dry-run")
  override
}

# ---- Phase runner (identical to run_pipeline.R) ----

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

# ---- Stubs for phases not yet implemented ----
# Each logs a "not yet implemented" marker and returns NULL. Replace with
# `source()` lines and real `run_*` calls as each phase lands.

stub_phase <- function(phase_name, logger) {
  log4r::warn(logger,
              sprintf("phase '%s' has no implementation yet; treating as no-op", phase_name))
  invisible(NULL)
}

# ---- Top-level entry point ----

run_legacy_pipeline <- function(dry_run = FALSE) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "run_legacy_pipeline_log.txt"))

  run_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  log4r::info(logger, sprintf("=== run_legacy_pipeline start: timestamp=%s ===",
                              run_timestamp))
  log4r::info(logger, sprintf("    flags: STRICT_QUALITY_GATES=%s ENABLE_S3_UPLOAD=%s ENABLE_PARQUET=%s",
                              CONFIG$STRICT_QUALITY_GATES,
                              CONFIG$ENABLE_S3_UPLOAD,
                              CONFIG$ENABLE_PARQUET))

  phase("1 legacy download", CONFIG$ENABLE_DOWNLOAD, logger,
        function() run_legacy_download())
  # Phase 2 (unpack) deliberately omitted: legacy files are flat CSV, no unzip step.
  phase("3 harmonize",       CONFIG$ENABLE_HARMONIZE, logger,
        function() run_legacy_harmonize())
  phase("5 quality",         CONFIG$ENABLE_QUALITY,   logger,
        function() run_quality(harmonized_root = PATHS$harmonized_legacy,
                               forms    = c("990combined", "990pf"),
                               strict   = CONFIG$STRICT_QUALITY_GATES,
                               logs_dir = PATHS$logs_legacy))
  phase("6 dictionary",      CONFIG$ENABLE_DICTIONARY,logger,
        function() run_dictionary(harmonized_root = PATHS$harmonized_legacy,
                                  processed_root  = PATHS$processed_legacy,
                                  forms           = c("990combined", "990pf")))
  phase("7 render",          CONFIG$ENABLE_RENDER_REPORT, logger,
        function() run_render_reports(logs_dir     = PATHS$logs_legacy,
                                      reports_root = PATHS$quality_reports_legacy))
  phase("7.5 promote",       TRUE, logger,
        function() run_promote_legacy())
  phase("9 parquet",         CONFIG$ENABLE_PARQUET,   logger,
        function() run_parquet(processed_root = PATHS$processed_legacy))
  phase("8 upload",          CONFIG$ENABLE_S3_UPLOAD, logger,
        function() run_upload_legacy(run_timestamp = run_timestamp))

  log4r::info(logger, sprintf("=== run_legacy_pipeline complete: timestamp=%s ===",
                              run_timestamp))
  invisible(NULL)
}

# ---- CLI entry ----

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  override <- apply_cli_overrides(args)
  run_legacy_pipeline(dry_run = override$dry_run %||% FALSE)
}
