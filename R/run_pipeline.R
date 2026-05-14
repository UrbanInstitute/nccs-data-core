# R/run_pipeline.R
# Phase 9: orchestrator for the SOI-current CORE pipeline.
# Wires phases 1-8 with per-phase ENABLE_* toggles + CLI overrides.
#
# Usage (from repo root):
#   Rscript R/run_pipeline.R --years 2024 --forms 990ez --no-upload
#   Rscript R/run_pipeline.R --years 2012-2024 --forms 990,990ez,990pf --strict
#   Rscript R/run_pipeline.R --skip-download --skip-unpack       # re-use existing intermediate
#
# All flags are optional. Defaults come from CONFIG in R/config.R.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "01_download.R"))
source(here("R", "02_unpack.R"))
source(here("R", "quality", "pre_checks.R"))
source(here("R", "03_harmonize.R"))
source(here("R", "04_derive_combined.R"))
source(here("R", "05_quality.R"))
source(here("R", "06_dictionary.R"))
source(here("R", "07_render_report.R"))
source(here("R", "09_parquet.R"))
source(here("R", "08_upload.R"))

# Small helper, base R lacks %||% pre-4.4
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- CLI parsing ----

#' Parse e.g. "2012-2024" or "2012,2015,2024" -> integer vector.
parse_year_spec <- function(s) {
  if (is.null(s) || !nzchar(s)) return(integer(0))
  unlist(lapply(strsplit(s, ",", fixed = TRUE)[[1]], function(part) {
    part <- trimws(part)
    if (grepl("-", part, fixed = TRUE)) {
      rng <- as.integer(strsplit(part, "-", fixed = TRUE)[[1]])
      seq.int(rng[1], rng[2])
    } else {
      as.integer(part)
    }
  }))
}

#' Parse e.g. "990,990ez" -> character vector.
parse_form_spec <- function(s) {
  if (is.null(s) || !nzchar(s)) return(character(0))
  trimws(strsplit(s, ",", fixed = TRUE)[[1]])
}

#' Read --key value (or --key=value) pairs from a character vector of args.
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

  yrs <- parse_year_spec(get_arg(args, "--years"))
  if (length(yrs)) override$processing_years <- yrs

  frms <- parse_form_spec(get_arg(args, "--forms"))
  if (length(frms)) override$forms <- frms

  if (has_flag(args, "--no-download"))  CONFIG$ENABLE_DOWNLOAD      <<- FALSE
  if (has_flag(args, "--no-unpack"))    CONFIG$ENABLE_UNPACK        <<- FALSE
  if (has_flag(args, "--no-harmonize")) CONFIG$ENABLE_HARMONIZE     <<- FALSE
  if (has_flag(args, "--no-combined"))  CONFIG$ENABLE_COMBINED      <<- FALSE
  if (has_flag(args, "--no-quality"))   CONFIG$ENABLE_QUALITY       <<- FALSE
  if (has_flag(args, "--no-dictionary"))CONFIG$ENABLE_DICTIONARY    <<- FALSE
  if (has_flag(args, "--no-render"))    CONFIG$ENABLE_RENDER_REPORT <<- FALSE
  if (has_flag(args, "--parquet"))      CONFIG$ENABLE_PARQUET       <<- TRUE
  if (has_flag(args, "--no-parquet"))   CONFIG$ENABLE_PARQUET       <<- FALSE
  if (has_flag(args, "--no-upload"))    CONFIG$ENABLE_S3_UPLOAD     <<- FALSE
  if (has_flag(args, "--upload"))       CONFIG$ENABLE_S3_UPLOAD     <<- TRUE
  if (has_flag(args, "--strict"))       CONFIG$STRICT_QUALITY_GATES <<- TRUE
  if (has_flag(args, "--no-strict"))    CONFIG$STRICT_QUALITY_GATES <<- FALSE

  override$dry_run <- has_flag(args, "--dry-run")

  override
}

# ---- Phase runner with timing ----

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

# ---- Pre-checks bridge (between unpack and harmonize) ----

run_pre_checks_all <- function(processing_years, forms, logger,
                               strict = CONFIG$STRICT_QUALITY_GATES) {
  any_failed <- FALSE
  combos <- expand.grid(py = processing_years, form = forms, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(combos))) {
    src_dir <- file.path(PATHS$unpacked, combos$py[i], combos$form[i])
    if (!dir.exists(src_dir)) next   # nothing unpacked for this combo (e.g., 2017-2019 PF gap)
    res <- run_pre_checks_one(combos$py[i], combos$form[i], logger = logger)
    if (!res$passed) any_failed <- TRUE
  }
  if (any_failed && isTRUE(strict)) {
    stop("Pre-checks failed and STRICT_QUALITY_GATES is TRUE. See log.")
  }
  invisible(!any_failed)
}

# ---- Top-level entry point ----

run_pipeline <- function(processing_years = CONFIG$EARLIEST_YEAR:CONFIG$LATEST_YEAR,
                         forms            = CONFIG$FORMS,
                         dry_run          = FALSE) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "run_pipeline_log.txt"))

  run_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  log4r::info(logger, sprintf("=== run_pipeline start: timestamp=%s years=%s forms=%s ===",
                              run_timestamp,
                              paste(range(processing_years), collapse = "-"),
                              paste(forms, collapse = ",")))
  log4r::info(logger, sprintf("    flags: STRICT_QUALITY_GATES=%s ENABLE_S3_UPLOAD=%s",
                              CONFIG$STRICT_QUALITY_GATES, CONFIG$ENABLE_S3_UPLOAD))

  phase("1 download",  CONFIG$ENABLE_DOWNLOAD,      logger,
        function() run_download(processing_years, forms))
  phase("2 unpack",    CONFIG$ENABLE_UNPACK,        logger,
        function() run_unpack())
  phase("2.5 pre-checks", CONFIG$ENABLE_QUALITY,    logger,
        function() run_pre_checks_all(processing_years, forms, logger,
                                      strict = CONFIG$STRICT_QUALITY_GATES))
  phase("3 harmonize", CONFIG$ENABLE_HARMONIZE,     logger,
        function() run_harmonize(processing_years, forms))
  phase("4 combined",  CONFIG$ENABLE_COMBINED,      logger,
        function() run_derive_combined())
  phase("5 quality",   CONFIG$ENABLE_QUALITY,       logger,
        function() run_quality(strict = CONFIG$STRICT_QUALITY_GATES))
  phase("6 dictionary",CONFIG$ENABLE_DICTIONARY,    logger,
        function() run_dictionary())
  phase("7 render",    CONFIG$ENABLE_RENDER_REPORT, logger,
        function() run_render_reports())
  phase("9 parquet",   CONFIG$ENABLE_PARQUET,       logger,
        function() run_parquet())
  phase("8 upload",    TRUE,                         logger,
        function() run_upload(dry_run = dry_run, run_timestamp = run_timestamp,
                              enable_upload = CONFIG$ENABLE_S3_UPLOAD))

  log4r::info(logger, sprintf("=== run_pipeline complete: timestamp=%s ===", run_timestamp))
  invisible(NULL)
}

# ---- CLI entry ----

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  override <- apply_cli_overrides(args)
  run_pipeline(
    processing_years = override$processing_years %||% (CONFIG$EARLIEST_YEAR:CONFIG$LATEST_YEAR),
    forms            = override$forms            %||% CONFIG$FORMS,
    dry_run          = override$dry_run %||% FALSE
  )
}
