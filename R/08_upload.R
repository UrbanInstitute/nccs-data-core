# R/08_upload.R
# Phase 8: promote harmonized CSVs into the processed/ tier, then sync each
# data tier to S3. Uses `aws s3 sync` via the AWS CLI for native batching,
# concurrency, and retry. Per-tier toggles from CONFIG (R/config.R).

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))

# ---- Step 1: promote harmonized data CSVs into processed/ ----

#' Copy each data/intermediate/harmonized/{tax_year}/{form}/core_*.csv into
#' data/processed/{tax_year}/{form}/. Dictionary + quality HTML already live
#' under data/processed/ from phases 7a/7b.
promote_harmonized_to_processed <- function(harmonized_root = PATHS$harmonized,
                                            processed_root  = PATHS$processed,
                                            logger = NULL) {
  files <- list.files(harmonized_root, pattern = "^core_\\d{4}_[A-Za-z0-9]+\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  n <- 0L
  for (src in files) {
    parts <- strsplit(sub(paste0("^", harmonized_root, "/"), "", src), "/", fixed = TRUE)[[1]]
    if (length(parts) < 3L) next
    tax_year <- parts[1]; form <- parts[2]
    dest_dir <- file.path(processed_root, tax_year, form)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    dest <- file.path(dest_dir, basename(src))
    if (!file.copy(src, dest, overwrite = TRUE)) {
      if (!is.null(logger)) log4r::error(logger, sprintf("FAILED promote %s -> %s", src, dest))
      next
    }
    n <- n + 1L
  }
  if (!is.null(logger)) log4r::info(logger, sprintf("Promoted %d data CSVs to processed/", n))
  invisible(n)
}

# ---- Step 2: per-tier S3 sync ----

s3_uri <- function(prefix, suffix = "") {
  out <- sprintf("s3://%s/%s", S3$bucket, prefix)
  if (nzchar(suffix)) out <- sprintf("%s/%s", out, suffix)
  paste0(sub("/+$", "", out), "/")
}

#' Run `aws s3 sync` with optional dry-run + extra flags. Returns 0 on success.
aws_sync <- function(src, dest, dry_run = FALSE, extra_args = character(), logger = NULL) {
  if (!dir.exists(src)) {
    if (!is.null(logger)) log4r::warn(logger, sprintf("SKIP sync: source %s does not exist", src))
    return(invisible(0L))
  }
  args <- c("s3", "sync", src, dest, extra_args)
  if (dry_run) args <- c(args, "--dryrun")
  if (!is.null(logger)) log4r::info(logger, sprintf("aws %s", paste(args, collapse = " ")))
  status <- system2("aws", args = args, stdout = TRUE, stderr = TRUE)
  attr_status <- attr(status, "status")
  rc <- if (is.null(attr_status)) 0L else as.integer(attr_status)
  if (!is.null(logger)) {
    if (rc == 0L) log4r::info(logger, sprintf("  ok (%d lines)", length(status)))
    else          log4r::error(logger, sprintf("  failed rc=%d: %s",
                                               rc, paste(tail(status, 5), collapse = " | ")))
  }
  invisible(rc)
}

# ---- Top-level orchestrator ----

#' @param dry_run logical: if TRUE, all aws sync calls use --dryrun (no S3 writes).
#' @param run_timestamp character: subfolder for logs uploads, defaults to current ISO time.
#' @param enable_upload logical: master toggle override. Defaults to CONFIG$ENABLE_S3_UPLOAD.
run_upload <- function(dry_run       = FALSE,
                       run_timestamp = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
                       enable_upload = CONFIG$ENABLE_S3_UPLOAD) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "08_upload_log.txt"))

  log4r::info(logger, sprintf("Phase 8 start: dry_run=%s, run_timestamp=%s",
                              dry_run, run_timestamp))

  # ---- Promotion (always runs unless ENABLE_S3_UPLOAD is the only switch) ----
  promote_harmonized_to_processed(logger = logger)

  if (!isTRUE(enable_upload)) {
    log4r::warn(logger, "enable_upload is FALSE; skipping all S3 sync")
    return(invisible(NULL))
  }

  rc <- 0L

  if (isTRUE(CONFIG$ENABLE_UPLOAD_RAW)) {
    rc <- rc + aws_sync(PATHS$soi_extracts, s3_uri(S3$raw_prefix), dry_run, logger = logger)
  } else {
    log4r::info(logger, "skip: ENABLE_UPLOAD_RAW=FALSE")
  }

  if (isTRUE(CONFIG$ENABLE_UPLOAD_INTERMEDIATE)) {
    rc <- rc + aws_sync(PATHS$unpacked,   s3_uri(S3$unpacked_prefix),   dry_run, logger = logger)
    rc <- rc + aws_sync(PATHS$harmonized, s3_uri(S3$harmonized_prefix), dry_run, logger = logger)
  } else {
    log4r::info(logger, "skip: ENABLE_UPLOAD_INTERMEDIATE=FALSE")
  }

  if (isTRUE(CONFIG$ENABLE_UPLOAD_PROCESSED)) {
    rc <- rc + aws_sync(PATHS$processed, s3_uri(S3$processed_prefix), dry_run, logger = logger)
  } else {
    log4r::info(logger, "skip: ENABLE_UPLOAD_PROCESSED=FALSE")
  }

  if (isTRUE(CONFIG$ENABLE_UPLOAD_LOGS)) {
    rc <- rc + aws_sync(PATHS$logs, s3_uri(S3$logs_prefix, run_timestamp), dry_run, logger = logger)
  } else {
    log4r::info(logger, "skip: ENABLE_UPLOAD_LOGS=FALSE")
  }

  log4r::info(logger, sprintf("Phase 8 complete: cumulative rc=%d", rc))
  if (rc != 0L) stop(sprintf("S3 upload had failures (cumulative rc=%d). See data/logs/08_upload_log.txt", rc))
  invisible(NULL)
}

if (sys.nframe() == 0L) run_upload()
