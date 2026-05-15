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
source(here("R", "aws_s3_sync.R"))

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

#' Compress every *.html under `src_root` into a mirror directory tree under
#' `tmp_root`. The mirrored files keep their original names (`.html`, not
#' `.html.gz`) but contain gzipped bytes; uploaded with `Content-Encoding:
#' gzip`, browsers decompress them transparently on load.
#'
#' @return integer count of files compressed.
gzip_htmls_to_mirror <- function(src_root, tmp_root, logger = NULL) {
  htmls <- list.files(src_root, pattern = "\\.html$", recursive = TRUE,
                      full.names = TRUE)
  src_norm <- normalizePath(src_root, mustWork = TRUE)
  for (src in htmls) {
    rel  <- substring(normalizePath(src, mustWork = TRUE),
                      nchar(src_norm) + 2L)
    dest <- file.path(tmp_root, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    raw <- readBin(src, what = "raw", n = file.info(src)$size)
    writeBin(memCompress(raw, type = "gzip"), dest)
  }
  if (!is.null(logger)) {
    log4r::info(logger, sprintf("gzipped %d HTML files to %s", length(htmls), tmp_root))
  }
  invisible(length(htmls))
}

#' Sync the processed/ tier to S3, optionally splitting *.html into a gzipped
#' pass with Content-Encoding: gzip. When gzip is disabled, behaves like a
#' single uncompressed sync.
sync_processed_tier <- function(processed_root, s3_dest, dry_run = FALSE,
                                gzip_html = TRUE, logger = NULL) {
  if (!isTRUE(gzip_html)) {
    return(aws_sync(processed_root, s3_dest, dry_run, logger = logger))
  }

  # Pass 1: everything except *.html, uncompressed.
  rc1 <- aws_sync(processed_root, s3_dest, dry_run,
                  extra_args = c("--exclude", "*.html"),
                  logger     = logger)

  # Pass 2: gzipped *.html mirror, uploaded with Content-Encoding: gzip.
  tmp <- tempfile("processed_gz_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  gzip_htmls_to_mirror(processed_root, tmp, logger = logger)
  rc2 <- aws_sync(tmp, s3_dest, dry_run,
                  extra_args = c("--exclude", "*",
                                 "--include", "*.html",
                                 "--content-encoding", "gzip",
                                 "--content-type", "text/html",
                                 "--metadata-directive", "REPLACE"),
                  logger     = logger)

  invisible(rc1 + rc2)
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
    rc <- rc + aws_sync(PATHS$soi_extracts,     s3_uri(S3$raw_prefix),          dry_run, logger = logger)
    rc <- rc + aws_sync(PATHS$soi_dictionaries, s3_uri(S3$dictionaries_prefix), dry_run, logger = logger)
  } else {
    log4r::info(logger, "skip: ENABLE_UPLOAD_RAW=FALSE")
  }

  if (isTRUE(CONFIG$ENABLE_UPLOAD_FORMS)) {
    rc <- rc + aws_sync(PATHS$forms, s3_uri(S3$forms_prefix), dry_run, logger = logger)
  } else {
    log4r::info(logger, "skip: ENABLE_UPLOAD_FORMS=FALSE")
  }

  if (isTRUE(CONFIG$ENABLE_UPLOAD_INTERMEDIATE)) {
    rc <- rc + aws_sync(PATHS$unpacked,   s3_uri(S3$unpacked_prefix),   dry_run, logger = logger)
    rc <- rc + aws_sync(PATHS$harmonized, s3_uri(S3$harmonized_prefix), dry_run, logger = logger)
  } else {
    log4r::info(logger, "skip: ENABLE_UPLOAD_INTERMEDIATE=FALSE")
  }

  if (isTRUE(CONFIG$ENABLE_UPLOAD_PROCESSED)) {
    rc <- rc + sync_processed_tier(
      processed_root = PATHS$processed,
      s3_dest        = s3_uri(S3$processed_prefix),
      dry_run        = dry_run,
      gzip_html      = isTRUE(CONFIG$ENABLE_GZIP_HTML_UPLOAD),
      logger         = logger
    )
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


# ---- Legacy-pipeline upload (used by run_legacy_pipeline.R) ----

#' Phase 8 of the legacy pipeline. Promotes harmonized_legacy/ CSVs into
#' processed_legacy/ (alongside dictionaries written by phase 6), then syncs
#' that tier to s3://nccsdata/processed_legacy/core/. Quality RDS + HTML
#' artifacts live under data/logs/legacy/ and docs/quality-reports/legacy/
#' respectively.
#'
#' Legacy is mostly an intermediate to the merge (analyst-facing artifact is
#' the merged panel at s3://nccsdata/processed_merged/), so this upload exists
#' for parity and rehydration — not as the primary distribution path.
#'
#' Mirror of run_upload_merged().
run_upload_legacy <- function(dry_run       = FALSE,
                              run_timestamp = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
                              enable_upload = CONFIG$ENABLE_S3_UPLOAD) {

  dir.create(PATHS$logs_legacy, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs_legacy, "08_upload_log.txt"))

  log4r::info(logger, sprintf("Phase 8 (legacy) start: dry_run=%s, run_timestamp=%s",
                              dry_run, run_timestamp))

  promote_harmonized_to_processed(
    harmonized_root = PATHS$harmonized_legacy,
    processed_root  = PATHS$processed_legacy,
    logger          = logger
  )

  if (!isTRUE(enable_upload)) {
    log4r::warn(logger, "enable_upload is FALSE; skipping S3 sync of legacy tier")
    return(invisible(NULL))
  }

  if (!isTRUE(CONFIG$ENABLE_UPLOAD_PROCESSED)) {
    log4r::info(logger, "skip: ENABLE_UPLOAD_PROCESSED=FALSE (applies to legacy tier as well)")
    return(invisible(NULL))
  }

  rc <- sync_processed_tier(
    processed_root = PATHS$processed_legacy,
    s3_dest        = s3_uri(S3$processed_legacy_prefix),
    dry_run        = dry_run,
    gzip_html      = isTRUE(CONFIG$ENABLE_GZIP_HTML_UPLOAD),
    logger         = logger
  )

  log4r::info(logger, sprintf("Phase 8 (legacy) complete: rc=%d", rc))
  if (rc != 0L) {
    stop(sprintf("Legacy-tier upload had failures (rc=%d). See %s/08_upload_log.txt",
                 rc, PATHS$logs_legacy))
  }
  invisible(NULL)
}

# ---- Merged-panel upload (used by run_build_panel.R) ----

#' Phase 8 of the merged-panel pipeline. Promotes harmonized_merged/ CSVs into
#' processed_merged/ (alongside dictionaries written by phase 6), then syncs
#' that tier to s3://nccsdata/processed_merged/core/. Quality RDS + HTML
#' artifacts live under data/logs/merged/ and docs/quality-reports/merged/
#' respectively; RDS rides along with the SOI-current pipeline's logs sync
#' (recursive on data/logs/), HTML is served by GitHub Pages.
#'
#' Mirror of run_upload() but scoped to the merged tier only; nothing here
#' touches data/raw/, data/intermediate/, or data/processed/.
run_upload_merged <- function(dry_run       = FALSE,
                              run_timestamp = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
                              enable_upload = CONFIG$ENABLE_S3_UPLOAD) {

  dir.create(PATHS$logs_merged, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs_merged, "08_upload_log.txt"))

  log4r::info(logger, sprintf("Phase 8 (merged) start: dry_run=%s, run_timestamp=%s",
                              dry_run, run_timestamp))

  # Promotion is always attempted regardless of enable_upload — the promote
  # step is local-only and harmless if S3 sync is skipped.
  promote_harmonized_to_processed(
    harmonized_root = PATHS$harmonized_merged,
    processed_root  = PATHS$processed_merged,
    logger          = logger
  )

  if (!isTRUE(enable_upload)) {
    log4r::warn(logger, "enable_upload is FALSE; skipping S3 sync of merged tier")
    return(invisible(NULL))
  }

  if (!isTRUE(CONFIG$ENABLE_UPLOAD_PROCESSED)) {
    log4r::info(logger, "skip: ENABLE_UPLOAD_PROCESSED=FALSE (applies to merged tier as well)")
    return(invisible(NULL))
  }

  rc <- sync_processed_tier(
    processed_root = PATHS$processed_merged,
    s3_dest        = s3_uri(S3$processed_merged_prefix),
    dry_run        = dry_run,
    gzip_html      = isTRUE(CONFIG$ENABLE_GZIP_HTML_UPLOAD),
    logger         = logger
  )

  log4r::info(logger, sprintf("Phase 8 (merged) complete: rc=%d", rc))
  if (rc != 0L) {
    stop(sprintf("Merged-tier upload had failures (rc=%d). See %s/08_upload_log.txt",
                 rc, PATHS$logs_merged))
  }
  invisible(NULL)
}

if (sys.nframe() == 0L) run_upload()
