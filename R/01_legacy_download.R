# R/01_legacy_download.R
# Phase 1 of the legacy CORE pipeline: mirror in-scope legacy CSVs from
# s3://nccsdata/legacy/core/ to data/raw/legacy/core/.
#
# In scope (per docs/09 locked-in decisions):
#   - Tax years 1989-2011 only.
#   - Scopes PZ (501C3-CHARITIES + 501CE-NONPROFIT) and PF (501C3-PRIVFOUND).
#   - 2012+ NCCS+SOI hybrids and the PC scope (which only exists post-2011)
#     are out of scope and never downloaded.
#
# Uses aws s3 cp per-file rather than `aws s3 sync` because sync's --exclude
# logic over many keys is fiddly; per-file with an explicit allowlist is
# simpler and idempotent (skip-if-exists by default).
#
# Output layout: data/raw/legacy/core/CORE-{YYYY}-{SUBSECTION_CLASS}-{SCOPE}.csv
# (flat, no per-year subdirectory — matches the upstream S3 layout).

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "create_logger.R"))

# In-scope filter — same regex as scripts/inventory_legacy.R.
.LEGACY_PATTERN <- "^CORE-(\\d{4})-(501C3-CHARITIES|501CE-NONPROFIT|501C3-PRIVFOUND)-(PZ|PF)\\.csv$"
.LEGACY_YEAR_LO <- 1989L
.LEGACY_YEAR_HI <- 2011L

#' List in-scope legacy keys from s3://nccsdata/legacy/core/.
#'
#' Returns a character vector of bucket keys (e.g., "legacy/core/CORE-1989-...").
#' Filters to 1989-2011 PZ + PF; the 2012+ hybrids and any PC files are dropped.
list_legacy_keys <- function() {
  out <- system2("aws", c("s3", "ls", sprintf("s3://%s/legacy/core/", S3$bucket)),
                 stdout = TRUE, stderr = TRUE)
  if (length(out) == 0L) {
    stop("Empty listing for s3://", S3$bucket, "/legacy/core/. Check AWS credentials.")
  }
  # `aws s3 ls` returns lines like: "2023-10-24 21:29:04  63606704 CORE-1989-...csv"
  filenames <- sub("^.*\\s+", "", out)
  filenames <- filenames[grepl(.LEGACY_PATTERN, filenames)]
  yrs <- as.integer(sub(.LEGACY_PATTERN, "\\1", filenames))
  filenames <- filenames[yrs >= .LEGACY_YEAR_LO & yrs <= .LEGACY_YEAR_HI]
  paste0("legacy/core/", filenames)
}

#' Download in-scope legacy files. Idempotent: skips files already on disk.
run_legacy_download <- function(dest_root = PATHS$legacy_raw,
                                overwrite = FALSE) {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "01_legacy_download_log.txt"))

  keys <- list_legacy_keys()
  log4r::info(logger, sprintf("In-scope legacy keys: %d", length(keys)))

  for (k in keys) {
    fn <- basename(k)
    dest <- file.path(dest_root, fn)
    if (file.exists(dest) && !overwrite) {
      log4r::info(logger, sprintf("SKIP exists: %s", dest))
      next
    }
    src <- sprintf("s3://%s/%s", S3$bucket, k)
    log4r::info(logger, sprintf("FETCH %s -> %s", src, dest))
    rc <- system2("aws", c("s3", "cp", src, dest, "--no-progress"),
                  stdout = FALSE, stderr = FALSE)
    if (rc == 0L && file.exists(dest)) {
      log4r::info(logger, sprintf("OK %s (%.1f MB)", dest, file.info(dest)$size / 1e6))
    } else {
      log4r::error(logger, sprintf("FAIL %s (rc=%d)", src, rc))
      if (file.exists(dest)) unlink(dest)
    }
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) run_legacy_download()
