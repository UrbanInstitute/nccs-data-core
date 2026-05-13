# R/01_download.R
# Phase 1: download IRS SOI extract zips for each (processing_year, form) into
# data/raw/soi_extracts/{processing_year}/. Idempotent: skips files already on disk.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "aws_s3_sync.R"))

#' Rehydrate SOI dictionaries from S3 into PATHS$soi_dictionaries.
#'
#' The dictionary xls(x) files and derived _var_matrix_*.csv files are small
#' (~1.5 MB total), archival, and used by pre-checks for column-count
#' tolerance. They're gitignored, so a fresh EC2 has no local copy — without
#' this, pre-checks log "No expected column count available; skipping
#' tolerance check". `aws s3 sync` is idempotent: subsequent runs are no-ops.
rehydrate_soi_dictionaries <- function(logger = NULL) {
  dir.create(PATHS$soi_dictionaries, recursive = TRUE, showWarnings = FALSE)
  src <- sprintf("s3://%s/%s/", S3$bucket, S3$dictionaries_prefix)
  aws_sync(src, PATHS$soi_dictionaries, logger = logger)
}

run_download <- function(processing_years = CONFIG$EARLIEST_YEAR:CONFIG$LATEST_YEAR,
                         forms = CONFIG$FORMS,
                         dest_root = PATHS$soi_extracts,
                         overwrite = FALSE,
                         timeout_sec = 600L) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "01_download_log.txt"))

  rehydrate_soi_dictionaries(logger = logger)

  old_timeout <- getOption("timeout")
  options(timeout = timeout_sec)
  on.exit(options(timeout = old_timeout), add = TRUE)

  combos <- expand.grid(processing_year = processing_years,
                        form = forms,
                        stringsAsFactors = FALSE)

  for (i in seq_len(nrow(combos))) {
    py   <- combos$processing_year[i]
    form <- combos$form[i]
    url  <- build_soi_url(py, form)
    if (is.na(url)) {
      log4r::info(logger, sprintf("SKIP no published extract for %s/%d", form, py))
      next
    }

    year_dir <- file.path(dest_root, py)
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    dest <- file.path(year_dir, basename(url))

    if (file.exists(dest) && !overwrite) {
      log4r::info(logger, sprintf("SKIP exists: %s", dest))
      next
    }

    log4r::info(logger, sprintf("FETCH %s -> %s", url, dest))
    result <- tryCatch(
      utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
      error = function(e) { log4r::error(logger, sprintf("FAIL %s: %s", url, conditionMessage(e))); -1L },
      warning = function(w) { log4r::warn(logger, sprintf("WARN %s: %s", url, conditionMessage(w))); -1L }
    )
    if (identical(result, 0L)) {
      log4r::info(logger, sprintf("OK %s (%.1f MB)", dest, file.info(dest)$size / 1e6))
    } else if (file.exists(dest)) {
      unlink(dest)
    }
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) run_download()
