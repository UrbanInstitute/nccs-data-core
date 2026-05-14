# R/09_parquet.R
# Phase 9: write a .parquet copy next to every processed .csv under
# data/processed/. Parquet outputs are intended for API serving and R-package
# consumption — much faster column-wise reads than CSV at the cost of being
# binary (less convenient for ad-hoc inspection).
#
# Idempotent: a .parquet that exists and is at least as new as its .csv source
# is skipped. Pass overwrite=TRUE to regenerate all.
#
# Pipeline-agnostic: works against any directory tree the SOI-current or
# legacy pipelines write to under PATHS$processed.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "create_logger.R"))

#' Convert one CSV to a sibling Parquet file.
#'
#' @return TRUE if a parquet was written (or already up-to-date), FALSE on error.
.csv_to_parquet <- function(csv_path, overwrite, logger) {
  pq_path <- sub("\\.csv$", ".parquet", csv_path, ignore.case = TRUE)
  if (pq_path == csv_path) {
    # No .csv suffix — defensive; skip.
    log4r::warn(logger, sprintf("SKIP no .csv suffix: %s", csv_path))
    return(FALSE)
  }
  if (!overwrite && file.exists(pq_path) &&
      file.info(pq_path)$mtime >= file.info(csv_path)$mtime) {
    log4r::info(logger, sprintf("SKIP up-to-date: %s", pq_path))
    return(TRUE)
  }
  tryCatch({
    df <- data.table::fread(csv_path)
    arrow::write_parquet(df, pq_path)
    log4r::info(logger, sprintf("OK %s (%.1f MB csv -> %.1f MB parquet)",
                                pq_path,
                                file.info(csv_path)$size / 1e6,
                                file.info(pq_path)$size / 1e6))
    TRUE
  }, error = function(e) {
    log4r::error(logger, sprintf("FAIL %s: %s", csv_path, conditionMessage(e)))
    if (file.exists(pq_path)) unlink(pq_path)
    FALSE
  })
}

#' Walk a tree, write Parquet next to every CSV found.
#'
#' @param processed_root directory to walk. Defaults to PATHS$processed.
#' @param overwrite if TRUE, regenerate parquets that are already newer than their csv.
run_parquet <- function(processed_root = PATHS$processed,
                        overwrite = FALSE) {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "09_parquet_log.txt"))

  if (!dir.exists(processed_root)) {
    log4r::warn(logger, sprintf("processed_root does not exist: %s", processed_root))
    return(invisible(NULL))
  }

  csvs <- list.files(processed_root, pattern = "\\.csv$", recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  log4r::info(logger, sprintf("Walking %s: %d CSVs", processed_root, length(csvs)))

  n_ok <- 0L; n_fail <- 0L
  for (p in csvs) {
    ok <- .csv_to_parquet(p, overwrite = overwrite, logger = logger)
    if (isTRUE(ok)) n_ok <- n_ok + 1L else n_fail <- n_fail + 1L
  }
  log4r::info(logger, sprintf("=== run_parquet done: %d ok, %d failed ===",
                              n_ok, n_fail))
  invisible(list(n_ok = n_ok, n_fail = n_fail))
}

if (sys.nframe() == 0L) run_parquet()
