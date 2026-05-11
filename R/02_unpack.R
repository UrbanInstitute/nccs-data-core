# R/02_unpack.R
# Phase 2: unzip raw SOI extracts into data/intermediate/unpacked/{processing_year}/{form}/.
# Each zip contains exactly one data file (.dat for 2012, .csv for 2013+).

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))

# Parse SOI extract zip filename -> list(yy, form).
# Handles all observed IRS naming variants for 2012-2024.
parse_extract_filename <- function(zip_path) {
  base <- basename(zip_path)
  m <- regmatches(base,
                  regexec("^(\\d{2})eo(?:fin)?extract(990pf|990ez|990EZ|EZ|ez|990)\\.zip$",
                          base))[[1]]
  if (length(m) != 3L) stop(sprintf("Unrecognized SOI zip name: %s", base))
  tag <- m[3]
  form <- if (tag %in% c("990ez", "990EZ", "EZ", "ez")) "990ez"
          else if (tag == "990pf")                      "990pf"
          else                                          "990"
  list(yy = m[2], form = form)
}

run_unpack <- function(src_root  = PATHS$soi_extracts,
                       dest_root = PATHS$unpacked,
                       overwrite = FALSE) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "02_unpack_log.txt"))

  zips <- list.files(src_root,
                     pattern = "^\\d{2}eo(fin)?extract(990(ez|EZ|pf)?|ez|EZ)\\.zip$",
                     recursive = TRUE,
                     full.names = TRUE)

  if (length(zips) == 0L) {
    log4r::warn(logger, sprintf("No SOI zips found under %s", src_root))
    return(invisible(NULL))
  }

  for (zip_path in zips) {
    meta <- parse_extract_filename(zip_path)
    processing_year <- as.integer(paste0(if (meta$yy >= "90") "19" else "20", meta$yy))
    out_dir <- file.path(dest_root, processing_year, meta$form)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    inside <- unzip(zip_path, list = TRUE)$Name
    existing <- file.path(out_dir, basename(inside))
    if (all(file.exists(existing)) && !overwrite) {
      log4r::info(logger, sprintf("SKIP unpacked: %s", zip_path))
      next
    }

    log4r::info(logger, sprintf("UNZIP %s -> %s", zip_path, out_dir))
    extracted <- unzip(zip_path, exdir = out_dir, junkpaths = TRUE, overwrite = TRUE)
    for (f in extracted) {
      log4r::info(logger, sprintf("  %s (%.1f MB)", basename(f), file.info(f)$size / 1e6))
    }
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) run_unpack()
