# R/04_derive_combined.R
# Phase 5: derive `990combined` by stacking harmonized 990 + 990-EZ rows on the
# 53 (intersect-of-crosswalks) shared harmonized columns plus the universal
# pipeline columns. One CSV per tax_year at
# data/intermediate/harmonized/{tax_year}/990combined/core_{tax_year}_990combined.csv.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))

# Universal pipeline-added columns preserved in 990combined alongside the
# crosswalk intersection. `source_form` is added by this step to record which
# form each row came from.
COMBINED_UNIVERSAL_COLS <- c("tax_year", "tax_month", "is_501c3",
                             "extract_year", "is_amendment")

#' Compute the set of harmonized column names shared between 990 and 990-EZ.
shared_990_990ez_cols <- function() {
  x990   <- fread(CROSSWALK_FILES[["990"]])
  x990ez <- fread(CROSSWALK_FILES[["990ez"]])
  intersect(unique(x990$harmonized_name), unique(x990ez$harmonized_name))
}

#' Project a data.table to the shared schema, padding missing cols with NA.
project_to_shared <- function(dt, shared_cols) {
  missing <- setdiff(shared_cols, names(dt))
  if (length(missing)) dt[, (missing) := NA]
  cols_to_keep <- intersect(c(shared_cols, COMBINED_UNIVERSAL_COLS), names(dt))
  dt[, ..cols_to_keep]
}

run_derive_combined <- function(harmonized_root = PATHS$harmonized) {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "04_derive_combined_log.txt"))

  shared <- shared_990_990ez_cols()
  log4r::info(logger, sprintf("Shared schema: %d harmonized cols", length(shared)))

  tax_year_dirs <- list.dirs(harmonized_root, recursive = FALSE, full.names = TRUE)

  for (yd in tax_year_dirs) {
    yr <- as.integer(basename(yd))
    f990   <- file.path(yd, "990",   sprintf("core_%d_990.csv", yr))
    f990ez <- file.path(yd, "990ez", sprintf("core_%d_990ez.csv", yr))

    have_990   <- file.exists(f990)
    have_990ez <- file.exists(f990ez)
    if (!have_990 && !have_990ez) next

    pieces <- list()
    if (have_990) {
      dt <- fread(f990, colClasses = c(ein = "character", tax_period = "character"))
      dt <- project_to_shared(dt, shared)
      dt[, source_form := "990"]
      pieces[[length(pieces) + 1L]] <- dt
    }
    if (have_990ez) {
      dt <- fread(f990ez, colClasses = c(ein = "character", tax_period = "character"))
      dt <- project_to_shared(dt, shared)
      dt[, source_form := "990ez"]
      pieces[[length(pieces) + 1L]] <- dt
    }

    combined <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
    out_dir  <- file.path(harmonized_root, yr, "990combined")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(out_dir, sprintf("core_%d_990combined.csv", yr))
    fwrite(combined, out_path)
    log4r::info(logger, sprintf("WROTE %s (%d rows, %d cols; 990=%d, 990ez=%d)",
                                out_path, nrow(combined), ncol(combined),
                                sum(combined$source_form == "990"),
                                sum(combined$source_form == "990ez")))
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) run_derive_combined()
