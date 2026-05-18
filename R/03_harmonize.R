# R/03_harmonize.R
# Phase 4: apply FINAL crosswalk + transforms, partition by tax_year, write one
# CSV per (tax_year, form) to data/intermediate/harmonized/{tax_year}/{form}/.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "transforms", "tax_period.R"))
source(here("R", "transforms", "ein.R"))
source(here("R", "transforms", "subsection.R"))
source(here("R", "transforms", "financial_amounts.R"))
source(here("R", "transforms", "indicators.R"))
source(here("R", "transforms", "efile_indicator.R"))

# Identity / derived columns excluded from blanket financial coercion.
IDENTITY_COLS  <- c("ein", "tax_period", "tax_year", "tax_month",
                    "subsection_cd", "is_501c3", "efile_indicator",
                    "extract_year", "is_amendment")
# Columns that MUST be mapped from a source var in every vintage. Missing one is fatal
# (an all-NA identity column silently corrupts every downstream partition).
REQUIRED_HARMONIZED_COLS <- c("ein", "tax_period", "subsection_cd")
# subsection_cd has the _cd suffix but is an integer code, not a binary indicator.
INDICATOR_EXCLUDE <- c("subsection_cd")

load_crosswalk <- function(form) {
  path <- CROSSWALK_FILES[[form]]
  if (is.null(path)) stop(sprintf("No crosswalk path for form '%s'", form))
  xwalk <- fread(path, colClasses = "character")
  xwalk[, source_var_lc := tolower(source_var)]
  xwalk
}

read_source <- function(processing_year, form, logger) {
  src_dir <- file.path(PATHS$unpacked, processing_year, form)
  files <- list.files(src_dir, pattern = "\\.(csv|dat)$", full.names = TRUE)
  if (length(files) == 0L) {
    log4r::warn(logger, sprintf("No unpacked file for %s/%s under %s",
                                processing_year, form, src_dir))
    return(NULL)
  }
  if (length(files) > 1L) {
    log4r::warn(logger, sprintf("Multiple files in %s; using first: %s",
                                src_dir, basename(files[1])))
  }
  src <- files[1]
  fmt <- SOURCE_FORMAT_FROM_PATH(src)
  log4r::info(logger, sprintf("READ %s (sep='%s')", src, fmt$sep))
  # fill = TRUE: tolerate malformed rows (one IRS .dat file has a single bad
  # row that would otherwise truncate the read mid-file — e.g., 2014 990
  # stopped at row 2980 of 299,405 before this flag).
  dt <- fread(src, sep = fmt$sep, header = TRUE, na.strings = c("", "NA", "."),
              colClasses = "character", fill = TRUE)
  setnames(dt, tolower(names(dt)))
  dt
}

apply_crosswalk <- function(dt, xwalk, logger) {
  lookup <- setNames(xwalk$harmonized_name, xwalk$source_var_lc)
  src_cols <- names(dt)
  in_xwalk <- src_cols %in% names(lookup)

  dropped <- src_cols[!in_xwalk]
  if (length(dropped)) {
    log4r::warn(logger, sprintf("Dropping %d source columns not in crosswalk: %s",
                                length(dropped),
                                paste(head(dropped, 10), collapse = ", ")))
    dt[, (dropped) := NULL]
  }

  src_cols <- names(dt)
  new_names <- unname(lookup[src_cols])
  setnames(dt, src_cols, new_names)

  # Synonyms (multiple source vars -> same harmonized name): coalesce duplicates.
  dup_names <- unique(new_names[duplicated(new_names)])
  for (nm in dup_names) {
    idx <- which(names(dt) == nm)
    coalesced <- Reduce(function(a, b) ifelse(is.na(a) | a == "", b, a),
                        lapply(idx, function(i) dt[[i]]))
    dt[, (nm) := coalesced]
    drop_idx <- idx[-1]
    dt[, (drop_idx) := NULL]
  }

  # Guard: required identity columns must be present after rename + coalesce.
  # If any is missing, no source var in this vintage maps to it — fail loud rather
  # than silently NA-fill and produce a partitionless output.
  missing_required <- setdiff(REQUIRED_HARMONIZED_COLS, names(dt))
  if (length(missing_required)) {
    msg <- sprintf(
      "Required identity column(s) absent after crosswalk apply: %s. Check the FINAL crosswalk for missing source-var synonyms for this vintage.",
      paste(missing_required, collapse = ", "))
    log4r::error(logger, msg)
    stop(msg)
  }

  # Add NA placeholders for harmonized names absent from this vintage.
  all_harmonized <- unique(xwalk$harmonized_name)
  missing_h <- setdiff(all_harmonized, names(dt))
  if (length(missing_h)) {
    log4r::info(logger,
                sprintf("Padding %d harmonized columns not present in this vintage",
                        length(missing_h)))
    dt[, (missing_h) := NA_character_]
  }

  dt
}

apply_transforms <- function(dt, logger) {
  if ("tax_period"      %in% names(dt)) transform_tax_period(dt, logger)
  if ("ein"             %in% names(dt)) transform_ein(dt, logger)
  if ("subsection_cd"   %in% names(dt)) transform_subsection_cd(dt, logger)
  if ("efile_indicator" %in% names(dt)) transform_efile_indicator(dt, logger)

  indicator_cols <- setdiff(grep("_cd$", names(dt), value = TRUE), INDICATOR_EXCLUDE)
  if (length(indicator_cols)) transform_indicators(dt, indicator_cols, logger)

  financial_cols <- setdiff(names(dt),
                            c(IDENTITY_COLS, indicator_cols, INDICATOR_EXCLUDE))
  if (length(financial_cols)) transform_financial_amounts(dt, financial_cols, logger)

  dt
}

harmonize_one <- function(processing_year, form, xwalk, logger) {
  dt <- read_source(processing_year, form, logger)
  if (is.null(dt)) return(NULL)
  log4r::info(logger, sprintf("  %d rows, %d source cols", nrow(dt), ncol(dt)))
  apply_crosswalk(dt, xwalk, logger)
  apply_transforms(dt, logger)
  dt[, `extract_year` := processing_year]
  dt
}

partition_and_write <- function(dt, form, dest_root, logger) {
  yrs <- sort(unique(stats::na.omit(dt$tax_year)))

  # Clean stale partitions before writing the new set. Partition dirs from
  # a previous run may exist for years the current run no longer produces
  # — e.g. pre-2012 dirs written before the LEGACY_TAX_YEAR_MAX clamp landed.
  # Phase 5 quality walks the harmonized tree and would otherwise route stale
  # pre-2012 partitions through the legacy crosswalk dispatch, which has no
  # entry for 990/990ez (legacy uses 990pz/990pf), producing a hard failure.
  # Mirrors the same block in R/03_legacy_harmonize.R.
  if (dir.exists(dest_root)) {
    existing_year_dirs <- list.dirs(dest_root, recursive = FALSE, full.names = FALSE)
    existing_year_dirs <- existing_year_dirs[grepl("^\\d{4}$", existing_year_dirs)]
    existing_years <- as.integer(existing_year_dirs)
    stale_years <- setdiff(existing_years, yrs)
    for (sy in stale_years) {
      sy_form_dir <- file.path(dest_root, sy, form)
      if (dir.exists(sy_form_dir)) {
        unlink(sy_form_dir, recursive = TRUE)
        log4r::info(logger, sprintf("Removed stale partition %s", sy_form_dir))
        if (length(list.files(file.path(dest_root, sy))) == 0L) {
          unlink(file.path(dest_root, sy), recursive = TRUE)
        }
      }
    }
  }

  written <- character(0)
  for (yr in yrs) {
    out_dir <- file.path(dest_root, yr, form)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(out_dir, sprintf("core_%d_%s.csv", yr, form))
    chunk <- dt[tax_year == yr]
    fwrite(chunk, out_path)
    written <- c(written, out_path)
    log4r::info(logger, sprintf("WROTE %s (%d rows, %d cols)",
                                out_path, nrow(chunk), ncol(chunk)))
  }
  invisible(written)
}

run_harmonize <- function(processing_years = CONFIG$EARLIEST_YEAR:CONFIG$LATEST_YEAR,
                          forms     = CONFIG$FORMS,
                          dest_root = PATHS$harmonized) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "03_harmonize_log.txt"))

  for (form in forms) {
    xwalk <- load_crosswalk(form)
    log4r::info(logger, sprintf("==== FORM %s (%d harmonized cols) ====",
                                form, length(unique(xwalk$harmonized_name))))

    pieces <- list()
    for (py in processing_years) {
      src_dir <- file.path(PATHS$unpacked, py, form)
      if (!dir.exists(src_dir)) next
      log4r::info(logger, sprintf("-- processing_year %d / %s --", py, form))
      dt <- harmonize_one(py, form, xwalk, logger)
      if (!is.null(dt)) pieces[[length(pieces) + 1L]] <- dt
    }
    if (length(pieces) == 0L) {
      log4r::warn(logger, sprintf("No unpacked sources found for form %s", form))
      next
    }

    combined <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
    log4r::info(logger, sprintf("Combined %s: %d rows across %d processing years",
                                form, nrow(combined), length(pieces)))

    # is_amendment flag: within (ein, tax_period), earliest extract_year is the
    # original filing (is_amendment=FALSE); any later extract_years are amendments.
    combined[, is_amendment := extract_year > min(extract_year),
             by = .(ein, tax_period)]
    n_amend <- sum(combined$is_amendment, na.rm = TRUE)
    if (n_amend > 0L) {
      log4r::info(logger, sprintf("Flagged %d rows as amendments (ein+tax_period seen in multiple extracts)",
                                  n_amend))
    }

    partition_and_write(combined, form, dest_root, logger)
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) run_harmonize()
