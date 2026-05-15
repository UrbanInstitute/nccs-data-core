# R/03_legacy_harmonize.R
# Phase 3 of the legacy CORE pipeline.
#
# Reads:
#   data/raw/legacy/core/CORE-{YYYY}-{SUBSECTION_CLASS}-{SCOPE}.csv  (raw legacy NCCS files)
#   data/crosswalks/legacy_pz_crosswalk_FINAL.csv                    (PZ -> 990combined)
#   data/crosswalks/legacy_pf_crosswalk_FINAL.csv                    (PF -> 990pf)
#
# Writes one CSV per (tax_year, form) under
#   data/intermediate/harmonized_legacy/{tax_year}/{990combined|990pf}/core_{tax_year}_{form}.csv
#
# Writes to a SEPARATE tree from the SOI-current pipeline (PATHS$harmonized).
# Both pipelines can produce files at the same tax_year (SOI-current's 2012+
# extracts contain late-filer rows with TAXPER going back to the 1990s) but
# the schemas differ — SOI-current 990combined is a 53-col 990∩990ez intersect;
# legacy 990combined uses the full legacy_pz crosswalk (~120 cols). Keeping
# the trees separate prevents path collisions and lets each quality run
# validate against its own crosswalk. A future merge step can combine them
# at the processed/ tier if a unified pre-2012-to-present panel is needed.
#
# Per-form mechanics:
#
# 990combined (legacy PZ):
#   - For each tax year, read BOTH CORE-{YYYY}-501C3-CHARITIES-PZ.csv and
#     CORE-{YYYY}-501CE-NONPROFIT-PZ.csv. Union with rbindlist(fill=TRUE).
#   - subsection_cd derivation: rows from 501C3-CHARITIES files get
#     subsection_cd=3L unconditionally (it's a single-subsection partition).
#     Rows from 501CE-NONPROFIT use the row's own subsection-coded column
#     (SUBSECCD or equivalent, harmonized to subsection_cd by the crosswalk)
#     when present, else NA — analyst-visible signal that the legacy NCCS
#     file did not record subsection at row level for that vintage.
#   - is_501c3 := subsection_cd == 3L (strict-boolean, matches SOI-current).
#
# 990pf (legacy PF):
#   - Single CORE-{YYYY}-501C3-PRIVFOUND-PF.csv per year.
#   - subsection_cd = 3L unconditionally (private foundations are 501(c)(3) by
#     definition of the form). is_501c3 = TRUE for all rows.
#
# Other notes:
#   - tax_year is derived from the source filename ("CORE-1995-..." -> 1995).
#     The legacy NCCS files were partitioned by tax year at write time, so the
#     filename is authoritative. Any TAXPER / FisYr column in the source rows
#     is renamed via crosswalk to tax_period (when present) for downstream
#     compatibility but is NOT used to derive tax_year.
#   - Crosswalk rows with harmonized_name == "" represent NCCS-appended BMF
#     metadata that the schema-parity decision (docs/09) says to drop. They
#     are filtered out before the rename step.

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

LEGACY_CROSSWALK_FILES <- list(
  "990combined" = "data/crosswalks/legacy_pz_crosswalk_FINAL.csv",
  "990pf"       = "data/crosswalks/legacy_pf_crosswalk_FINAL.csv"
)

# Subsection classes recognized in legacy filenames; their is_501c3 implication.
LEGACY_SUBCLASS_IS_501C3 <- c(
  "501C3-CHARITIES" = TRUE,
  "501CE-NONPROFIT" = NA,     # mixed bag: subsection inferred from row column
  "501C3-PRIVFOUND" = TRUE
)

LEGACY_FILENAME_RE <-
  "^CORE-(\\d{4})-(501C3-CHARITIES|501CE-NONPROFIT|501C3-PRIVFOUND)-(PZ|PF)\\.csv$"

# Identity / derived columns excluded from blanket financial coercion.
IDENTITY_COLS <- c("ein", "tax_period", "tax_year", "tax_month",
                   "subsection_cd", "is_501c3", "efile_indicator",
                   "source_subsection_class")
# subsection_cd has the _cd suffix but is an integer code.
INDICATOR_EXCLUDE <- c("subsection_cd")

# Required after the rename step. ein is universal; subsection_cd is filled
# either by the crosswalk or by the filename-derived fallback below.
REQUIRED_HARMONIZED_COLS <- c("ein")

#' Read the legacy FINAL crosswalk and return a lookup with empty-harmonized
#' (i.e., dropped) rows filtered out.
load_legacy_crosswalk <- function(form) {
  path <- LEGACY_CROSSWALK_FILES[[form]]
  if (is.null(path)) stop(sprintf("No legacy crosswalk for form '%s'", form))
  xwalk <- fread(path, colClasses = "character")
  if (!"source_column" %in% names(xwalk) || !"harmonized_name" %in% names(xwalk)) {
    stop(sprintf("Crosswalk %s missing source_column/harmonized_name", path))
  }
  xwalk <- xwalk[!is.na(harmonized_name) & harmonized_name != ""]
  xwalk[, source_var_lc := tolower(source_column)]
  xwalk
}

#' Parse a legacy filename into its (tax_year, subsection_class, scope).
parse_legacy_filename <- function(filename) {
  m <- regmatches(filename, regexec(LEGACY_FILENAME_RE, filename))[[1]]
  if (length(m) != 4L) stop(sprintf("Unrecognized legacy filename: %s", filename))
  list(tax_year         = as.integer(m[2]),
       subsection_class = m[3],
       scope            = m[4])
}

#' List the legacy source files for a given form ("990combined" -> PZ, "990pf" -> PF).
list_legacy_sources <- function(form, source_root) {
  if (form == "990combined") {
    pat <- "^CORE-\\d{4}-(501C3-CHARITIES|501CE-NONPROFIT)-PZ\\.csv$"
  } else if (form == "990pf") {
    pat <- "^CORE-\\d{4}-501C3-PRIVFOUND-PF\\.csv$"
  } else {
    stop(sprintf("Unknown legacy form '%s'", form))
  }
  files <- list.files(source_root, pattern = pat, full.names = TRUE)
  sort(files)
}

#' Read one legacy CSV; lowercase column names; tag with filename-derived
#' tax_year + subsection_class.
read_legacy_source <- function(path, logger) {
  meta <- parse_legacy_filename(basename(path))
  log4r::info(logger, sprintf("READ %s (tax_year=%d, subclass=%s)",
                              path, meta$tax_year, meta$subsection_class))
  dt <- fread(path, header = TRUE, na.strings = c("", "NA", "."),
              colClasses = "character", fill = TRUE)
  setnames(dt, tolower(names(dt)))
  # Pipeline-added columns (kept through crosswalk apply via the rename
  # lookup miss path being treated as drop — so we add them AFTER apply).
  attr(dt, "tax_year_from_filename") <- meta$tax_year
  attr(dt, "subsection_class")       <- meta$subsection_class
  dt
}

#' Apply the legacy crosswalk: drop columns absent from the lookup; rename
#' columns present; coalesce synonyms; pad missing harmonized columns with NA.
apply_legacy_crosswalk <- function(dt, xwalk, logger) {
  lookup <- setNames(xwalk$harmonized_name, xwalk$source_var_lc)
  src_cols <- names(dt)
  in_xwalk <- src_cols %in% names(lookup)

  dropped <- src_cols[!in_xwalk]
  if (length(dropped)) {
    log4r::info(logger, sprintf("Dropping %d source columns not in legacy crosswalk (BMF-origin or threshold-filtered)",
                                length(dropped)))
    dt[, (dropped) := NULL]
  }

  src_cols <- names(dt)
  new_names <- unname(lookup[src_cols])
  setnames(dt, src_cols, new_names)

  # Synonyms: multiple legacy source columns -> same harmonized name. Coalesce.
  dup_names <- unique(new_names[duplicated(new_names)])
  for (nm in dup_names) {
    idx <- which(names(dt) == nm)
    coalesced <- Reduce(function(a, b) ifelse(is.na(a) | a == "", b, a),
                        lapply(idx, function(i) dt[[i]]))
    dt[, (nm) := coalesced]
    drop_idx <- idx[-1]
    dt[, (drop_idx) := NULL]
  }

  missing_required <- setdiff(REQUIRED_HARMONIZED_COLS, names(dt))
  if (length(missing_required)) {
    msg <- sprintf("Required identity column(s) absent after crosswalk apply: %s",
                   paste(missing_required, collapse = ", "))
    log4r::error(logger, msg)
    stop(msg)
  }

  # Pad missing harmonized columns with NA so every (tax_year, form) output
  # has the same schema (per the docs/09 vintage-padding decision).
  all_harmonized <- unique(xwalk$harmonized_name)
  missing_h <- setdiff(all_harmonized, names(dt))
  if (length(missing_h)) {
    dt[, (missing_h) := NA_character_]
  }

  dt
}

#' Apply per-field transforms. Mirrors R/03_harmonize.R::apply_transforms.
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

#' Derive subsection_cd from the filename's subclass when the row-level
#' column is NA (501C3-CHARITIES and 501C3-PRIVFOUND files are
#' single-subsection partitions; their rows are unambiguously subsection 3).
derive_subsection_from_partition <- function(dt, subsection_class, logger) {
  if (!subsection_class %in% c("501C3-CHARITIES", "501C3-PRIVFOUND")) {
    # 501CE-NONPROFIT is mixed; rely on row's own value entirely.
    return(invisible(dt))
  }
  if (!"subsection_cd" %in% names(dt)) {
    dt[, subsection_cd := 3L]
    log4r::info(logger,
                sprintf("Filled subsection_cd=3 for %d rows from filename partition (%s)",
                        nrow(dt), subsection_class))
    return(invisible(dt))
  }
  na_idx <- which(is.na(dt$subsection_cd))
  if (length(na_idx)) {
    dt[na_idx, subsection_cd := 3L]
    log4r::info(logger,
                sprintf("Filled subsection_cd=3 for %d NA rows from filename partition (%s)",
                        length(na_idx), subsection_class))
  }
  invisible(dt)
}

#' Harmonize a single legacy form across all years. Returns NULL if no
#' source files exist for the form.
harmonize_legacy_form <- function(form, xwalk, source_root, logger) {
  files <- list_legacy_sources(form, source_root)
  if (length(files) == 0L) {
    log4r::warn(logger, sprintf("No legacy source files for form '%s' under %s",
                                form, source_root))
    return(NULL)
  }

  pieces <- list()
  for (f in files) {
    dt <- read_legacy_source(f, logger)
    tax_year_fn <- attr(dt, "tax_year_from_filename")
    subclass    <- attr(dt, "subsection_class")
    log4r::info(logger, sprintf("  %d rows, %d source cols", nrow(dt), ncol(dt)))

    apply_legacy_crosswalk(dt, xwalk, logger)
    apply_transforms(dt, logger)
    derive_subsection_from_partition(dt, subclass, logger)

    # tax_year is set by transform_tax_period from TAXPER's first 4 chars (per
    # CLAUDE.md: "Outputs are partitioned by tax year (first 4 of TAXPER), not
    # the year the form was filed"). The legacy NCCS file's filename encodes a
    # publication year that may differ from a row's TAXPER (e.g., the 1989
    # NCCS file contains late filers with TAXPER 1987-1990). Fall back to the
    # filename year only for rows whose TAXPER was missing/invalid, so we
    # don't silently drop those rows in partition_and_write.
    if (!"tax_year" %in% names(dt)) dt[, tax_year := NA_integer_]
    dt[is.na(tax_year), tax_year := tax_year_fn]
    dt[, source_subsection_class := subclass]
    if ("subsection_cd" %in% names(dt)) {
      dt[, is_501c3 := !is.na(subsection_cd) & subsection_cd == 3L]
    }

    pieces[[length(pieces) + 1L]] <- dt
  }

  combined <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
  log4r::info(logger, sprintf("Combined %s: %d rows across %d source files",
                              form, nrow(combined), length(files)))

  # Drop rows whose TAXPER spills into 2012+. The 2011 NCCS file contains a
  # non-trivial population of rows with TAXPER starting "2012", presumably
  # fiscal-year-2012 filers that NCCS captured early. They get partitioned
  # into harmonized_legacy/2012/ with the narrow legacy schema and then fail
  # the strict schema gate. SOI-current's 2012 extracts have the wide schema
  # and own tax_year >= 2012 anyway, so dropping the spillover here is the
  # right move — the merge pipeline (Option D) sees the same orgs from the
  # SOI side without losing coverage.
  if ("tax_year" %in% names(combined)) {
    n_before <- nrow(combined)
    combined <- combined[is.na(tax_year) | tax_year <= LEGACY_TAX_YEAR_MAX]
    n_dropped <- n_before - nrow(combined)
    if (n_dropped > 0L) {
      log4r::info(logger,
                  sprintf("Dropped %d rows with tax_year > %d (SOI-current owns these)",
                          n_dropped, LEGACY_TAX_YEAR_MAX))
    }
  }

  combined
}

partition_and_write <- function(dt, form, dest_root, logger) {
  yrs <- sort(unique(stats::na.omit(dt$tax_year)))
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

run_legacy_harmonize <- function(forms       = c("990combined", "990pf"),
                                 source_root = PATHS$legacy_raw,
                                 dest_root   = PATHS$harmonized_legacy) {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "03_legacy_harmonize_log.txt"))

  if (!dir.exists(source_root)) {
    log4r::error(logger, sprintf("source_root does not exist: %s", source_root))
    stop("Run phase 1 (legacy download) before harmonize.")
  }

  for (form in forms) {
    xwalk <- load_legacy_crosswalk(form)
    log4r::info(logger,
                sprintf("==== FORM %s (%d harmonized cols after dropping empty) ====",
                        form, length(unique(xwalk$harmonized_name))))
    combined <- harmonize_legacy_form(form, xwalk, source_root, logger)
    if (is.null(combined)) next
    partition_and_write(combined, form, dest_root, logger)
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) run_legacy_harmonize()
