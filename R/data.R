# R/data.R
# Static reference data: form inventory, per-vintage expected column counts,
# crosswalk file paths. Sourced by run_pipeline.R and quality checks.
# Run-time configuration (paths, flags, URL templates) lives in R/config.R.

# ---- Form inventory ----

FORMS_CURRENT  <- c("990", "990ez", "990pf")   # IRS SOI extracts, 2012+
FORMS_DERIVED  <- c("990combined")              # built from 990 + 990ez
FORMS_LEGACY   <- c("990pz", "990pf")           # raw legacy NCCS, 1989-2011 (separate pipeline)

ALL_OUTPUT_SERIES <- c(FORMS_CURRENT, FORMS_DERIVED)

# ---- Crosswalk file paths ----

CROSSWALK_FILES <- list(
  "990"    = "data/crosswalks/soi_990_crosswalk_FINAL.csv",
  "990ez"  = "data/crosswalks/soi_990ez_crosswalk_FINAL.csv",
  "990pf"  = "data/crosswalks/soi_990pf_crosswalk_FINAL.csv"
)

# Legacy crosswalks: only the 990combined and 990pf series have legacy
# coverage (1989-2011). The 990 and 990ez series start in 2012 (SOI-current).
LEGACY_CROSSWALK_FILES <- list(
  "990combined" = "data/crosswalks/legacy_pz_crosswalk_FINAL.csv",
  "990pf"       = "data/crosswalks/legacy_pf_crosswalk_FINAL.csv"
)

# Tax-year cutover between legacy and SOI-current pipelines.
LEGACY_TAX_YEAR_MAX <- 2011L

#' Resolve the crosswalk file path for a CORE output series + tax year.
#'
#' Pre-2012 rows are produced by the legacy pipeline and use the legacy_pz /
#' legacy_pf crosswalks; 2012+ rows use the SOI-current crosswalks. The
#' `990combined` SOI-current crosswalk routes to soi_990 (its dictionary +
#' quality reports use the 990 crosswalk since 990 covers all 53 shared cols);
#' for legacy 990combined the legacy_pz crosswalk is its own full schema.
#'
#' @param form one of "990", "990ez", "990pf", "990combined".
#' @param tax_year integer year of the output partition; if NULL, defaults to
#'   the SOI-current crosswalk (back-compat for non-tax-year-aware callers).
CROSSWALK_FOR_SERIES <- function(form, tax_year = NULL) {
  if (!is.null(tax_year) && tax_year <= LEGACY_TAX_YEAR_MAX) {
    path <- LEGACY_CROSSWALK_FILES[[form]]
    if (is.null(path)) {
      stop(sprintf("No legacy crosswalk for form '%s' (tax_year=%d)", form, tax_year))
    }
    return(path)
  }
  switch(form,
    "990"         = CROSSWALK_FILES[["990"]],
    "990ez"       = CROSSWALK_FILES[["990ez"]],
    "990pf"       = CROSSWALK_FILES[["990pf"]],
    "990combined" = CROSSWALK_FILES[["990"]],
    stop(sprintf("No crosswalk for form '%s'", form))
  )
}

#' TRUE if a crosswalk file uses the legacy schema (source_column) rather than
#' the SOI-current schema (source_var). Used by post-checks and downstream
#' consumers to dispatch column-name lookups.
is_legacy_crosswalk_path <- function(path) {
  grepl("/legacy_(pz|pf)_crosswalk_", path, fixed = FALSE)
}

# ---- Source-format quirks by file extension ----
# 2012-2017 IRS SOI extracts are space-delimited .dat files (the "eofinextract"
# program). 2018+ are comma-delimited .csv (the "eoextract" program). Detect
# from extension rather than year so the rule survives future URL/format drift.

SOURCE_FORMAT_FROM_PATH <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "dat") list(sep = " ", ext = "dat")
  else              list(sep = ",", ext = "csv")
}

# Backwards-compat shim for legacy call sites. Resolves the form's unpacked file
# under the standard intermediate path and inspects its extension. Returns the
# old-style list shape.
SOURCE_FORMAT <- function(processing_year, form = NULL) {
  if (!is.null(form)) {
    src_dir <- file.path("data", "intermediate", "unpacked", processing_year, form)
    files <- list.files(src_dir, pattern = "\\.(csv|dat)$", full.names = TRUE)
    if (length(files)) return(SOURCE_FORMAT_FROM_PATH(files[1]))
  }
  # Fallback by year if no path available yet
  if (processing_year <= 2017L) list(sep = " ", ext = "dat")
  else                          list(sep = ",", ext = "csv")
}

# ---- Expected source column counts per (processing_year, form) ----
# Populated from data/raw/soi_dictionaries/_var_matrix_{form}.csv during
# pre-checks. Placeholder structure here; pre_checks.R loads the actual counts.
# Pre-check tolerance: ±5% (per plan §7).

EXPECTED_COL_COUNT_TOLERANCE <- 0.05

# ---- Tax-year plausibility window ----
# Used by post-checks. tax_year extracted from first 4 chars of tax_period.

tax_year_range <- function() {
  # Lower bound 1985 (not 1989) accommodates legacy NCCS files that contain
  # late filers with TAXPER predating the file's publication year. The earliest
  # legitimate tax_year observed in the legacy bucket is 1987 (PF series); 1985
  # gives a small cushion for very-late filers without losing outlier signal
  # for genuinely corrupt dates.
  c(1985L, as.integer(format(Sys.Date(), "%Y")) + 1L)
}

# ---- Known subsection codes ----
# Loaded from data/lookups/subsection_codes.csv (built from IRM 25.7.1 Exhibit
# 25.7.1-4, the IRS-internal SS/CL code reference). The lookup is maintained
# independently from nccs-data-bmf's parallel copy; both should derive from the
# same IRM upstream. Cross-check periodically.

SUBSECTION_CODES_PATH <- "data/lookups/subsection_codes.csv"
KNOWN_SUBSECTION_CODES <- sort(unique(
  data.table::fread(SUBSECTION_CODES_PATH)$subsection_code
))
SUBSECTION_501C3 <- 3L

# ---- Quality report category groupings ----
# Drive the "Field Completeness by Category" section in the quality report.
# Categories derived from the `location` column in FINAL crosswalk.

REPORT_CATEGORIES <- list(
  "990"         = c("header", "part_iv_checklist", "part_v_other_filings",
                    "part_vii_compensation", "part_viii_revenue", "part_ix_expenses",
                    "part_x_balance_sheet", "sched_a_170", "sched_a_509"),
  "990ez"       = c("header", "part_i_revenue_expenses", "part_ii_balance",
                    "part_v_other", "sched_a"),
  "990pf"       = c("header", "part_i_revenue_expenses", "part_ii_balance_sheet",
                    "part_v_excise_tax", "part_vi_a_activities", "part_vi_b_form4720",
                    "part_xiii_priv_op", "part_xv_income_activities", "part_xvi_nce_org"),
  "990combined" = c("header", "financial_summary")
)
