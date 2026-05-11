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

# ---- Source-format quirks by processing year ----
# 2012 extract: space-delimited .dat files with header.
# 2013+ extracts: comma-delimited CSV with header.

SOURCE_FORMAT <- function(processing_year) {
  if (processing_year <= 2012L) {
    list(sep = " ", ext = "dat")
  } else {
    list(sep = ",", ext = "csv")
  }
}

# ---- Expected source column counts per (processing_year, form) ----
# Populated from data/raw/soi_dictionaries/_var_matrix_{form}.csv during
# pre-checks. Placeholder structure here; pre_checks.R loads the actual counts.
# Pre-check tolerance: ±5% (per plan §7).

EXPECTED_COL_COUNT_TOLERANCE <- 0.05

# ---- Tax-year plausibility window ----
# Used by post-checks. tax_year extracted from first 4 chars of tax_period.

tax_year_range <- function() {
  c(1989L, as.integer(format(Sys.Date(), "%Y")) + 1L)
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
