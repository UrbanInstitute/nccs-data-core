# R/config.R
# Central configuration for the SOI-current CORE pipeline.
# Paths, S3 prefixes, IRS URL templates, run-time flags.
# Per-vintage expected schemas live in R/data.R.

# ---- Run-time flags (overridable via env vars or args parsed in run_pipeline.R) ----

CONFIG <- list(

  # Phase toggles
  ENABLE_DOWNLOAD          = TRUE,
  ENABLE_UNPACK            = TRUE,
  ENABLE_HARMONIZE         = TRUE,
  ENABLE_COMBINED          = TRUE,
  ENABLE_QUALITY           = TRUE,
  ENABLE_DICTIONARY        = TRUE,
  ENABLE_RENDER_REPORT     = TRUE,
  ENABLE_PARQUET           = FALSE,  # write .parquet next to processed .csv for API/R-package consumption
  ENABLE_S3_UPLOAD         = FALSE,

  # Per-tier upload toggles
  ENABLE_UPLOAD_RAW          = FALSE,
  ENABLE_UPLOAD_FORMS        = TRUE,  # data/raw/forms/ — small, archival, slow-changing
  ENABLE_UPLOAD_INTERMEDIATE = FALSE,
  ENABLE_UPLOAD_PROCESSED    = TRUE,
  ENABLE_UPLOAD_LOGS         = TRUE,

  # When TRUE, *.html files in processed/ are gzip-compressed before upload
  # and tagged with Content-Encoding: gzip so browsers decompress
  # transparently. Cuts transfer / storage cost ~5-10x on the embed-resources
  # quality reports. Trade-off: raw downloads via the S3 console / aws s3 cp
  # return compressed bytes that the user must `gunzip` manually. Non-HTML
  # files in processed/ (data CSVs, dictionaries) are unaffected.
  ENABLE_GZIP_HTML_UPLOAD = TRUE,

  # Quality gate behavior
  STRICT_QUALITY_GATES = TRUE,

  # Checkpoint behavior
  ENABLE_CHECKPOINTS = TRUE,

  # Year window
  EARLIEST_YEAR = 2012L,
  LATEST_YEAR   = 2024L,

  # Forms to process
  FORMS = c("990", "990ez", "990pf")
)

# ---- Local paths (relative to repo root) ----

PATHS <- list(
  data              = "data",
  raw               = "data/raw",
  soi_extracts      = "data/raw/soi_extracts",
  soi_dictionaries  = "data/raw/soi_dictionaries",
  forms             = "data/raw/forms",
  legacy_raw        = "data/raw/legacy/core",        # mirror of s3://nccsdata/legacy/core/, pre-2012 PZ + PF only
  intermediate      = "data/intermediate",
  unpacked          = "data/intermediate/unpacked",
  harmonized        = "data/intermediate/harmonized",
  processed         = "data/processed",
  logs              = "data/logs",
  crosswalks        = "data/crosswalks",
  docs              = "docs"
)

# ---- S3 layout (under s3://nccsdata/) ----

S3 <- list(
  bucket             = "nccsdata",
  raw_prefix         = "raw/core/soi-extracts",      # {processing_year}/{form}/*.zip
  dictionaries_prefix= "raw/core/soi-dictionaries",  # SOI dictionary xls(x) + var-matrix CSVs (small, archival)
  forms_prefix       = "raw/core/forms",             # IRS form PDFs + text extractions (current + historical)
  legacy_prefix      = "legacy/core",                # raw legacy NCCS files (read-only, used by run_legacy_pipeline.R)
  unpacked_prefix    = "intermediate/core/unpacked", # {processing_year}/{form}/
  harmonized_prefix  = "intermediate/core/harmonized", # {tax_year}/{form}/
  processed_prefix   = "processed/core",             # {tax_year}/{form}/
  logs_prefix        = "logs/core"                   # {run_timestamp}/
)

# ---- IRS SOI extract URL templates ----
# IRS naming is irregular across years. Observed patterns from the SOI annual
# extract landing page (scraped 2026-05-11):
#   2012-2017: {YY}eofinextract{form_tag}.zip
#   2018+    : {YY}eoextract{form_tag}.zip
# Form tag for 990-EZ also drifts: "990ez" / "EZ" / "ez" / "990EZ".
# Form tag for 990-PF: gap for 2017-2019 (no PF extract published those years).
# 2012 zips contain a single space-delimited .dat. 2013+ zips: comma-delimited .csv.

IRS_SOI <- list(base_url = "https://www.irs.gov/pub/irs-soi")

# Per-year, per-form filename stem (no .zip extension). NA = not published.
SOI_FILENAME_STEMS <- list(
  "990"   = c("2012" = "12eofinextract990",   "2013" = "13eofinextract990",
              "2014" = "14eofinextract990",   "2015" = "15eofinextract990",
              "2016" = "16eofinextract990",   "2017" = "17eofinextract990",
              "2018" = "18eoextract990",      "2019" = "19eoextract990",
              "2020" = "20eoextract990",      "2021" = "21eoextract990",
              "2022" = "22eoextract990",      "2023" = "23eoextract990",
              "2024" = "24eoextract990"),
  "990ez" = c("2012" = "12eofinextract990ez", "2013" = "13eofinextract990ez",
              "2014" = "14eofinextract990ez", "2015" = "15eofinextractEZ",
              "2016" = "16eofinextractez",    "2017" = "17eofinextractEZ",
              "2018" = "18eoextractez",       "2019" = "19eoextractez",
              "2020" = "20eoextractez",       "2021" = "21eoextractez",
              "2022" = "22eoextractez",       "2023" = "23eoextractez",
              "2024" = "24eoextract990EZ"),
  "990pf" = c("2012" = "12eofinextract990pf", "2013" = "13eofinextract990pf",
              "2014" = "14eofinextract990pf", "2015" = "15eofinextract990pf",
              "2016" = "16eofinextract990pf", "2017" = NA,
              "2018" = NA,                    "2019" = NA,
              "2020" = "20eoextract990pf",    "2021" = "21eoextract990pf",
              "2022" = "22eoextract990pf",    "2023" = "23eoextract990pf",
              "2024" = "24eoextract990pf")
)

build_soi_url <- function(processing_year, form) {
  stopifnot(form %in% names(SOI_FILENAME_STEMS))
  stem <- SOI_FILENAME_STEMS[[form]][as.character(processing_year)]
  if (is.na(stem) || is.null(stem) || length(stem) == 0L) return(NA_character_)
  sprintf("%s/%s.zip", IRS_SOI$base_url, stem)
}
