# nccs-data-core — SOI-current pipeline implementation plan

**Status:** plan locked 2026-05-08. Subsequent edits recorded inline with date.

This plan covers the rebuild of `nccs-data-core` for the **current IRS SOI extracts (2012-present)**. The legacy NCCS pipeline (1989-2011) is a separate parallel build, scoped out below.

## 1. Decisions recap

All locked-in design decisions live in `~/.claude/.../memory/project_overhaul_goals.md`. Headlines:

- **Modeled after `UrbanInstitute/nccs-data-bmf`**: per-field transforms, phased orchestrator with checkpoints, quality gates as code, EC2 batch scripts.
- **Two pipelines, parallel**: `R/run_pipeline.R` (this plan) for SOI 2012+, `R/run_legacy_pipeline.R` (later) for raw legacy 1989-2011 PZ/PF only.
- **Output grain:** per-`(tax_year, form_type)`. No master/merger output. No geocoding.
- **Four series:** `990`, `990ez`, `990pf`, `990combined` (= 990 + 990-EZ stacked on the 53 shared harmonized columns).
- **File naming:** `core_{tax_year}_{form}.csv`.
- **Tax year:** `substr(tax_period, 1, 4)`, NOT calendar year of filing.
- **Source priority on overlap:** SOI wins from 2012+; legacy fills pre-2012.
- **Crosswalks done.** FINAL files in `data/crosswalks/`. BASELINE/OVERRIDES/FINAL workflow safeguards user edits.

## 2. Repo layout

```
nccs-data-core/
├── R/
│   ├── config.R                    # paths, S3 prefixes, IRS URL templates, expected schemas
│   ├── run_pipeline.R              # phased orchestrator with ENABLE_* flags
│   ├── 01_download.R               # fetch IRS SOI extract zips
│   ├── 02_unpack.R                 # unzip + parse (CSV 2013+, space-delim .dat 2012)
│   ├── 03_harmonize.R              # apply FINAL crosswalk per form; case-fold; partition by tax_year
│   ├── 04_derive_combined.R        # build 990combined by stacking 990 + 990ez on 53 shared cols
│   ├── 05_quality.R                # pre + post checks; build quality RDS report data
│   ├── 06_dictionary.R             # auto-generate per-output dictionary CSV from FINAL crosswalk + post stats
│   ├── 07_render_report.R          # render docs/quality_report_template.qmd per (tax_year, form)
│   ├── 08_upload.R                 # S3 sync to nccsdata/{raw,intermediate,processed,logs}/core/...
│   ├── checkpoints.R               # save/skip phase outputs
│   ├── data.R                      # constants: form list, expected col counts per vintage, IRS URL templates
│   ├── transforms/                 # per-concept pure functions
│   │   ├── tax_period.R            # TAXPER → tax_year + tax_month
│   │   ├── ein.R                   # zero-pad / validate
│   │   ├── subsection.R            # decode subseccd
│   │   ├── financial_amounts.R     # numeric coercion, NA handling
│   │   └── indicators.R            # _cd columns: Y/N/1/0 → logical
│   ├── quality/
│   │   ├── pre_checks.R            # source file integrity, expected col counts
│   │   └── post_checks.R           # output schema, row counts, null rates, value ranges
│   ├── utils.R                     # path helpers, get_files(), S3 helpers
│   ├── create_logger.R             # log4r config (existing)
│   ├── aws_s3_sync.R               # existing
│   └── convert_to_csv.R            # existing
├── scripts/
│   ├── setup_ec2.sh                # existing; bump R deps
│   ├── run_pipeline.sh             # batch entry point
│   ├── inventory_soi_dictionaries.R       # done (kept for re-runs)
│   ├── extract_soi_variables.R            # done
│   ├── build_soi_var_matrix.R             # done
│   ├── draft_990_crosswalk.R              # done
│   ├── draft_990ez_crosswalk.R            # done
│   └── draft_990pf_crosswalk.R            # done
├── data/
│   ├── crosswalks/                 # tracked: BASELINE/OVERRIDES/FINAL CSVs (never delete OVERRIDES without permission)
│   ├── raw/                        # gitignored
│   ├── intermediate/               # gitignored
│   ├── processed/                  # gitignored
│   └── logs/                       # gitignored
└── docs/
    ├── _quarto.yml                 # Quarto book config (rename for CORE)
    ├── index.qmd                   # book TOC + overview
    ├── 01-architecture.qmd
    ├── 02-data-lineage.qmd
    ├── 03-transforms-reference.qmd
    ├── 04-crosswalks.qmd
    ├── 05-quality-gates.qmd
    ├── 06-configuration.qmd
    ├── 07-developer-guide.qmd
    ├── 08-output-schema.qmd
    ├── 09-legacy-harmonization.qmd
    ├── 10-ec2-batch-processing.qmd
    └── quality_report_template.qmd # existing template, parameterized for per-(tax_year, form) rendering
```

### Files to delete from existing repo (flawed first pass)

- `R/00_data_download.R`
- `R/01_data_harmonize.R`
- `R/01_pre_process.R`
- `R/02_data_post-process.R`
- `R/03_data_merge-soi.R`
- `R/04_data_validate.R`
- `R/05_data-dictionary.R`
- `R/06_data_upload.R`
- `R/data_*_helpers.R` (all of them)
- `R/data-dictionary_helpers.R`
- `data/crosswalks/VARIABLE-NAME-CROSSWALK-V1.xlsx` (legacy artifact, replaced by FINAL crosswalks)
- `data/crosswalks/VAR-CROSSWALK-CORE-COMBINED.csv` (legacy artifact)

## 3. S3 layout

All paths under `s3://nccsdata/`:

```
raw/core/
  soi-extracts/{processing_year}/{form}/    # IRS zips, KEPT FOREVER
                                            # e.g., raw/core/soi-extracts/2024/990/24eoextract990.zip
  nccs-legacy/...                           # legacy NCCS files (later phase)

intermediate/core/
  unpacked/{processing_year}/{form}/        # post-unzip CSVs / .dat (still in IRS vintage layout)
  harmonized/{tax_year}/{form}/             # post-crosswalk, partitioned by TAX year

processed/core/{tax_year}/{form}/
  core_{tax_year}_{form}.csv                # final data
  core_{tax_year}_{form}_dictionary.csv     # per-output dictionary
  core_{tax_year}_{form}_quality.html       # rendered Quarto quality report

logs/core/{run_timestamp}/                  # quality RDS + per-step log files (audit trail)
```

**Partitioning rules:**
- `raw/` and `intermediate/unpacked/` use **processing year** (when IRS published the extract).
- `intermediate/harmonized/` and `processed/` use **tax year** (`substr(TAXPER, 1, 4)`).

## 4. Pipeline phases

Orchestrator: `R/run_pipeline.R` with flags `ENABLE_DOWNLOAD`, `ENABLE_UNPACK`, `ENABLE_HARMONIZE`, `ENABLE_COMBINED`, `ENABLE_QUALITY`, `ENABLE_DICTIONARY`, `ENABLE_RENDER_REPORT`, `STRICT_QUALITY_GATES`, `ENABLE_S3_UPLOAD`, plus per-tier upload toggles `ENABLE_UPLOAD_{RAW,INTERMEDIATE,PROCESSED,LOGS}`.

| Phase | Script | Reads | Writes |
|---|---|---|---|
| 1. Download | `01_download.R` | IRS URLs (`config.R`) | `raw/core/soi-extracts/{processing_year}/{form}/*.zip` |
| 2. Unpack | `02_unpack.R` | raw zips | `intermediate/core/unpacked/{processing_year}/{form}/*.csv \| *.dat` |
| 3. Pre-checks | `quality/pre_checks.R` | unpacked files | `data/logs/<step>_log.txt` |
| 4. Harmonize | `03_harmonize.R` | unpacked + FINAL crosswalks | `intermediate/core/harmonized/{tax_year}/{form}/core_{tax_year}_{form}.csv` |
| 5. Combined | `04_derive_combined.R` | harmonized 990 + 990ez | `intermediate/core/harmonized/{tax_year}/990combined/core_{tax_year}_990combined.csv` |
| 6. Post-checks | `05_quality.R` | harmonized | `data/logs/quality_{form}_{tax_year}.rds` |
| 7. Dictionary | `06_dictionary.R` | FINAL crosswalk + harmonized | `processed/core/{tax_year}/{form}/core_{tax_year}_{form}_dictionary.csv` |
| 8. Quality report | `07_render_report.R` | `quality_*.rds` + Quarto template | `processed/core/{tax_year}/{form}/core_{tax_year}_{form}_quality.html` |
| 9. Promote | (within harmonize tail) | harmonized data | `processed/core/{tax_year}/{form}/core_{tax_year}_{form}.csv` |
| 10. Upload | `08_upload.R` | all tiers | `s3://nccsdata/{raw,intermediate,processed,logs}/core/...` |

## 5. Per-form harmonization (`03_harmonize.R`)

### Source format quirks

- **2013+:** comma-delimited CSV with header. `data.table::fread()`.
- **2012:** space-delimited `.dat` with header. `data.table::fread(sep = " ")`.
- All forms across all years 2012-2024: lowercase ALL column names on read. Handles PF case rename + intra-2012 inconsistency in one move.

### Crosswalk apply

1. Load `data/crosswalks/soi_<form>_crosswalk_FINAL.csv`.
2. Build a lookup: `tolower(source_var) → harmonized_name`.
3. Rename source columns to harmonized names. Drop columns absent from crosswalk (warn if non-empty drop list — would indicate a new IRS column).
4. Add NA-filled placeholders for harmonized names absent from this vintage (e.g., `efile_indicator` won't exist in 2012-2014; the column should still exist with all NAs so the schema is stable across years).

### Tax-year partitioning

- Apply `R/transforms/tax_period.R`: derive `tax_year` and `tax_month` from `tax_period` (which holds the source `TAXPER` / `tax_pd` / `tax_prd` value, all mapped via crosswalk).
- Group rows by `tax_year`, write one CSV per `(tax_year, form)`.

### Type coercion (`R/transforms/`)

- `ein.R`: zero-pad to 9 chars, validate as numeric-castable.
- `tax_period.R`: parse `YYYYMM` (6 chars), keep as character; derive `tax_year` (int 4-digit), `tax_month` (int 1-12).
- `indicators.R`: normalize `_cd` columns. Map {Y, y, 1, T, TRUE} → TRUE; {N, n, 0, F, FALSE} → FALSE; everything else → NA. Use suffix lookup against crosswalk.
- `financial_amounts.R`: numeric coercion. Log row count of parse failures per column. Empty/whitespace → NA.
- `subsection.R`: integer coercion + validation against known subsection codes (1-92, plus a few special values).

## 6. `990combined` derivation (`04_derive_combined.R`)

1. Compute the 53-column shared schema:
   ```r
   shared <- intersect(crosswalk_990$harmonized_name, crosswalk_990ez$harmonized_name)
   ```
2. Read all `intermediate/core/harmonized/{tax_year}/{990,990ez}/core_*.csv`.
3. Project each to the 53 shared columns.
4. Add `source_form` column (`"990"` or `"990ez"`).
5. `data.table::rbindlist(use.names = TRUE, fill = TRUE)`.
6. Write per `tax_year` to `intermediate/core/harmonized/{tax_year}/990combined/`.

The `990combined` series gets the same dictionary + quality report treatment as the source forms.

## 7. Quality framework

### Pre-checks (`quality/pre_checks.R`)

Run before harmonization. Per (processing_year, form):

- File exists and non-empty.
- Header row present.
- Column count within ±5% of expected per-vintage value (table in `R/data.R`, sourced from `data/raw/soi_dictionaries/_var_matrix_{form}.csv`).
- Row count > 0.
- No duplicate header column names.

### Post-checks (`quality/post_checks.R`)

Run after harmonization. Per (tax_year, form):

- **Schema:** every harmonized column from FINAL crosswalk is present (with NAs where source absent in vintage).
- **Type:** numeric columns parse as numeric; logical columns binary; tax_year is 4-digit numeric in `[1989, current_year + 1]`.
- **EIN format:** 9 digits, no nulls, no duplicates.
- **TAXPER format:** 6 chars, parseable; tax_year in plausible range.
- **Subsection codes:** in known set.
- **Row-count plausibility:** within ±20% of prior-year row count for same `(tax_year, form)` (warn).
- **Null rates:** each column's null rate within historical bounds (warn).

`STRICT_QUALITY_GATES = TRUE` aborts on hard-failure checks (schema, EIN format, type); soft checks (row-count delta, null rate) always warn.

### Quality report data (`05_quality.R`)

Builds an R list mirroring the BMF report shape, saved to RDS at `data/logs/quality_{form}_{tax_year}.rds`. Required fields:

- `passed`, `timestamp`, `form`, `tax_year`
- `row_count`, `column_count`, `overall_completeness`, `row_preservation`
- `summary_stats$unique_eins`, `duplicate_eins`
- `column_completeness`: per-column `{completeness_pct, null_count, empty_count, source_columns}`
- `category_reports`: per-section breakdown (categories listed below per form)
- `source_column_map`, `column_descriptions` (from FINAL crosswalk)
- `summary_stats$subsection_distribution` (replaces BMF `org_type_distribution`)
- `summary_stats$financial`: per-form totals + medians of revenue, expenses, assets
- `summary_stats$tax_period_year_distribution`
- `summary_stats$year_over_year_delta`: row-count delta vs prior-vintage same `(tax_year, form)`
- `missing_columns`, `critical_field_issues`, `extra_columns`

**Category groupings** for the "Field Completeness by Category" section:

- `990`: header, part_iv_checklist, part_v_other_filings, part_vii_compensation, part_viii_revenue, part_ix_expenses, part_x_balance_sheet, sched_a_170, sched_a_509
- `990ez`: header, part_i_revenue_expenses, part_ii_balance, part_v_other, sched_a
- `990pf`: header, part_i_revenue_expenses, part_ii_balance_sheet, part_v_excise_tax, part_vi_a_activities, part_vi_b_form4720, part_xiii_priv_op, part_xv_income_activities, part_xvi_nce_org
- `990combined`: header, financial_summary

Categories derived from the `location` column in FINAL crosswalk (parse part number from "990 Core_Pt VIII-1A" etc.).

### Quality report rendering (`07_render_report.R`)

```r
quarto::quarto_render(
  input = "docs/quality_report_template.qmd",
  output_format = "html",
  execute_params = list(report_data_path = rds_path),
  output_file = sprintf("core_%s_%s_quality.html", tax_year, form)
)
```

Adapted template (one-time edit to `docs/quality_report_template.qmd`):
- Title becomes "CORE Pipeline Quality Report" with subtitle showing `{form} - tax year {tax_year}`.
- Replace BMF "Org Type Distribution" with "Subsection Code Distribution".
- Drop NTEE Major Group section (NTEE not in SOI extracts).
- Drop Address Quality section (no addresses in SOI).
- Replace Date Coverage section with "Tax Period Year Coverage" + "Year-over-Year Row Count" delta chart.

## 8. Data dictionary (`06_dictionary.R`)

Auto-generated per output. Columns:

| col | source |
|---|---|
| `harmonized_name` | FINAL crosswalk |
| `description` | FINAL crosswalk |
| `source_var` | FINAL crosswalk (synonyms joined with `\|`) |
| `source_location` | FINAL crosswalk |
| `data_type` | inferred from harmonized output |
| `n_rows` | output |
| `n_nonnull` | output |
| `null_pct` | derived |
| `n_distinct` | output |
| `min_value` | numeric only |
| `max_value` | numeric only |
| `years_present` | FINAL crosswalk |

Output: `processed/core/{tax_year}/{form}/core_{tax_year}_{form}_dictionary.csv`.

## 9. Quarto book (parallel build)

Update `docs/_quarto.yml`:
- `title: "CORE Data Pipeline Guide"`
- `output-file: "core-pipeline-guide"`
- Chapter list as in section 2 above.

Scaffold all chapters as empty `.qmd` stubs at start of pipeline build. Fill incrementally as features land. Stub each chapter with a one-line purpose description and a `TODO` callout — keeps the book renderable from day 1.

Drop BMF chapters that don't apply: `04-dimension-tables.qmd` (no dimensions in CORE), `08-lookup-tables.qmd` (no lookups), `11-master-bmf.qmd` (no master per decision).

Add CORE-specific chapters: `04-crosswalks.qmd` (BASELINE/OVERRIDES/FINAL workflow), `08-output-schema.qmd` (auto-generated harmonized column reference per form).

## 10. EC2 batch orchestration

### `scripts/setup_ec2.sh`

Bump from existing version: ensure `assertr`, `data.validator`, `quarto`, `jsonlite`, `duckdb`, `DBI` installed. Verify `aws` CLI and `pdftotext` (for one-off form re-fetches).

### `scripts/run_pipeline.sh`

```bash
#!/usr/bin/env bash
# Args: --years 2012-2024 --forms 990,990ez,990pf [--no-upload] [--strict]
Rscript R/run_pipeline.R "$@"
```

### IAM

EC2 role grants:
- `s3:GetObject` on `gt990datalake-rawdata` (BMF inputs, used downstream).
- `s3:GetObject`, `s3:PutObject`, `s3:ListBucket` on `nccsdata`.

## 11. Developer workflow

- **Local quick iteration:** `Rscript R/run_pipeline.R --years 2024 --forms 990ez --no-upload`. 990-EZ is the smallest extract.
- **Crosswalk edit:** edit `data/crosswalks/soi_<form>_crosswalk_OVERRIDES.csv` in place, save. Re-run `scripts/draft_<form>_crosswalk.R` to regenerate FINAL. **Never delete OVERRIDES without explicit user permission.**
- **New tax year roll-out:** bump `LATEST_YEAR` in `R/config.R`, run pipeline. Pre/post checks flag schema drift.
- **Re-render quality report from existing RDS:** `Rscript R/07_render_report.R --rds data/logs/quality_990_2023.rds` (no need to re-harmonize).
- **Skip phases:** set `ENABLE_DOWNLOAD = FALSE` (and others) in env or args to re-use existing intermediate files.

## 12. Out of scope (decisions already made)

- **Legacy pipeline (1989-2011 PZ/PF):** parallel `R/run_legacy_pipeline.R` to be built after this pipeline ships. Will need raw-legacy column inventory at that time.
- **Master / merger across years:** not building (decision #10).
- **Geocoding:** not in CORE (decision #11). Address data is not in SOI extracts; downstream consumers join CORE×BMF for geo.
- **2017–2019 990-PF:** permanent gap, no imputation.
- **990-EZ historical (pre-2012):** unrecoverable from any source; the `990ez` series is current-only.

## 13. Build phases (suggested ordering)

1. **Scaffold + config:** create `R/config.R`, `R/data.R`, empty Quarto stubs, delete old scripts.
2. **Download + unpack:** `01_download.R`, `02_unpack.R`. End-to-end smoke test for one (year, form).
3. **Transforms:** flesh out `R/transforms/`. Unit-test against a small sample CSV.
4. **Harmonize:** `03_harmonize.R`. Verify output schema matches FINAL crosswalk for each form.
5. **Combined derivation:** `04_derive_combined.R`. Sanity-check stack on one tax year.
6. **Quality framework:** pre-checks, post-checks, RDS report data.
7. **Dictionary + render:** `06_dictionary.R`, `07_render_report.R`. Adapt Quarto template.
8. **Upload:** `08_upload.R`. Per-tier `ENABLE_UPLOAD_*` flags.
9. **Orchestrator:** `R/run_pipeline.R` ties it all together with checkpoint + flag logic.
10. **EC2 wiring:** `scripts/run_pipeline.sh` + IAM.
11. **Documentation pass:** fill Quarto book chapters.
