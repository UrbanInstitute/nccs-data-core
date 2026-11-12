# Pipeline reference

Task-specific details moved from the agent entry point. Read `AGENTS.md`
for repository rules and the linked topic chapters for implementation detail.

## Pipeline structure

The codebase has **three orchestrators** under `R/`, each runnable from the repo root. Each invokes a sequence of numbered phase scripts (also under `R/`) that are individually runnable for debugging.

| Orchestrator | Tier produced | Phases used |
|---|---|---|
| `R/run_pipeline.R` | SOI-current → `data/processed/`, `s3://.../processed/core/` | 1 download · 2 unpack · 3 harmonize · 4 derive_combined · 5 quality · 6 dictionary · 7 render · 7.5 promote · 9 parquet · 8 upload |
| `R/run_legacy_pipeline.R` | Legacy → `data/processed_legacy/`, `s3://.../processed_legacy/core/` | 1 legacy_download · 3 legacy_harmonize · 5 quality · 6 dictionary · 7 render · 7.5 promote · 9 parquet · 8 upload (no phase 2/4) |
| `R/run_build_panel.R` | Merged → `data/processed_merged/`, `s3://.../processed_merged/core/` | 4 legacy_merge · 5 quality · 6 dictionary · 7 render · 7.5 promote · 9 parquet · 8 upload (depends on outputs from both upstream pipelines) |

Phase scripts:

| Script | Role |
|---|---|
| `01_download.R` / `01_legacy_download.R` | Fetch IRS SOI zips / mirror raw legacy CSVs from S3 |
| `02_unpack.R` | Unzip SOI extracts (SOI-current only) |
| `03_harmonize.R` / `03_legacy_harmonize.R` | Apply crosswalk: rename source cols → harmonized names, coalesce synonyms, NA-pad vintage gaps, apply type-specific transforms, partition by `tax_year` |
| `04_derive_combined.R` | Stack 990 + 990-EZ on shared columns → `990combined` |
| `04_legacy_merge.R` | Column-merge legacy ∪ SOI-current on `(ein, tax_period)` with SOI precedence; emits a per-(year, form) disagreement audit CSV |
| `05_quality.R` | Per-(form, tax_year) post-checks (schema, EIN format, tax_period range, type validation, YoY tripwire); writes RDS to `logs_dir` (parametrized, see "Per-pipeline RDS isolation" below) |
| `06_dictionary.R` | Generate per-output data dictionary CSV |
| `07_render_report.R` | Render Quarto quality reports per (form, tax_year) to HTML |
| `08_upload.R` | Two roles: phase 7.5 `promote_harmonized_to_processed()` copies harmonized CSVs into the `processed*/` tier so phase 9 can see them (preserves mtime to keep phase-6 dictionaries non-stale); phase 8 `run_upload*()` runs `aws s3 sync` per tier (`run_upload()` / `run_upload_legacy()` / `run_upload_merged()`). |
| `09_parquet.R` | Write `.parquet` alongside each promoted `.csv` (runs after 7.5 promote in all three orchestrators). |

Shared modules: `R/config.R` (paths, S3 prefixes, IRS URL templates, CONFIG flags), `R/data.R` (form/scope constants, crosswalk path lookups), `R/create_logger.R` (log4r setup), `R/aws_s3_sync.R` (CLI wrapper), `R/utils.R`, `R/transforms/` (column transforms), `R/quality/` (pre/post-check validators).


## Per-pipeline RDS isolation

Phase 5 writes `quality_{form}_{tax_year}.rds`, with no pipeline tag in the filename. Each orchestrator passes its own `logs_dir` to `run_quality()` to avoid clobbering at shared `(form, tax_year)` keys (e.g. `990combined/2011` exists in all three pipelines):

| Orchestrator | `logs_dir` | HTML `reports_root` |
|---|---|---|
| `run_pipeline.R` | `data/logs/` | `docs/quality-reports/` |
| `run_legacy_pipeline.R` | `data/logs/legacy/` | `docs/quality-reports/legacy/` |
| `run_build_panel.R` | `data/logs/merged/` | `docs/quality-reports/merged/` |

The legacy/merged subdirs auto-sync to S3 logs via the existing recursive `aws s3 sync data/logs/` in the SOI-current upload phase.


## Running

This is an R data pipeline (no build artifact, but there is a test harness):

```bash
# Run a full pipeline from repo root:
Rscript R/run_pipeline.R                                    # SOI-current
Rscript R/run_legacy_pipeline.R                             # legacy 1989-2011
Rscript R/run_build_panel.R                                 # merge + publish merged panel

# CLI flags for any orchestrator (per-phase skip):
Rscript R/run_pipeline.R --no-download --no-upload          # re-use existing local mirror
Rscript R/run_build_panel.R --no-merge --no-quality         # just dictionary + render + upload

# Test harness (see tests/run_all.R for the current file list):
Rscript tests/run_all.R                                     # exits nonzero on failure
Rscript tests/test_legacy_merge.R                           # individual file
```

Required R packages (see `scripts/setup_ec2.sh` for the EC2 bootstrap): `data.table`, `arrow`, `openxlsx`, `rio`, `here`, `purrr`, `stringr`, `lubridate`, `jsonlite`, `quarto`, `duckdb`, `DBI`, `log4r`, `tidyverse`, `data.validator`, `assertr`. S3 operations go through the AWS CLI (not an R-side SDK like paws/aws.s3), so AWS credentials must be available to the shell: IAM role on EC2, `aws configure`, `AWS_PROFILE`, or `AWS_*` env vars.

For cron / EC2 entry points, use `bash scripts/run_pipeline.sh [flags]`, a thin `--vanilla` wrapper that tees a timestamped console log to `data/logs/` and propagates the R exit code.
