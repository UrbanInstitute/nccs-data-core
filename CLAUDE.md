# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Purpose

`nccs-data-core` produces NCCS's CORE Series — harmonized panels of select Form 990 / 990-EZ / 990-PF fields. Two upstream sources are stitched together:

1. **IRS SOI Tax-Exempt Organization annual extracts** (2012+, downloaded from `irs.gov/pub/irs-soi/`).
2. **NCCS's legacy CORE files** (1989–2011, hosted at `s3://nccsdata/legacy/core/`).

Output is one CSV per `(tax_year, form)` named `core_{year}_{form}.csv`, where `form` is `990`, `990ez`, `990pf`, or `990combined` (the stacked 990 + 990-EZ panel on shared columns). Outputs are partitioned by **tax year** (the first 4 characters of `TAXPER`), not the year the form was filed — a deliberate departure from the upstream NCCS/SOI files.

The published output lives in three S3 tiers under `s3://nccsdata/`:

| Tier | Prefix | What it is |
|---|---|---|
| `processed/core/` | SOI-current panel | 2012+, direct from IRS SOI extracts |
| `processed_legacy/core/` | Legacy panel | 1989–2011, from raw NCCS legacy files (mostly intermediate to the merge) |
| `processed_merged/core/` | **Merged panel** | Legacy ∪ SOI-current via column-merge with SOI precedence — the analyst-facing canonical artifact spanning 1989–present |

## Pipeline structure

The codebase has **three orchestrators** under `R/`, each runnable from the repo root. Each invokes a sequence of numbered phase scripts (also under `R/`) that are individually runnable for debugging.

| Orchestrator | Tier produced | Phases used |
|---|---|---|
| `R/run_pipeline.R` | SOI-current → `data/processed/`, `s3://.../processed/core/` | 1 download · 2 unpack · 3 harmonize · 4 derive_combined · 5 quality · 6 dictionary · 7 render · 8 upload · 9 parquet |
| `R/run_legacy_pipeline.R` | Legacy → `data/processed_legacy/`, `s3://.../processed_legacy/core/` | 1 legacy_download · 3 legacy_harmonize · 5 quality · 6 dictionary · 7 render · 8 upload · 9 parquet (no phase 2/4) |
| `R/run_build_panel.R` | Merged → `data/processed_merged/`, `s3://.../processed_merged/core/` | 4 legacy_merge · 5 quality · 6 dictionary · 7 render · 8 upload (depends on outputs from both upstream pipelines) |

Phase scripts:

| Script | Role |
|---|---|
| `01_download.R` / `01_legacy_download.R` | Fetch IRS SOI zips / mirror raw legacy CSVs from S3 |
| `02_unpack.R` | Unzip SOI extracts (SOI-current only) |
| `03_harmonize.R` / `03_legacy_harmonize.R` | Apply crosswalk: rename source cols → harmonized names, coalesce synonyms, NA-pad vintage gaps, apply type-specific transforms, partition by `tax_year` |
| `04_derive_combined.R` | Stack 990 + 990-EZ on 53 shared columns → `990combined` |
| `04_legacy_merge.R` | Column-merge legacy ∪ SOI-current on `(ein, tax_period)` with SOI precedence; emits a per-(year, form) disagreement audit CSV |
| `05_quality.R` | Per-(form, tax_year) post-checks (schema, EIN format, tax_period range, type validation, YoY tripwire); writes RDS to `logs_dir` (parametrized — see "Per-pipeline RDS isolation" below) |
| `06_dictionary.R` | Generate per-output data dictionary CSV |
| `07_render_report.R` | Render Quarto quality reports per (form, tax_year) to HTML |
| `08_upload.R` | Promote harmonized → processed/ then `aws s3 sync` per tier; one of `run_upload()` / `run_upload_legacy()` / `run_upload_merged()` per orchestrator |
| `09_parquet.R` | Write `.parquet` alongside `.csv` (shared by SOI-current + legacy) |

Shared modules: `R/config.R` (paths, S3 prefixes, IRS URL templates, CONFIG flags), `R/data.R` (form/scope constants, crosswalk path lookups), `R/create_logger.R` (log4r setup), `R/aws_s3_sync.R` (CLI wrapper), `R/utils.R`, `R/transforms/` (six pure column transforms), `R/quality/` (pre/post-check validators).

## Per-pipeline RDS isolation

Phase 5 writes `quality_{form}_{tax_year}.rds`, with no pipeline tag in the filename. Each orchestrator passes its own `logs_dir` to `run_quality()` to avoid clobbering at shared `(form, tax_year)` keys (e.g. `990combined/2011` exists in all three pipelines):

| Orchestrator | `logs_dir` | HTML `reports_root` |
|---|---|---|
| `run_pipeline.R` | `data/logs/` | `docs/quality-reports/` |
| `run_legacy_pipeline.R` | `data/logs/legacy/` | `docs/quality-reports/legacy/` |
| `run_build_panel.R` | `data/logs/merged/` | `docs/quality-reports/merged/` |

The legacy/merged subdirs auto-sync to S3 logs via the existing recursive `aws s3 sync data/logs/` in the SOI-current upload phase.

## Crosswalks

Crosswalks under `data/crosswalks/` drive every column rename. Each follows a BASELINE / OVERRIDES / FINAL split:

- `soi_990_crosswalk_{BASELINE,OVERRIDES,FINAL}.csv` — SOI 990 extract → harmonized names
- `soi_990ez_crosswalk_{...}.csv` — SOI 990-EZ → harmonized names
- `soi_990pf_crosswalk_{...}.csv` — SOI 990-PF → harmonized names
- `legacy_pz_crosswalk_{...}.csv` — legacy PZ (501C3-CHARITIES + 501CE-NONPROFIT) → harmonized names (authored against SOI vocabulary so the panels stack at the 2011/2012 boundary)
- `legacy_pf_crosswalk_{...}.csv` — legacy PRIVFOUND-PF → harmonized names

The FINAL file is what harmonize reads. New variables must be added to BASELINE (regenerable) or OVERRIDES (user-edited); never edit FINAL directly — generator scripts will overwrite it. See `docs/04-crosswalks.qmd` for the workflow.

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

# Test harness (209 tests across seven test files):
Rscript tests/run_all.R                                     # exits nonzero on failure
Rscript tests/test_legacy_merge.R                           # individual file
```

Required R packages (see `scripts/setup_ec2.sh` for the EC2 bootstrap): `data.table`, `arrow`, `aws.s3`, `paws`, `openxlsx`, `rio`, `here`, `purrr`, `stringr`, `lubridate`, `jsonlite`, `quarto`, `duckdb`, `DBI`, `log4r`, `tidyverse`, `data.validator`, `assertr`. AWS credentials must be available (IAM role on EC2, `aws configure`, `AWS_PROFILE`, or `AWS_*` env vars) for any script that touches S3 — every upload phase + the legacy download do.

For cron / EC2 entry points, use `bash scripts/run_pipeline.sh [flags]` — a thin `--vanilla` wrapper that tees a timestamped console log to `data/logs/` and propagates the R exit code.

## Conventions worth knowing

- **Tax-year partitioning** is based on the first 4 chars of `TAXPER`, not the calendar year a file was published. A 2011-NCCS-published legacy file may contain rows with `TAXPER` from 1987 to 1992; a 2012 SOI extract contains rows with `TAXPER` going back into the 1990s. Don't conflate publication year with tax year.
- **Logging** is routed through `create_logger("<logs_dir>/<step>_log.txt")`. Don't `print()` or `message()` for pipeline-level events — use the returned logger so output lands in the per-step log file. Per-pipeline `logs_dir` keeps SOI-current, legacy, and merged logs separate.
- **Gitignored**: `data/raw/`, `data/intermediate/`, `data/processed/`, `data/processed_legacy/`, `data/processed_merged/`, `data/logs/`. Tracked: `data/crosswalks/`, `data/lookups/`, `docs/quality-reports/` (incl. `legacy/` and `merged/` subdirs — served by GitHub Pages).
- **Three S3 tiers, three local processed dirs.** Don't sync the wrong tier — each orchestrator has its own `run_upload_*()` function targeting its own S3 prefix.
- **Crosswalks must be double-checked.** The algorithmic name-matcher is a starting point — every entry should be verified against the source dictionary before shipping. Past incidents traced to relying on Jaccard scores alone (see `data/crosswalks/legacy_pz_crosswalk_FINAL.csv` for examples where the algorithm flagged things below threshold but the human-authored mapping is correct).

## Triaging pipeline run output

When the user pastes `run_pipeline` console output, log4r logs, or other multi-phase pipeline traces, treat triage as a deliberate, verify-as-you-go task — not a fast skim. Logs here are long and easy to misread.

- **Read the log at least twice before concluding.** First pass: inventory phases, hard/soft failures, warning counts. Second pass: look for what the first pass missed — silent mismatches that still `passed=TRUE`, off-by-ones between phases, framings that overreach (one bad year ≠ regime change).
- **Distinguish facts from inferences.** A fact is a literal log line. An inference is anything derived ("column X is empty", "Y is a typo of Z", "the dip starts in 2018"). Before reporting an inference as a finding: verify it (read the source file, grep the crosswalk, query the harmonized output, inspect the var matrix). If you can't verify, label it explicitly as inference and say what would confirm it.
- **Quote exact numbers.** Paraphrased counts drift; the user is reading the same log.
- **Don't propose fixes for unverified findings.** If the conclusion hasn't been verified against code or data, the fix it implies hasn't either.
- **Consult `docs/log-triage-gotchas.md` before drawing conclusions** — it lists known log-shape quirks in this pipeline that look like bugs but aren't.
- **Maintain `docs/log-triage-gotchas.md` as you go.** If triage surfaces a new log-shape quirk worth remembering, or an existing entry becomes stale (pipeline change, schema change, fixed quirk), update the gotchas doc in the same turn — don't defer.
