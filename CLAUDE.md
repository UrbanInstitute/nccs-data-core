# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Purpose

`nccs-data-core` produces NCCS's CORE Series — harmonized panels of select Form 990 / 990EZ / 990PF fields — by merging two upstream sources:

1. NCCS's legacy CORE files (hosted on `s3://nccsdata/legacy/core/`)
2. The IRS SOI Tax-Exempt Organization extracts (sourced via the Giving Tuesday data lake at `s3://gt990datalake-rawdata/EfileData/Extracts/Data`)

The output is one CSV per `(tax_year, scope)` pair following `CORE-{YEAR}-{SCOPE}-HRMN-{Version}.csv`, where SCOPE is one of:

- `501C3-CHARITIES-PC` — fields common to 990 + 990EZ for 501(c)(3)s
- `501C3-CHARITIES-PZ` — full 990 fields for 501(c)(3)s
- `501CE-NONPROFIT-PC` — fields common to 990 + 990EZ for non-501(c)(3)s
- `501CE-NONPROFIT-PZ` — full 990 fields for non-501(c)(3)s
- `501C3-PRIVFOUND-PF` — full 990PF fields for private foundations

Outputs are partitioned by **tax year** (the first 4 characters of TAXPER), not the year the form was filed — this is a deliberate departure from the upstream NCCS/SOI files.

## Pipeline structure

The pipeline is a sequence of R scripts in `R/` numbered `00_` through `06_`. Run them in numerical order from the repo root; each script reads from / writes to fixed subpaths under `data/`:

| Script | Role | Reads | Writes |
|---|---|---|---|
| `00_data_download.R` | ETL raw SOI + legacy CORE; convert xlsx → csv | S3 buckets above | `data/raw/{soi,core}/` |
| `01_pre_process.R` | Pre-harmonization checks against the crosswalk | `data/raw/`, crosswalks | (checks only) |
| `01_data_harmonize.R` | Apply crosswalk to rename columns; partition by scope | `data/raw/`, `data/crosswalks/` | `data/harmonized/{soi,core}/{pc,ez,pz,pf}/` |
| `02_data_post-process.R` | Add derived columns to harmonized files | `data/harmonized/` | `data/processed/core/{scope}/` |
| `03_data_merge-soi.R` | Join harmonized CORE with SOI extracts using the unified BMF | `data/processed/`, `BMF_UNIFIED_V1.1.csv` from S3 | `data/processed/core/{scope}/` |
| `04_data_validate.R` | `data.validator` / `assertr` schema + value checks | `data/processed/` | `data/logs/`, validation reports |
| `05_data-dictionary.R` | Generate per-scope data dictionaries | `data/processed/` | dictionary outputs |
| `06_data_upload.R` | Upload finalized CSVs to S3 | `data/processed/` | `s3://nccsdata/...` |

Each numbered script sources matching helpers (`data_<step>_helpers.R`) plus shared modules: `utils.R` (S3 + path helpers), `data.R` (constants like `GT_SOI_FOLDER`), `create_logger.R` (log4r setup, logs go to `data/logs/`), `aws_s3_sync.R`, `convert_to_csv.R`. Use `here::here("R", "...")` or root-relative `"R/..."` when sourcing — scripts assume the repo root is the working directory.

## Crosswalks

`data/crosswalks/VARIABLE-NAME-CROSSWALK-V1.xlsx` is the canonical column-rename mapping (legacy CORE + SOI old names → harmonized names). `VAR-CROSSWALK-CORE-COMBINED.csv` is its derived/exported form. Harmonization is driven entirely off this crosswalk; new variables must be added there, not hardcoded in scripts.

## Running

There is no build/test harness — this is an R data pipeline. To run a step from the repo root:

```bash
Rscript R/01_data_harmonize.R
```

Required R packages (see `scripts/setup_ec2.sh` for the EC2 bootstrap): `data.table`, `arrow`, `aws.s3`, `paws`, `openxlsx`, `rio`, `here`, `purrr`, `stringr`, `lubridate`, `jsonlite`, `quarto`, `duckdb`, `DBI`, `log4r`, `tidyverse`, `data.validator`, `assertr`. AWS credentials must be available (IAM role on EC2, or `aws configure` / `AWS_*` env vars) for any script that touches S3 — most of them do.

> **Note on `scripts/run_master.sh` and `scripts/run_all_legacy.sh`:** these reference `R/run_master_pipeline.R`, `R/run_legacy_pipeline.R`, and `R/config.R`, which do not exist in this repo. They appear to belong to the sibling `nccs-data-bmf` pipeline (the URL inside `setup_ec2.sh` points there explicitly). Do not assume they run in this repo without first checking that the referenced R files exist.

## Conventions worth knowing

- File-path discovery is done with `get_files(folder_name, scope)` (defined in `R/utils.R`); call sites pass the scope tag (`PC`, `EZ`, `PZ`, `501C3-CHARITIES-PC`, etc.) as a filter, so renames must keep that token in the filename.
- Logging is routed through `create_logger("data/logs/<step>_log.txt")`. Don't `print()` or `message()` for pipeline-level events — use the returned logger so output lands in the per-step log file.
- `data/raw/`, `data/harmonized/`, `data/processed/`, `data/logs/` are gitignored (large CSVs); only `data/crosswalks/` is tracked.
- Tax-year partitioning is based on the first 4 chars of TAXPER, not the calendar year a file was published. Don't conflate the two when reading or writing.
