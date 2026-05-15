# nccs-data-core

`nccs-data-core` produces NCCS's CORE Series: harmonized panels of Form 990, 990-EZ, and 990-PF fields, built from the IRS Statistics of Income (SOI) annual extracts (2012-present).

## Outputs

Per `(tax_year, form)` CSV plus a per-output data dictionary and a per-output quality report:

- `990` — full 990 schedule, 990 filers only
- `990ez` — full 990-EZ schedule, 990-EZ filers only
- `990pf` — full 990-PF schedule, private foundations + §4947(a)(1) trusts treated as private foundations
- `990combined` — 990 + 990-EZ stacked on their 53 shared harmonized columns (a `source_form` column preserves provenance)

`tax_year` is the calendar year the fiscal period **ended**, derived from the first 4 chars of the IRS extract's `TAXPER` field — not the year the form was filed.

File naming: `core_{tax_year}_{form}.csv`. The companion dictionary and quality report use the same stem with `_dictionary.csv` / `_quality.html` suffixes.

## Quickstart

```r
# From the repo root in an R session:
setwd("/path/to/nccs-data-core")
source("R/run_pipeline.R")
run_pipeline(
  processing_years = 2012:2024,
  forms            = c("990", "990ez", "990pf"),
  dry_run          = FALSE
)
```

Or from the shell:

```bash
# Direct: same flags as the R entry point
Rscript R/run_pipeline.R --years 2012-2024 --forms 990,990ez,990pf --strict

# Wrapper: hermetic --vanilla invocation + timestamped console log, designed
# for cron / EC2 entry points. Flags are forwarded verbatim.
bash scripts/run_pipeline.sh --years 2012-2024 --forms 990,990ez,990pf --strict
```

CLI flags: `--years`, `--forms`, `--strict` / `--no-strict`, `--upload` / `--no-upload`, plus `--no-{download,unpack,harmonize,combined,quality,dictionary,render}` to skip individual phases. See `R/run_pipeline.R` for the full list.

Env-var knobs (read at runtime; useful for tuning cron without code changes):

| Variable | Meaning | Default |
|---|---|---|
| `NCCS_RENDER_WORKERS` | Worker count for parallel Quarto rendering in phase 7. | `detectCores() - 1` (uncapped; set the env var to throttle on memory-constrained hosts) |

## Pipeline structure

Nine phases, each as a standalone script under `R/`, all wired together by `R/run_pipeline.R`:

| Phase | Script | What it does |
|---|---|---|
| 1 | `01_download.R` | Fetches IRS SOI extract zips. Idempotent (skips files already on disk). |
| 2 | `02_unpack.R` | Unzips into `data/intermediate/unpacked/{processing_year}/{form}/`. |
| 2.5 | `quality/pre_checks.R` | File-level validation (header present, col count within ±5% of the IRS dictionary's per-vintage expected, no duplicate headers). |
| 3 | `03_harmonize.R` | Applies the FINAL crosswalk per form: lowercases headers, renames source vars to harmonized names, coalesces synonyms, NA-pads vintage gaps, applies type-specific transforms, partitions by `tax_year`. |
| 4 | `04_derive_combined.R` | Stacks 990 + 990-EZ on the 53 shared harmonized columns → `990combined`. |
| 5 | `05_quality.R` + `quality/{pre,post}_checks.R` | Post-harmonization checks: schema, EIN format (`XX-XXXXXXX`), `tax_period` range, `subsection_cd` whitelist, type validation, YoY row-count tripwire. Writes RDS reports to `data/logs/`. |
| 6 | `06_dictionary.R` | Auto-generates per-output data dictionary CSV from the FINAL crosswalk + harmonized data stats. |
| 7 | `07_render_report.R` | Renders the Quarto template `docs/quality_report_template.qmd` to HTML per `(form, tax_year)`. |
| 8 | `08_upload.R` | Promotes harmonized CSVs into `data/processed/{tax_year}/{form}/`, then per-tier `aws s3 sync` to `s3://nccsdata/`. |

Two additional orchestrators sit alongside `run_pipeline.R`:

- `R/run_legacy_pipeline.R` — pre-2012 raw NCCS legacy files (PZ + PF), writing to `data/intermediate/harmonized_legacy/` and `data/processed_legacy/`. See `docs/09-legacy-harmonization.qmd`.
- `R/run_build_panel.R` — Option D column-merge of the two harmonized trees on `(ein, tax_period)` with SOI precedence. Adds `source_pipeline` + `has_legacy_augment` tag columns and emits a per-(year, form) disagreement audit under `data/logs/`. Output: `data/intermediate/harmonized_merged/` → `data/processed_merged/`. Standalone because it depends on both upstream pipelines having produced output.

`R/transforms/` holds six pure column-transform functions (`tax_period`, `ein`, `subsection`, `financial_amounts`, `indicators`, `efile_indicator`). The test suite lives under `tests/` — seven files, 209 total tests covering transforms, crosswalk apply, combined-derivation, dictionary, pre/post-check validators, and the legacy/SOI merge. Run everything via the harness:

```bash
Rscript tests/run_all.R          # exits nonzero on failure
```

Or `source("tests/run_all.R")` from RStudio. Individual files also run standalone, e.g. `Rscript tests/test_harmonize.R`.

## Repo layout

```
nccs-data-core/
├── R/
│   ├── run_pipeline.R              # SOI-current orchestrator
│   ├── run_legacy_pipeline.R       # pre-2012 legacy NCCS orchestrator
│   ├── run_build_panel.R           # merged-panel orchestrator (legacy ∪ SOI-current)
│   ├── 01_download.R ... 08_upload.R, 04_legacy_merge.R, 09_parquet.R
│   ├── transforms/                 # six pure transforms
│   ├── quality/                    # pre/post-check validators + stat helpers
│   ├── config.R                    # paths, S3 prefixes, IRS URL table, phase toggles
│   ├── data.R                      # form inventory, lookup paths, subsection codes
│   └── utils.R, create_logger.R, aws_s3_sync.R, ...
├── scripts/
│   └── draft_990_crosswalk.R, draft_990ez_crosswalk.R, draft_990pf_crosswalk.R
├── data/
│   ├── crosswalks/                 # BASELINE / OVERRIDES / FINAL per form (tracked)
│   ├── lookups/                    # per-coded-column reference CSVs (tracked)
│   ├── raw/                        # SOI zips + form PDFs (gitignored)
│   ├── intermediate/               # unpacked sources + harmonized{,_legacy,_merged} output (gitignored)
│   ├── processed/                  # canonical SOI-current artifacts (gitignored)
│   ├── processed_legacy/           # canonical legacy artifacts (gitignored)
│   ├── processed_merged/           # merged-panel artifacts (gitignored)
│   └── logs/                       # per-phase log files + quality RDS + merge audits (gitignored)
├── docs/
│   ├── _quarto.yml                 # Quarto book config
│   ├── index.qmd, 01-architecture.qmd, ... 10-ec2-batch-processing.qmd
│   └── quality_report_template.qmd # Quarto template for per-output HTML reports
├── tests/
│   └── test_transforms.R
└── IMPLEMENTATION_PLAN.md          # design + build record
```

## Crosswalks

The crosswalk-driven harmonization layer is at `data/crosswalks/`. For each form there are three files:

- `soi_<form>_crosswalk_BASELINE.csv` — algorithmic draft, regenerable, **overwritten by the draft script**.
- `soi_<form>_crosswalk_OVERRIDES.csv` — manual editable copy. **Never overwritten by any script.** Edit here.
- `soi_<form>_crosswalk_FINAL.csv` — equals OVERRIDES verbatim. **Consumed by the pipeline.**

Re-run `scripts/draft_<form>_crosswalk.R` after editing OVERRIDES to regenerate FINAL.

## Lookups

`data/lookups/` holds per-coded-column reference CSVs with `(code, label, irc_ref, source, confidence)` columns. The first one, `subsection_codes.csv`, is built from IRM 25.7.1 Exhibit 25.7.1-4 (the IRS-internal EO subsection code reference) and is maintained independently from the parallel lookup in `nccs-data-bmf` — both derive from the same IRM upstream and should be cross-checked periodically.

## Rolling out a new processing year

The harmonize step rebuilds each `(tax_year, form)` output from the union of every `data/intermediate/unpacked/{processing_year}/{form}/` directory on disk. **Before running on a fresh machine or adding 2025+ extracts, rehydrate the prior unpacked state from S3** — otherwise rows for missing prior years drop silently:

```bash
aws s3 sync s3://nccsdata/intermediate/core/unpacked/ data/intermediate/unpacked/
aws s3 sync s3://nccsdata/raw/core/soi-extracts/      data/raw/soi_extracts/
```

Then bump `LATEST_YEAR` in `R/config.R`, add the new year's filename stems to `SOI_FILENAME_STEMS`, and run the pipeline.

See [Developer Guide](docs/07-developer-guide.qmd) for the full SOP.

## Required R packages

`data.table`, `arrow`, `aws.s3`, `paws`, `openxlsx`, `rio`, `here`, `purrr`, `stringr`, `lubridate`, `jsonlite`, `quarto`, `duckdb`, `DBI`, `log4r`, `tidyverse`, `data.validator`, `assertr`. Also requires the **AWS CLI** (used by phase 8 via `system2()`) and **pdftotext** (for ad-hoc form text extraction).

## Documentation

Full docs are a Quarto book under `docs/`, published to GitHub Pages at <https://urbaninstitute.github.io/nccs-data-core/> on every push to `main` that touches `docs/`, `data/lookups/`, or the workflow file itself (see `.github/workflows/publish-docs.yml`). To preview locally before pushing: `quarto render docs/` then open `docs/GUIDEBOOK/index.html`.

See `IMPLEMENTATION_PLAN.md` for the design rationale and build history. Outstanding work and known gaps are tracked in `TODO.md`.
