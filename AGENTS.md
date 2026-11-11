# Repository guidance

This repo produces NCCS CORE panels from IRS SOI extracts (2012+) and
legacy NCCS files (1989-2011). Follow the parent workspace instructions,
including its operating rules and code conventions.

## Entry points and outputs

Run from the repo root. Each orchestrator calls independently runnable
phase scripts under `R/`:

| Orchestrator | Local output | S3 prefix under `s3://nccsdata/` |
|---|---|---|
| `R/run_pipeline.R` | `data/processed/` | `processed/core/` |
| `R/run_legacy_pipeline.R` | `data/processed_legacy/` | `processed_legacy/core/` |
| `R/run_build_panel.R` | `data/processed_merged/` | `processed_merged/core/` |

Outputs are `core_{year}_{form}.csv` plus parquet, dictionaries and quality
reports. Forms: `990`, `990ez`, `990pf`, `990combined` (990 + 990-EZ).
The merged panel is the canonical analyst artifact, with SOI precedence.
Rebuild it after either upstream changes. Keep each pipeline's paths and
upload function separate.

## Rules that prevent data loss

- Partition by tax year, the first four characters of valid `TAXPER`,
  never the publication or filing year. Legacy rows with invalid/missing
  periods fall back to the source-file year. `source_file_year` records
  provenance; repeated legacy returns keep the newest file's copy
  (ADR 0057, `R/transforms/repeated_returns.R`).
- Harmonize rebuilds from all local source files. Before adding a year or
  running on a fresh machine, rehydrate prior unpacked sources from S3.
  Follow `docs/07-developer-guide.qmd`; missing inputs silently lose rows.
- Crosswalks drive renames. Edit BASELINE (regenerable) or OVERRIDES
  (user-authored), then regenerate FINAL. Never edit FINAL directly or
  delete OVERRIDES without explicit permission. Verify every mapping
  against source dictionaries; matcher scores alone are insufficient.
- Use `create_logger()` and log4r for pipeline events. Keep quality RDS
  and HTML paths separate across pipelines to avoid shared-key clobbering:
  SOI uses `data/logs/` and `docs/quality-reports/`; legacy and merged use
  their respective `legacy/` and `merged/` subdirectories.
- Data outputs, raw/intermediate inputs and logs are gitignored.
  Crosswalks, lookups and `docs/quality-reports/` are tracked.
- Publish-surface changes need an `ADR NNNN` breadcrumb and contracts
  reconciliation, or `contracts-ack` when no contract impact exists
  (ADR 0022). Keep `.github/workflows/contracts-guard.yml` paths aligned
  with publishing code and `core-990`, `core-legacy`, `core-panel` contracts.
  The guard checks acknowledgment, not correctness.

## Validation and log triage

Run `Rscript tests/run_all.R`; individual `tests/test_*.R` files also run
standalone. There is no package build. Use `--no-upload` for local pipeline
runs; other per-phase flags are documented in the reference below.
EC2/cron uses `bash scripts/run_pipeline.sh [flags]`, a `--vanilla` wrapper
that records console output and propagates the exit code.

For pipeline traces, read twice: inventory phases, failures and warning
counts, then check silent mismatches and claims that exceed the evidence.
Quote exact counts. Verify inferences against source data/code before
reporting findings or proposing fixes; label anything unverified and say
what would confirm it. Read `docs/log-triage-gotchas.md` first and update it
in the same turn when a new quirk appears or an entry becomes stale.

## Read when relevant

- `docs/pipeline-reference.md`: phase table, shared modules, RDS isolation,
  commands, package inventory and AWS CLI setup.
- `docs/04-crosswalks.qmd`: BASELINE/OVERRIDES/FINAL workflow.
- `docs/05-quality-gates.qmd`: pre/post checks and strict-mode behavior.
- `docs/06-configuration.qmd`: paths and flags (`R/config.R`, `R/data.R`).
- `docs/07-developer-guide.qmd`: rehydration, development and testing.
- `docs/08-output-schema.qmd`: columns and NA semantics.
- `docs/09-legacy-harmonization.qmd`: legacy processing and panel merge.
- `docs/10-ec2-batch-processing.qmd`: EC2 setup and operations.
- `docs/11-upstream-source-quirks.qmd`: source-specific exceptions.
- `docs/release-checklist.md`: publishing checks.
