# TODO

Outstanding work and known gaps as of 2026-05-11 (end of Phase 9, full SOI-current pipeline shipping).

## Pipeline / code

- [x] **Phase 10: `scripts/run_pipeline.sh`** — thin wrapper around `Rscript --vanilla R/run_pipeline.R "$@"` with tee'd per-run console log under `data/logs/`. `scripts/setup_ec2.sh` adapted from its BMF-flavored copy: corrected the URL/S3-verify path, expanded the R-package install list (`paws`, `rio`, `log4r`, `tidyverse`, `data.validator`, `assertr`), added `poppler-utils` for `pdftotext`.
- [ ] **`run_legacy_pipeline.R`** — parallel pipeline for raw legacy NCCS files (1989–2011 PZ + PF only; 2012+ files in `s3://nccsdata/legacy/core/` are NCCS+SOI hybrids and should be skipped). Will need its own crosswalk-builder scripts; the BASELINE/OVERRIDES/FINAL pattern applies. Document in `docs/09-legacy-harmonization.qmd` once built.
- [x] **Render parallelization** — `run_render_reports()` now uses `parallel::mclapply` with per-render template copies in `tempdir()` for full Quarto-cache isolation. Default workers `min(detectCores() - 1, 8)`; override via `NCCS_RENDER_WORKERS` env var or `workers` function arg (env var wins). Smoke test: 4 renders × 4 workers = 12 sec vs ~28 sec serial.
- [x] **gzip-on-upload for HTML** — phase 8's processed-tier sync now runs in two passes when `ENABLE_GZIP_HTML_UPLOAD = TRUE`: non-HTML files upload normally, then *.html files are gzipped into a tempdir mirror and uploaded with `--content-encoding gzip --content-type text/html --metadata-directive REPLACE`. Real compression on embed-resources quality reports is ~3× (the bulk is base64 fonts/CSS), not the 5-10× estimate — total transfer reduction across 109 reports is ~113 MB → ~37 MB. Trade-off documented in `docs/07-developer-guide.qmd`.
- [ ] **Pipeline-run archive for richer YoY** — current YoY tripwire compares against a single `quality_*.prev.rds` snapshot promoted at run start (catches most regressions). Longer-term drift analysis would benefit from a timestamped archive at `s3://nccsdata/logs/core/{run_timestamp}/` retained for multiple runs.

## Crosswalks / lookups

- [x] **`data/lookups/non_pf_status_reason_codes.csv`** (2026-05-12) — built. SOI uses the **pre-2016 Schedule A Part I line numbering**; codes 1–10 map directly to lines 1–10 of the 2011-era form (church, school, hospital, …, 509(a)(2), public safety). Codes 11–15 map to line 11 (supporting org) + subtypes 11a–11d. Code 16 (~1k rows across 13 years) lacks a clean Schedule A line mapping and is flagged `confidence = unknown`. Cross-checked against the actual data distribution — code 9 grows 34% → 50% across 2010–2024 with no step change at the 2016 form revision, ruling out the alternative "agricultural research organization" interpretation that the new line numbering would suggest.
- [ ] **Historical forms archive** — fetch every prior year's Form 990 / 990-EZ / 990-PF + Schedule A PDF + instructions, save to `s3://nccsdata/raw/core/forms/{tax_year}/`, publish on the NCCS website as a permanent citable record (IRS occasionally removes old forms from `irs.gov/pub/irs-prior/`). Phase 8 now syncs whatever is in `data/raw/forms/` to `s3://nccsdata/raw/core/forms/` (gated by `ENABLE_UPLOAD_FORMS = TRUE`); the open work is *populating* the local dir with historical content. Current contents are 2024-vintage only.
- [x] **2013+ source-var name verification** — full pipeline run 2026-05-11 surfaced and resolved the actual drift: 990 cycles `tax_prd` (py2012, py2014, py2015) / `tax_pd` elsewhere; 990-EZ has five variants. All variants mapped in OVERRIDES; documented in `docs/11-upstream-source-quirks.qmd` ("`tax_period` source column name drifts across years").

## Refactors / debt

- [x] **Deduplicate `is_blank` helper** — single function definition in `R/utils.R`; `R/quality/post_checks.R` and `R/06_dictionary.R` now source utils.R and call it. (TODO note had three locations; only two were live — the `R/quality/stat_helpers.R` reference was stale.)
- [x] **Deduplicate `CROSSWALK_FOR_SERIES`** — single definition in `R/data.R` alongside `CROSSWALK_FILES`. `R/05_quality.R` and `R/06_dictionary.R` already source data.R, so no new source lines needed.
- [x] **`data/raw/` cleanup (2026-05-12)** — deleted the stray top-level `14eofinextract990pf.csv` plus the redundant `soi/` (4.1 GB), `soi_pf/` (439 MB), and `legacy_inventory/` (188K) directories. `soi/` and `soi_pf/` were predecessor formats whose content is byte-identical to `data/intermediate/unpacked/` (verified by md5); recoverable via the rehydrate-from-S3 SOP if needed. Removed the dead `PATHS$legacy_inventory` entry from `R/config.R`. Total reclaimed: ~4.5 GB.
- [ ] **`data/raw/core_pf/`** — contains NCCS legacy 990-PF CSVs (1989–2007 + a 2019 hybrid). Fine to leave for the future legacy pipeline; just be aware.

## Tests

- [x] **Tests beyond transforms (Tier 1 + Tier 2)** — five new test files cover the pure logic of phases 2.5, 3, 4, 5, 6:
      `tests/test_harmonize.R` (apply_crosswalk: rename, coalesce synonyms, NA-pad, required-col guard, case-insensitive match),
      `tests/test_combined.R` (project_to_shared + shared_990_990ez_cols integration check against actual crosswalks),
      `tests/test_dictionary.R` (build_dictionary_one: per-column stats, synonym collapse, Inf→NA cleanup, UNIVERSAL_DICTIONARY_ROWS injection),
      `tests/test_pre_checks.R` (file-level validators: missing/empty/header-only files, duplicate headers, trailing-empty quirk, BOM stripping, expected_col_count),
      `tests/test_post_checks.R` (parse_years_present, expected_harmonized_cols, EIN/tax_period/subsection/type validators, row-count tripwire).
      `tests/run_all.R` is the top-level harness (`Rscript tests/run_all.R` or `source("tests/run_all.R")` from RStudio). Total: 161 tests across 6 files, ~2.2 sec wall. Phase 1, 2, 7, 8 still uncovered — they're subprocess-bound (network / fread / Quarto / aws CLI) and unit-test ROI is low; smoke-tested via the full pipeline run.

## Documentation

- [x] **Quarto chapter prose pass** (2026-05-12) — `09-legacy-harmonization.qmd` expanded from 27 to 95 lines (scope, source-data inventory, locked-in decisions, planned design mirroring BMF, open design questions); `06-configuration.qmd` expanded from 26 to 107 lines (full reference for phase toggles, paths, S3 layout, IRS URL patterns, env vars, R/data.R constants); `01-architecture.qmd` got a top-of-page framing paragraph and a sibling-chapter index. `03-transforms-reference.qmd` was already filled during the 2026-05-12 indicator/efile work and didn't need this pass.
- [x] **Publish the rendered Quarto book** (2026-05-12) — already done. `.github/workflows/publish-docs.yml` renders the book on every push to `main` that touches `docs/`, `data/lookups/`, or the workflow file, and deploys to GitHub Pages. Live at <https://urbaninstitute.github.io/nccs-data-core/>. Two small workflow cleanups this session: fixed the path filter typo (`data/lookup/**` → `data/lookups/**` plural) so lookup-only edits actually retrigger the build, and removed a dead `cp -r docs/quality-reports/*.html` step that was masked by `|| true` (quality reports live under `data/processed/`, not in the repo).

## Data findings to validate or chase

- [x] **`nonpfrea` code 9 is 42% of 2018 990-EZ filers.** Resolved (2026-05-12): SOI's encoding is locked to the **pre-2016 Schedule A line numbering**, in which line 9 = `509(a)(2)` (gross-receipts public charity), not "agricultural research organization" as the post-2016 form numbering would suggest. The 42% share is consistent — gross-receipts is the most common 509(a) qualification for small operating charities. See `data/lookups/non_pf_status_reason_codes.csv` and the schema doc.
- [x] **2017–2019 990-PF gap** — IRS published no 990-PF extract for these processing years. Currently handled by NA-return from `build_soi_url()` (download phase logs a skip). Documented in `docs/11-upstream-source-quirks.qmd` ("Missing 990-PF publications for py2017, py2018, py2019"); resulting low row counts for tax_years 2016–2018 in 990-PF outputs are expected.
- [ ] **Cross-form §4947(a) trust appearance** — §4947(a)(1) trusts treated as private foundations show up in 990-PF (~6% of rows with `subsection_cd = 92`). Need to verify whether any analogous category appears in 990 or 990-EZ for the years we haven't yet harmonized.
