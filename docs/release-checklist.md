# CORE Series public-consumption release checklist

Run this checklist end-to-end before announcing a new CORE Series release to analysts. Treat it as a sequential walkthrough — one tester, ~1–2 hours. The stopping rules at the bottom say what to do when a section fails.

## 1. S3 accessibility

| # | Check | How | Pass |
|---|---|---|---|
| 1.1 | Buckets are publicly readable (no creds needed) | `aws s3 ls s3://nccsdata/processed/core/ --no-sign-request` | Lists year prefixes without auth error |
| 1.2 | Same for legacy tier | `aws s3 ls s3://nccsdata/processed_legacy/core/ --no-sign-request` | Lists 1987–2011 |
| 1.3 | Same for merged tier | `aws s3 ls s3://nccsdata/processed_merged/core/ --no-sign-request` | Lists 1987–2024 |
| 1.4 | Direct HTTPS GET works (no SDK) | `curl -I https://nccsdata.s3.amazonaws.com/processed_merged/core/2020/990combined/core_2020_990combined.parquet` | HTTP 200, correct `Content-Length` |
| 1.5 | No leftover stale prefixes | Re-run the Level 1 mismatch check (see "Level 1 inventory check" section below) | Zero `MISMATCH` lines |

## 2. File integrity

| # | Check | How | Pass |
|---|---|---|---|
| 2.1 | Random CSV opens cleanly in pandas / data.table | Download 3 random partitions, open with `read_csv` / `fread` | No parse errors, expected col count |
| 2.2 | Random parquet opens in arrow / duckdb | `duckdb> SELECT COUNT(*) FROM 's3://...parquet'` | Returns same count as CSV row count |
| 2.3 | Parquet ↔ CSV row-count parity (sample) | Run the Level 2 R script (see below) | Zero mismatches |
| 2.4 | Parquet ↔ CSV column-name parity | `diff <(head -1 ...csv | tr ',' '\n') <(arrow_columns.py ...parquet)` | Identical column lists |
| 2.5 | No zero-byte or truncated files | `aws s3 ls --recursive s3://nccsdata/processed/core/ | awk '$3==0'` | Empty result |
| 2.6 | Dictionary file matches data file columns | Open `_dictionary.csv`, compare its rows to data CSV header | One dict row per data column, no extras/missing |

## 3. Schema & data sanity (per tier)

For each of `processed/`, `processed_legacy/`, `processed_merged/`, pick **one partition per form** and verify:

| # | Check | What to look for |
|---|---|---|
| 3.1 | `ein` is `XX-XXXXXXX` format, 9 chars | `unique(nchar(ein))` returns only `9` |
| 3.2 | `tax_period` is 6-char `YYYYMM` | All values match `^[12][09][0-9]{2}(0[1-9]|1[0-2])$` |
| 3.3 | `tax_year` matches first 4 chars of `tax_period` | `all(substr(tax_period,1,4) == tax_year)` |
| 3.4 | `subsection_cd` values are in `data/lookups/subsection_codes.csv` whitelist | Quality report flags none outside |
| 3.5 | Financial columns parse as numeric | `class(...)` is `numeric` / `double` |
| 3.6 | No surprising NA explosion since last release | Spot-check a column's NA rate against prior release |
| 3.7 | **Merged tier only**: `source_pipeline` is `"legacy"` for tax_year ≤ 2011, `"soi_current"` for ≥ 2012 | `table(source_pipeline, tax_year >= 2012)` is diagonal |
| 3.8 | **Merged tier only**: `has_legacy_augment` is `FALSE` everywhere post-clamp | Should be ~all FALSE; non-zero rows indicate the disjoint-clamp assumption is wrong |
| 3.9 | **SOI-current only**: `is_amendment == FALSE` for ~99 %+ of rows | A handful TRUE expected (per harmonize log) |

## 4. Cross-tier consistency spot checks

| # | Check | How | Pass |
|---|---|---|---|
| 4.1 | A 2020/990 row in `processed/` appears identically in `processed_merged/990combined/` (where both exist) | Pick 3 EINs, compare key columns across both tiers | Values match for shared columns |
| 4.2 | A 2005/990combined row in `processed_legacy/` appears in `processed_merged/2005/990combined/` | Same approach | Values match (legacy stands alone pre-2012) |
| 4.3 | 2011/990combined boundary year: legacy and SOI rows for the same EIN in `processed_merged/` follow SOI precedence | Pick one EIN that exists in both 2011 raw legacy and 2012 SOI extract; trace which version landed in merged | SOI value wins on shared columns; legacy fills SOI's NAs |
| 4.4 | Total row count of merged ≈ legacy + SOI minus overlap | `aws s3 ls --summarize --recursive ...` byte-count sanity | Within ~10 % (overlap is small after symmetric clamps) |

## 5. Quality reports (GitHub Pages)

| # | Check | How | Pass |
|---|---|---|---|
| 5.1 | Every published partition has a quality report URL that resolves | For 5 random `(tier, year, form)` triples, `curl -I <url>` | HTTP 200 |
| 5.2 | Reports render in browser without JS errors | Open 3 reports manually | Page paints, no red console errors |
| 5.3 | Embedded plots show data | Scroll each report | Bar / coverage plots have bars; no broken images |
| 5.4 | Hard-fail status is correct | Quality report header says `hard_passed: TRUE` for all current partitions | All TRUE |
| 5.5 | Stale pre-2012 SOI-current reports removed from GH Pages | `curl -I https://urbaninstitute.github.io/nccs-data-core/quality-reports/1995/990pf/...` | HTTP 404 (file was correctly deleted) |
| 5.6 | Legacy + merged report subtrees populated | Random URL pattern in each | HTTP 200 |
| 5.7 | Known-gap years still render (just with low row counts) | 990pf 2017/2018/2019, 990pf 1993 legacy | Reports exist, completeness shown, no errors |

## 6. Documentation site

| # | Check | How | Pass |
|---|---|---|---|
| 6.1 | Site root loads | `curl -I https://urbaninstitute.github.io/nccs-data-core/` | HTTP 200 |
| 6.2 | All chapters in `_quarto.yml` resolve | Loop chapter list, `curl -I` each `*.html` | All HTTP 200 |
| 6.3 | Internal cross-references work | Click 5 inter-chapter links | No 404s |
| 6.4 | Mermaid diagrams render | Open `01-architecture.html`, `02-data-lineage.html` | Diagrams display, not raw text |
| 6.5 | Output-schema chapter matches actual columns | Compare doc column list to one downloaded CSV header | Lists agree |
| 6.6 | Mobile / narrow viewport readable | Resize browser to 400 px wide | No horizontal scroll on body text |

## 7. Website catalog page (nccs.urban.org)

| # | Check | How | Pass |
|---|---|---|---|
| 7.1 | Catalog page loads | https://nccs.urban.org/nccs/catalogs/catalog-core.html | HTTP 200, paints |
| 7.2 | Three tiers documented | Visual inspection | SOI-current, legacy, merged all visible |
| 7.3 | Download buttons link to live S3 objects | Click 3 CSV + 3 parquet download buttons | Each downloads correctly |
| 7.4 | Coverage matrix shows known gaps | Visual | 990pf 2017–2019 + legacy 1993 marked / footnoted |
| 7.5 | Dedup caveat for merged tier is present | Visual / search page text | Note exists, points users to per-tier files for amendments |
| 7.6 | Links to quality reports work | Click 3 quality-report links from catalog | Each resolves to the right HTML on GH Pages |
| 7.7 | Link to pipeline docs site works | Click "Documentation" / "Methodology" link | Lands on GH Pages site root |
| 7.8 | Column reference / dictionary linked or embedded | Visual | Either inline schema or link to `_dictionary.csv` download |

## 8. Edge cases & known gaps

| # | Check | What to verify |
|---|---|---|
| 8.1 | 990pf 2017 partition has ~665 rows and quality report says completeness 100 % | Open `https://urbaninstitute.github.io/nccs-data-core/quality-reports/2017/990pf/...` |
| 8.2 | 990pf 1993 legacy partition has ~11 k rows | Open `processed_legacy/1993/990pf/core_1993_990pf.csv`, count |
| 8.3 | Merged tier `990combined` for 2011 has only legacy rows (no SOI 2011) | `table(source_pipeline)` on 2011 partition |
| 8.4 | Merged tier `990combined` for 2012 has only SOI rows | Same |
| 8.5 | 2024 is "partial year" (only ~75 k 990combined rows vs ~520 k for 2023) | Add visible note on catalog page if not present |

## 9. Performance smoke tests (analyst experience)

| # | Check | How | Pass |
|---|---|---|---|
| 9.1 | Single parquet via DuckDB-S3 query | `duckdb> SELECT COUNT(*) FROM 's3://nccsdata/processed_merged/core/2020/990combined/core_2020_990combined.parquet'` | Returns in < 5 s |
| 9.2 | Multi-year glob query works | `SELECT tax_year, COUNT(*) FROM 's3://nccsdata/processed_merged/core/*/990combined/core_*_990combined.parquet' GROUP BY 1` | Returns in < 60 s, all years present |
| 9.3 | R-arrow `open_dataset` works | `open_dataset("s3://...", format="parquet")` | Returns a query-able dataset |
| 9.4 | CSV download time for largest file is reasonable | `time curl -O ... 2018_990combined.csv` (~125 MB) | Under 30 s on a good connection |

## 10. Communications & rollout

| # | Check | Status |
|---|---|---|
| 10.1 | Catalog page change announced internally (Slack / email) | |
| 10.2 | Known-gap callouts explicit in release note (not just buried in catalog) | |
| 10.3 | Migration guide for analysts moving from old SOI-only tier to merged tier | |
| 10.4 | If any column names changed since prior release, breaking-change note included | |
| 10.5 | Contact / issue-reporting path documented (GitHub issues link?) | |

## Stopping rule

- **All of §1–7 pass** → safe to announce to analysts.
- **§1 or §2 fails** → halt rollout; data integrity issue.
- **§3.7 or §3.8 fails** → halt rollout; merge boundary is broken.
- **§5 or §6 fails** → can still announce data availability if §1–4 are clean; document the docs gap and patch within a week.
- **§4 fails** → don't halt rollout, but file an issue and add a "known issue" note to the catalog; merge inconsistency is a data quality bug, not a publication blocker.
- **§7 fails** → halt the announcement; the surface analysts see has to work before they hear about it.

---

## Reusable scripts

### Level 1 inventory check (S3 csv ↔ parquet partition pairing)

```bash
for tier in processed processed_legacy processed_merged; do
  echo "== s3://nccsdata/$tier/core/ =="
  aws s3 ls s3://nccsdata/$tier/core/ --recursive \
    | awk '{print $NF}' \
    | grep -E '/core_[0-9]{4}_[^_]+\.(csv|parquet)$' \
    | awk -F'/' '{
        fname = $NF
        sub(/\.(csv|parquet)$/, "", fname)
        ext = ($0 ~ /\.parquet$/) ? "parquet" : "csv"
        partition = $(NF-2) "/" $(NF-1)
        seen[partition,ext] = 1
        parts[partition] = 1
      }
      END {
        for (p in parts) {
          c = seen[p,"csv"] ? "csv" : "----"
          q = seen[p,"parquet"] ? "parquet" : "-------"
          if (c == "----" || q == "-------") print "  MISMATCH " p "  [" c " " q "]"
        }
      }' | sort
done
```

Expect: three clean headers, zero `MISMATCH` lines.

### Level 2 row-count parity (local; run on EC2 against `data/processed*/`)

```bash
cat <<'EOF' | Rscript --vanilla -
suppressPackageStartupMessages({ library(data.table); library(arrow) })
roots <- c("data/processed", "data/processed_legacy", "data/processed_merged")
for (root in roots) {
  cat("==", root, "==\n")
  csvs <- list.files(root, pattern = "^core_\\d{4}_[^_]+\\.csv$",
                     recursive = TRUE, full.names = TRUE)
  csvs <- csvs[!grepl("_dictionary\\.csv$", csvs)]
  mismatches <- 0L
  for (csv in csvs) {
    parquet <- sub("\\.csv$", ".parquet", csv)
    if (!file.exists(parquet)) {
      cat("  MISSING PARQUET:", parquet, "\n"); mismatches <- mismatches + 1L; next
    }
    n_csv <- nrow(fread(csv, select = 1L))
    n_pq  <- nrow(read_parquet(parquet, col_select = 1L))
    if (n_csv != n_pq) {
      cat(sprintf("  ROW MISMATCH %s: csv=%d parquet=%d\n", csv, n_csv, n_pq))
      mismatches <- mismatches + 1L
    }
  }
  cat(sprintf("  checked %d partitions, %d mismatch(es)\n", length(csvs), mismatches))
}
EOF
```

Expect: zero mismatches per tier.

### Level 3 round-trip equality on a sample (local)

```bash
cat <<'EOF' | Rscript --vanilla -
suppressPackageStartupMessages({ library(data.table); library(arrow) })
samples <- c(
  "data/processed/2020/990/core_2020_990",
  "data/processed_legacy/1998/990combined/core_1998_990combined",
  "data/processed_merged/2015/990pf/core_2015_990pf"
)
for (s in samples) {
  csv     <- paste0(s, ".csv")
  parquet <- paste0(s, ".parquet")
  if (!file.exists(csv) || !file.exists(parquet)) {
    cat("MISSING:", s, "\n"); next
  }
  c_dt <- fread(csv, na.strings = c("", "NA"))
  p_dt <- as.data.table(read_parquet(parquet))
  setcolorder(p_dt, names(c_dt))
  cat(sprintf("%-65s cols: csv=%d pq=%d  rows: csv=%d pq=%d  identical=%s\n",
              s, ncol(c_dt), ncol(p_dt), nrow(c_dt), nrow(p_dt),
              identical(c_dt, p_dt)))
}
EOF
```

Expect: `identical=TRUE` for each sample (or a small diff explainable by `fread`/`read_parquet` type coercion).
