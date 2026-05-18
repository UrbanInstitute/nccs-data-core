# Log-triage gotchas

Known quirks in `nccs-data-core` pipeline run output that look like bugs but aren't, plus repeatable pitfalls in interpreting the logs. Consulted by Claude during log triage (see `CLAUDE.md` → "Triaging pipeline run output").

This doc is the **internal triage cheat-sheet** — short, log-shaped, focused on telling real problems from known ones. The companion user-facing reference is `docs/11-upstream-source-quirks.qmd`, which documents the underlying IRS source-file oddities and the pipeline's compensating logic in more depth. When an entry here references "the documented case for X," `11-upstream-source-quirks.qmd` is where to look.

**Maintenance contract:** when log triage surfaces a new quirk worth remembering, or an existing entry becomes stale (pipeline change, fixed quirk, schema change), the entry is added/updated in the same turn — not deferred. Each entry should say what the log looks like, why it's not a bug (or, if it is, what fixes it), and how to tell it apart from a real problem.

---

## Pre-check `passed=TRUE` does not mean `cols == exp`

Pre-check tolerance is wide; `cols=N (exp=M)` can differ silently with `passed=TRUE`. Always compare the two values directly rather than relying on the pass flag.

When `cols < exp`: a source column the var_matrix expects is missing — could be a real upstream change, or (more often) a stray non-variable row in the var matrix (e.g., an asterisk-prefixed footnote ingested as a variable). Inspect the matrix row before assuming a source-data issue.

## 990pf has no 2017/2018/2019 published extracts

Low row counts for tax_years 2016–2018 in 990pf outputs are *expected*: most 990pf forms for those tax_years would have come from py2017–py2019 extracts, which the IRS never published. Don't flag these as anomalies.

## Legacy 1993/990pf row count is ~75% lower than neighbors

`processed_legacy/1993/990pf/` and `processed_merged/1993/990pf/` contain ~10–12k rows vs. ~40k for 1992 and 1994. **`CORE-1993-501C3-PRIVFOUND-PF.csv` is missing from `s3://nccsdata/legacy/core/`** (verified 2026-05-18). The 1993 rows that do exist are late-filer spillover from neighbor-year PF files whose `TAXPER` starts "1993". Don't flag this as a pipeline regression.

## Pre-check `cols=N` vs reader `cols=N±1` off-by-one

The pre-check log's `cols=N` is the raw `strsplit` count of header fields (includes trailing empties). The reader's `cols=M` comes from `fread(fill=TRUE)` and may differ by ±1 depending on how trailing commas resolve. The tolerance check uses `n_named` (empties excluded), which is what matches the `exp` value. A 1-count gap between the two log lines is cosmetic, not a real mismatch.

## Single-digit "unparseable" / "outside indicator set" warnings

These get NA-coerced and are harmless — typically a stray non-numeric in a financial column or one bad indicator value. Thousands of such warnings on a single column in a single vintage is a real signal worth investigating; single digits are not.

## Trailing `vNN` columns in 2020+ CSV vintages

IRS 2020+ CSV extracts have trailing commas that produce empty header fields; `fread` synthesizes names like `v73`, `v247`–`v251`. These are not real columns. A genuine unmapped column has a meaningful name (e.g., `e-file`, `filedfyyn0polcd`) — those need a crosswalk entry.

## Single-vintage completeness drops are usually upstream, not pipeline bugs

A `_cd` indicator column going from ~100% complete to ~20–80% in one vintage, then recovering in subsequent ones, almost always reflects the IRS SOI extract for that processing year omitting or under-populating the column — not a harmonization bug. The documented example is **tax_year 2021 / 990** (28 affected indicator columns; see `docs/08-output-schema.qmd` → "Tax_year 2021 990 has a single-vintage gap…"). Before debugging the pipeline, check (a) whether the dip is concentrated in one vintage with recovery on either side, and (b) whether the affected columns are all `_cd` indicators rather than a mix of types — both point to upstream cause.

## YoY tripwire warnings on the first run after a snapshot reset

`yoy=outside_bounds` for years before the snapshot baseline is normal on the first full run; the warning reflects the absence of a prior baseline, not a row-count anomaly. Tripwires become meaningful only on subsequent runs with the same baseline.
