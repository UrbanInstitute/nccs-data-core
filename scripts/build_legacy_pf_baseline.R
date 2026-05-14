# Build legacy_pf_crosswalk_BASELINE.csv from the inventory TSV.
#
# Reads:  data/raw/legacy_inventory/headers_by_file.tsv
# Writes: data/crosswalks/legacy_pf_crosswalk_BASELINE.csv (always regenerated)
#         data/crosswalks/legacy_pf_crosswalk_OVERRIDES.csv (initialized once, then preserved)
#         data/crosswalks/legacy_pf_crosswalk_FINAL.csv (regenerated from OVERRIDES)
#
# Locked-in policy (docs/09-legacy-harmonization.qmd):
#   - Scope: legacy PF (501C3-PRIVFOUND), tax years 1989-2011.
#   - Threshold: a source column is kept in BASELINE only if it appears in
#     >= 3 distinct tax years. Below-threshold columns drop out at the
#     crosswalk stage; can be opted back in via OVERRIDES if needed.
#   - Algorithmic harmonized_name = tolower(source) only. Legacy names are too
#     cryptic for safe automated expansion; real naming lives in OVERRIDES,
#     authored against the legacy dictionary.

suppressPackageStartupMessages({
  library(data.table)
})

INV_PATH       <- "data/raw/legacy_inventory/headers_by_file.tsv"
DICT_PATH      <- "data/raw/legacy_dictionaries/inventory.csv"
BASELINE_PATH  <- "data/crosswalks/legacy_pf_crosswalk_BASELINE.csv"
OVERRIDES_PATH <- "data/crosswalks/legacy_pf_crosswalk_OVERRIDES.csv"
FINAL_PATH     <- "data/crosswalks/legacy_pf_crosswalk_FINAL.csv"
SCOPE_FILTER   <- "PF"
MIN_TAX_YEARS  <- 3L

collapse_years <- function(yrs) {
  yrs <- sort(unique(as.integer(yrs)))
  if (length(yrs) == 0) return("")
  breaks <- c(0L, which(diff(yrs) != 1L), length(yrs))
  parts <- character()
  for (i in seq_len(length(breaks) - 1L)) {
    run <- yrs[(breaks[i] + 1L):breaks[i + 1L]]
    parts[i] <- if (length(run) == 1L) as.character(run) else
                paste0(run[1], "-", run[length(run)])
  }
  paste(parts, collapse = ", ")
}

inv <- fread(INV_PATH)
inv <- inv[scope == SCOPE_FILTER]
inv[, source_key := tolower(column_name_raw)]
setorder(inv, source_key, -tax_year)

agg <- inv[, .(
  source_column   = first(column_name_raw),
  n_tax_years     = uniqueN(tax_year),
  years_present   = collapse_years(tax_year),
  first_year      = min(tax_year),
  last_year       = max(tax_year),
  scope_observed  = paste(sort(unique(subsection_class)), collapse = "|")
), by = source_key]

cat(sprintf("Distinct source columns (case-folded) before threshold: %d\n",
            nrow(agg)))
cat(sprintf("Distribution of n_tax_years:\n"))
print(agg[, .N, by = n_tax_years][order(n_tax_years)])

kept    <- agg[n_tax_years >= MIN_TAX_YEARS]
dropped <- agg[n_tax_years <  MIN_TAX_YEARS]
cat(sprintf("\nKept (n_tax_years >= %d): %d\n", MIN_TAX_YEARS, nrow(kept)))
cat(sprintf("Dropped (vintage noise):  %d\n", nrow(dropped)))

kept[, harmonized_name := tolower(source_column)]

# ---- Enrich with dictionary metadata (most-recent year canonical, scope-matched) ----
if (file.exists(DICT_PATH)) {
  dict <- fread(DICT_PATH)
  dict <- dict[scope == SCOPE_FILTER]
  setorder(dict, column_name_upper, -tax_year)
  dict_canon <- dict[, .(label = first(label),
                         description = first(description),
                         section = first(section),
                         dtype = first(dtype)),
                     by = .(column_name_upper)]
  kept[, source_upper := toupper(source_column)]
  kept[dict_canon, on = c(source_upper = "column_name_upper"),
       `:=`(label = i.label, description = i.description,
            section = i.section, dtype = i.dtype)]
  kept[, source_upper := NULL]
  n_unmatched <- kept[is.na(description) | description == "", .N]
  cat(sprintf("\nDictionary join: %d / %d kept columns have descriptions (%d unmatched)\n",
              nrow(kept) - n_unmatched, nrow(kept), n_unmatched))
} else {
  cat(sprintf("\nNote: %s not found; BASELINE will lack description columns.\n",
              DICT_PATH))
  cat("Run scripts/scrape_core_dictionaries.py to populate.\n")
  kept[, `:=`(label = NA_character_, description = NA_character_,
              section = NA_character_, dtype = NA_character_)]
}

setorder(kept, source_column)

out <- kept[, .(source_column, harmonized_name, label, description, section, dtype,
                n_tax_years, years_present, first_year, last_year, scope_observed)]

dir.create(dirname(BASELINE_PATH), recursive = TRUE, showWarnings = FALSE)

# --- BASELINE: algorithmic, always regenerated ---
fwrite(out, BASELINE_PATH)

# --- OVERRIDES: user-editable, never overwritten ---
# Initialization rule: rows that didn't match a CORE dictionary entry are
# presumed to be NCCS-appended BMF metadata (per the schema-parity decision
# in docs/09 -> "Drop BMF-origin columns"). These get harmonized_name=""
# in the initial OVERRIDES so the harmonize step skips them. The user can
# flip any of them back to a real name during OVERRIDES authoring.
if (!file.exists(OVERRIDES_PATH)) {
  init <- copy(out)
  unmatched <- is.na(init$description) | init$description == ""
  init[unmatched, harmonized_name := ""]
  fwrite(init, OVERRIDES_PATH)
  n_dropped <- sum(unmatched)
  cat(sprintf("\nInitialized OVERRIDES at %s\n", OVERRIDES_PATH))
  cat(sprintf("  %d rows pre-marked harmonized_name='' (unmatched in CORE dict; presumed BMF-origin)\n",
              n_dropped))
  cat(sprintf("  %d rows kept with tolower(source_column) harmonized name\n",
              nrow(init) - n_dropped))
  cat("  -> Edit this file in place. Future runs will not touch it.\n")
} else {
  cat("\nPreserving existing OVERRIDES file (script will NOT overwrite):",
      OVERRIDES_PATH, "\n")
  ov <- fread(OVERRIDES_PATH)
  new_in_baseline       <- setdiff(out$source_column, ov$source_column)
  removed_from_baseline <- setdiff(ov$source_column, out$source_column)
  if (length(new_in_baseline) > 0) {
    cat(sprintf("\n*** ATTENTION: %d source_columns in BASELINE but not OVERRIDES ***\n",
                length(new_in_baseline)))
    cat("  ", paste(new_in_baseline, collapse = ", "), "\n", sep = "")
    cat("  -> Append these to OVERRIDES manually if you want them in FINAL.\n")
  }
  if (length(removed_from_baseline) > 0) {
    cat(sprintf("\n*** Heads-up: %d source_columns in OVERRIDES but no longer in BASELINE ***\n",
                length(removed_from_baseline)))
    cat("  ", paste(removed_from_baseline, collapse = ", "), "\n", sep = "")
  }
}

# --- FINAL: regenerated from OVERRIDES ---
final <- fread(OVERRIDES_PATH)
fwrite(final, FINAL_PATH)

cat("\n=== files written ===\n")
cat("  BASELINE :", BASELINE_PATH, "\n")
cat("  OVERRIDES:", OVERRIDES_PATH, "\n")
cat("  FINAL    :", FINAL_PATH, "\n")
cat("rows in BASELINE:", nrow(out), "\n")

cat("\n=== sample rows ===\n")
print(out[c(1, ceiling(nrow(out)/4), ceiling(nrow(out)/2),
            ceiling(3*nrow(out)/4), nrow(out))])

cat("\n=== rare-vintage drops (sample of dropped 1- and 2-year columns) ===\n")
print(dropped[order(-n_tax_years, source_column)][1:min(20, nrow(dropped))])
