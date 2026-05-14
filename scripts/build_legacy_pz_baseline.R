# Build legacy_pz_crosswalk_BASELINE.csv from the inventory TSV.
#
# Reads:  data/raw/legacy_inventory/headers_by_file.tsv
# Writes: data/crosswalks/legacy_pz_crosswalk_BASELINE.csv          (always regenerated)
#         data/crosswalks/legacy_pz_crosswalk_OVERRIDES.csv         (initialized once, then preserved)
#         data/crosswalks/legacy_pz_crosswalk_FINAL.csv             (regenerated from OVERRIDES, enriched with combined_years_present)
#         data/crosswalks/legacy_pz_dropped_below_threshold.csv     (always regenerated — review-only)
#
# Locked-in policy (docs/09-legacy-harmonization.qmd):
#   - Scope: legacy PZ (501C3-CHARITIES + 501CE-NONPROFIT), tax years 1989-2011.
#   - Threshold: a source column is kept in BASELINE only if it appears in
#     >= 3 distinct tax years. Below-threshold columns are persisted to
#     legacy_pz_dropped_below_threshold.csv so they can be opted back in via
#     OVERRIDES if a specific column proves indispensable.
#   - Algorithmic harmonized_name = tolower(source) only. Legacy names are too
#     cryptic for safe automated expansion; real naming lives in OVERRIDES,
#     authored against the legacy dictionary.
#   - combined_years_present in FINAL = legacy years ∪ SOI-current years for
#     the same harmonized_name, joined against soi_990 + soi_990ez (the two
#     forms whose harmonized vocabularies the legacy PZ output is supposed
#     to stack against in the 990combined series).

suppressPackageStartupMessages({
  library(data.table)
})

INV_PATH       <- "data/raw/legacy_inventory/headers_by_file.tsv"
DICT_PATH      <- "data/raw/legacy_dictionaries/inventory.csv"
BASELINE_PATH  <- "data/crosswalks/legacy_pz_crosswalk_BASELINE.csv"
OVERRIDES_PATH <- "data/crosswalks/legacy_pz_crosswalk_OVERRIDES.csv"
FINAL_PATH     <- "data/crosswalks/legacy_pz_crosswalk_FINAL.csv"
DROPPED_PATH   <- "data/crosswalks/legacy_pz_dropped_below_threshold.csv"
SOI_TARGETS    <- c("data/crosswalks/soi_990_crosswalk_FINAL.csv",
                    "data/crosswalks/soi_990ez_crosswalk_FINAL.csv")
SCOPE_FILTER   <- "PZ"
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

expand_years <- function(s) {
  if (is.na(s) || s == "") return(integer(0))
  parts <- strsplit(s, ",\\s*")[[1]]
  yrs <- integer(0)
  for (p in parts) {
    p <- trimws(p)
    if (p == "") next
    if (grepl("-", p, fixed = TRUE)) {
      bounds <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
      yrs <- c(yrs, bounds[1]:bounds[2])
    } else {
      yrs <- c(yrs, as.integer(p))
    }
  }
  sort(unique(yrs))
}

enrich_with_dict <- function(dt, dict_canon) {
  dt[, source_upper := toupper(source_column)]
  dt[dict_canon, on = c(source_upper = "column_name_upper"),
     `:=`(label = i.label, description = i.description,
          section = i.section, dtype = i.dtype)]
  dt[, source_upper := NULL]
  dt
}

compute_combined_coverage <- function(final, soi_paths) {
  soi_rows <- rbindlist(
    lapply(soi_paths, function(p) {
      if (!file.exists(p)) {
        cat(sprintf("  WARN: SOI target %s missing; skipped from combined coverage\n", p))
        return(NULL)
      }
      fread(p)[, .(harmonized_name, years_present)]
    }),
    fill = TRUE, use.names = TRUE
  )
  if (nrow(soi_rows) == 0) {
    final[, combined_years_present := years_present]
    return(final)
  }
  soi_yrs <- soi_rows[, .(soi_years_str = collapse_years(
    unlist(lapply(years_present, expand_years))
  )), by = harmonized_name]
  final[soi_yrs, on = "harmonized_name", soi_years_str := i.soi_years_str]
  final[, combined_years_present := vapply(seq_len(.N), function(i) {
    if (is.na(harmonized_name[i]) || harmonized_name[i] == "") return("")
    leg <- expand_years(years_present[i])
    soi_y <- if (is.na(soi_years_str[i])) integer(0) else expand_years(soi_years_str[i])
    collapse_years(c(leg, soi_y))
  }, character(1))]
  final[, soi_years_str := NULL]
  final
}

# ---- Inventory: aggregate per source column across years ----
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

# Algorithmic mapping: lowercase only. Legacy names need dictionary-verified
# expansion in OVERRIDES — no algorithmic guessing here.
kept[,    harmonized_name := tolower(source_column)]
dropped[, harmonized_name := tolower(source_column)]

# ---- Enrich kept + dropped with dictionary metadata ----
if (file.exists(DICT_PATH)) {
  dict <- fread(DICT_PATH)
  dict <- dict[scope == SCOPE_FILTER]
  setorder(dict, column_name_upper, -tax_year)
  dict_canon <- dict[, .(label = first(label),
                         description = first(description),
                         section = first(section),
                         dtype = first(dtype)),
                     by = .(column_name_upper)]
  enrich_with_dict(kept, dict_canon)
  enrich_with_dict(dropped, dict_canon)
  n_unmatched <- kept[is.na(description) | description == "", .N]
  cat(sprintf("\nDictionary join: %d / %d kept columns have descriptions (%d unmatched)\n",
              nrow(kept) - n_unmatched, nrow(kept), n_unmatched))
} else {
  cat(sprintf("\nNote: %s not found; BASELINE will lack description columns.\n",
              DICT_PATH))
  cat("Run scripts/scrape_core_dictionaries.py to populate.\n")
  for (dt in list(kept, dropped)) {
    dt[, `:=`(label = NA_character_, description = NA_character_,
              section = NA_character_, dtype = NA_character_)]
  }
}

setorder(kept, source_column)
setorder(dropped, -n_tax_years, source_column)

select_cols <- c("source_column", "harmonized_name", "label", "description",
                 "section", "dtype", "n_tax_years", "years_present",
                 "first_year", "last_year", "scope_observed")
out         <- kept[, ..select_cols]
dropped_out <- dropped[, ..select_cols]

dir.create(dirname(BASELINE_PATH), recursive = TRUE, showWarnings = FALSE)

# --- BASELINE: algorithmic, always regenerated ---
fwrite(out, BASELINE_PATH)

# --- DROPPED (below threshold): review-only, always regenerated ---
# These columns appear in <3 tax years and are filtered out before OVERRIDES.
# If any of them are real schema elements worth preserving, append the row to
# OVERRIDES manually with a real harmonized_name (the next builder run will
# carry it through to FINAL).
fwrite(dropped_out, DROPPED_PATH)
cat(sprintf("\nWrote %d below-threshold rows to %s for opt-in review.\n",
            nrow(dropped_out), DROPPED_PATH))

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

# --- FINAL: regenerated from OVERRIDES, enriched with combined_years_present ---
final <- fread(OVERRIDES_PATH)
final <- compute_combined_coverage(final, SOI_TARGETS)
fwrite(final, FINAL_PATH)

cat("\n=== files written ===\n")
cat("  BASELINE :", BASELINE_PATH, "\n")
cat("  DROPPED  :", DROPPED_PATH, "\n")
cat("  OVERRIDES:", OVERRIDES_PATH, "\n")
cat("  FINAL    :", FINAL_PATH, "\n")
cat("rows in BASELINE:", nrow(out), "\n")

cat("\n=== combined-coverage summary (FINAL) ===\n")
non_empty <- final[harmonized_name != "" & !is.na(harmonized_name)]
extended  <- non_empty[combined_years_present != years_present]
cat(sprintf("  %d / %d active harmonized columns gain coverage from SOI-current\n",
            nrow(extended), nrow(non_empty)))
if (nrow(extended) > 0) {
  cat("  sample (legacy years -> combined years):\n")
  print(head(extended[, .(source_column, harmonized_name, years_present, combined_years_present)],
             10))
}
