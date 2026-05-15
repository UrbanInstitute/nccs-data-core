# R/04_legacy_merge.R
# Phase 4 of the merged-panel build: column-level merge of legacy harmonized
# output with SOI-current harmonized output (Option D, per docs/09 and the
# 2026-05-14 design pause memo).
#
# Reads:
#   data/intermediate/harmonized_legacy/{tax_year}/{form}/core_{tax_year}_{form}.csv
#   data/intermediate/harmonized/{tax_year}/{form}/core_{tax_year}_{form}.csv
#
# Writes:
#   data/intermediate/harmonized_merged/{tax_year}/{form}/core_{tax_year}_{form}.csv
#   data/logs/merge_disagreements_{tax_year}_{form}.csv   (only if any shared-
#       column non-NA disagreements exist for that partition)
#
# Forms considered: 990combined, 990pf (the two the legacy crosswalks target).
# SOI-current 990 and 990ez wide partitions are NOT merged here — analysts who
# want them keep reading data/intermediate/harmonized/. The merge unifies on
# the form-level granularity that both pipelines speak.
#
# Merge rule (per (tax_year, form)):
#   1. Join legacy rows + SOI-current rows on (ein, tax_period).
#   2. For shared harmonized columns: SOI value wins where non-NA; legacy
#      value fills where SOI is NA. (NA *defers* — SOI's non-NA precedence
#      means we never silently overwrite a real legacy value with NA.)
#   3. For legacy-only columns: legacy value (NA on SOI-only rows).
#   4. For SOI-only columns: SOI value (NA on legacy-only rows).
#   5. Tag each row with:
#        source_pipeline     ∈ {"legacy", "soi_current"}  -- the primary origin
#        has_legacy_augment  : logical -- TRUE iff (ein, tax_period) appeared
#                                          in BOTH pipelines (i.e. legacy
#                                          contributed columns/values to a
#                                          SOI-origin row, or vice versa).
#      Origin precedence: a row that appears in both is tagged
#      source_pipeline="soi_current" + has_legacy_augment=TRUE. A
#      legacy-only row is tagged source_pipeline="legacy" +
#      has_legacy_augment=FALSE.
#   6. Where SOI and legacy disagree on a shared column with BOTH values
#      non-NA, emit a row to the disagreement audit. The merged output
#      takes the SOI value (rule 2), so the audit is the only place this
#      conflict is recorded.
#
# Where only one pipeline has a partition (e.g., 1999 legacy-only, 2024
# SOI-only), the merge passes through unchanged + tags appropriately.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "create_logger.R"))

MERGE_FORMS <- c("990combined", "990pf")
MERGE_KEY   <- c("ein", "tax_period")

# Columns that are pipeline-emitted metadata rather than harmonized fields.
# These are NOT subject to disagreement audit (they are pipeline state, not
# IRS data) and are reconciled with simple precedence (SOI wins where
# present).
MERGE_META_COLS <- c("source_subsection_class")

partition_path <- function(root, tax_year, form) {
  file.path(root, as.character(tax_year), form,
            sprintf("core_%s_%s.csv", tax_year, form))
}

list_partitions <- function(root, form) {
  yr_dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  yr_dirs <- yr_dirs[grepl("^\\d{4}$", yr_dirs)]
  yrs <- as.integer(yr_dirs)
  has <- vapply(yrs, function(y) file.exists(partition_path(root, y, form)),
                logical(1))
  sort(yrs[has])
}

read_partition <- function(root, tax_year, form) {
  p <- partition_path(root, tax_year, form)
  if (!file.exists(p)) return(NULL)
  fread(p, colClasses = "character", na.strings = c("", "NA"))
}

#' Coalesce two character vectors of equal length: take `primary` where
#' non-NA & non-empty, else `secondary`.
coalesce_chr <- function(primary, secondary) {
  use_primary <- !is.na(primary) & primary != ""
  out <- ifelse(use_primary, primary, secondary)
  out
}

#' Build a disagreement audit data.table for two aligned same-key sub-frames.
#' Returns a long-form DT with one row per (key, harmonized_name, legacy,
#' soi) where both values are non-NA and unequal.
build_disagreements <- function(soi_dt, legacy_dt, shared_cols, key_cols) {
  if (nrow(soi_dt) == 0L || length(shared_cols) == 0L) {
    return(data.table(ein = character(), tax_period = character(),
                      harmonized_name = character(),
                      legacy_value = character(), soi_value = character()))
  }
  pieces <- vector("list", length(shared_cols))
  for (i in seq_along(shared_cols)) {
    col <- shared_cols[i]
    soi_v <- soi_dt[[col]]
    leg_v <- legacy_dt[[col]]
    both <- !is.na(soi_v) & soi_v != "" & !is.na(leg_v) & leg_v != ""
    diff <- both & soi_v != leg_v
    if (any(diff)) {
      pieces[[i]] <- data.table(
        ein             = soi_dt$ein[diff],
        tax_period      = soi_dt$tax_period[diff],
        harmonized_name = col,
        legacy_value    = leg_v[diff],
        soi_value       = soi_v[diff]
      )
    }
  }
  rbindlist(pieces, use.names = TRUE, fill = TRUE)
}

#' Deduplicate a side's rows on the merge key. Multiple rows with the same
#' (ein, tax_period) on either side are unexpected but observed in legacy
#' (amendment-like duplicates). Rule: keep the first occurrence; log a count.
dedupe_side <- function(dt, side_name, logger) {
  if (nrow(dt) == 0L) return(dt)
  if (!all(MERGE_KEY %in% names(dt))) {
    stop(sprintf("%s side missing merge key column(s): %s", side_name,
                 paste(setdiff(MERGE_KEY, names(dt)), collapse = ", ")))
  }
  before <- nrow(dt)
  dt <- unique(dt, by = MERGE_KEY)
  after <- nrow(dt)
  if (before != after) {
    log4r::warn(logger,
                sprintf("  %s: dropped %d duplicate (ein, tax_period) rows (kept first occurrence)",
                        side_name, before - after))
  }
  dt
}

#' Merge a single (tax_year, form) partition. Returns the merged DT + the
#' disagreement DT. If a side is missing, the present side is passed through
#' tagged appropriately and disagreements are empty.
merge_partition <- function(legacy_dt, soi_dt, tax_year, form, logger) {

  has_legacy <- !is.null(legacy_dt) && nrow(legacy_dt) > 0L
  has_soi    <- !is.null(soi_dt)    && nrow(soi_dt) > 0L

  if (!has_legacy && !has_soi) {
    return(list(merged = NULL,
                disagreements = data.table()))
  }

  if (has_legacy) legacy_dt <- dedupe_side(legacy_dt, "legacy", logger)
  if (has_soi)    soi_dt    <- dedupe_side(soi_dt,    "soi_current", logger)

  # Pass-through cases ----------------------------------------------------
  if (!has_legacy) {
    soi_dt[, source_pipeline    := "soi_current"]
    soi_dt[, has_legacy_augment := FALSE]
    log4r::info(logger,
                sprintf("  pass-through SOI-current only: %d rows, %d cols",
                        nrow(soi_dt), ncol(soi_dt)))
    return(list(merged = soi_dt, disagreements = data.table()))
  }
  if (!has_soi) {
    legacy_dt[, source_pipeline    := "legacy"]
    legacy_dt[, has_legacy_augment := FALSE]
    log4r::info(logger,
                sprintf("  pass-through legacy only: %d rows, %d cols",
                        nrow(legacy_dt), ncol(legacy_dt)))
    return(list(merged = legacy_dt, disagreements = data.table()))
  }

  # Two-sided merge -------------------------------------------------------
  legacy_cols <- setdiff(names(legacy_dt), MERGE_KEY)
  soi_cols    <- setdiff(names(soi_dt),    MERGE_KEY)
  shared_cols <- intersect(legacy_cols, soi_cols)
  legacy_only <- setdiff(legacy_cols, soi_cols)
  soi_only    <- setdiff(soi_cols,    legacy_cols)

  # Partition both sides by overlap status on (ein, tax_period).
  setkeyv(legacy_dt, MERGE_KEY)
  setkeyv(soi_dt,    MERGE_KEY)

  soi_keys    <- soi_dt[, .SD, .SDcols = MERGE_KEY]
  legacy_keys <- legacy_dt[, .SD, .SDcols = MERGE_KEY]
  overlap_keys <- fintersect(soi_keys, legacy_keys)

  soi_overlap     <- soi_dt[overlap_keys,    on = MERGE_KEY, nomatch = NULL]
  soi_unique      <- soi_dt[!overlap_keys,   on = MERGE_KEY]
  legacy_overlap  <- legacy_dt[overlap_keys, on = MERGE_KEY, nomatch = NULL]
  legacy_unique   <- legacy_dt[!overlap_keys,on = MERGE_KEY]

  # Align overlap sub-frames row-for-row on the merge key.
  setkeyv(soi_overlap,    MERGE_KEY)
  setkeyv(legacy_overlap, MERGE_KEY)

  # Disagreement audit (shared cols, both non-NA, value differs) before we
  # apply the SOI-precedence coalesce.
  audit_shared <- setdiff(shared_cols, MERGE_META_COLS)
  disagreements <- build_disagreements(soi_overlap, legacy_overlap,
                                       audit_shared, MERGE_KEY)

  # Apply SOI-precedence coalesce for the overlap rows.
  for (col in shared_cols) {
    soi_overlap[, (col) := coalesce_chr(get(col), legacy_overlap[[col]])]
  }
  # Bring in legacy-only columns onto the SOI overlap rows.
  for (col in legacy_only) {
    soi_overlap[, (col) := legacy_overlap[[col]]]
  }

  soi_overlap[,    source_pipeline    := "soi_current"]
  soi_overlap[,    has_legacy_augment := TRUE]
  soi_unique[,     source_pipeline    := "soi_current"]
  soi_unique[,     has_legacy_augment := FALSE]
  legacy_unique[,  source_pipeline    := "legacy"]
  legacy_unique[,  has_legacy_augment := FALSE]

  # Pad each piece so all three pieces have identical column sets before
  # rbinding. Universe of harmonized cols = union of both sides.
  full_cols <- unique(c(MERGE_KEY, shared_cols, legacy_only, soi_only,
                        "source_pipeline", "has_legacy_augment"))
  for (col in setdiff(full_cols, names(soi_overlap)))
    soi_overlap[, (col) := NA_character_]
  for (col in setdiff(full_cols, names(soi_unique)))
    soi_unique[, (col) := NA_character_]
  for (col in setdiff(full_cols, names(legacy_unique)))
    legacy_unique[, (col) := NA_character_]

  setcolorder(soi_overlap,   full_cols)
  setcolorder(soi_unique,    full_cols)
  setcolorder(legacy_unique, full_cols)

  merged <- rbindlist(list(soi_overlap, soi_unique, legacy_unique),
                      use.names = TRUE)

  log4r::info(logger,
              sprintf("  merged: overlap=%d soi_only=%d legacy_only=%d total=%d (shared_cols=%d legacy_only_cols=%d soi_only_cols=%d disagreements=%d)",
                      nrow(soi_overlap), nrow(soi_unique),
                      nrow(legacy_unique), nrow(merged),
                      length(shared_cols), length(legacy_only),
                      length(soi_only), nrow(disagreements)))

  list(merged = merged, disagreements = disagreements)
}

write_partition <- function(merged, tax_year, form, dest_root, logger) {
  out_dir <- file.path(dest_root, tax_year, form)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, sprintf("core_%d_%s.csv", tax_year, form))
  fwrite(merged, out_path)
  log4r::info(logger, sprintf("WROTE %s (%d rows, %d cols)",
                              out_path, nrow(merged), ncol(merged)))
  out_path
}

write_disagreements <- function(disagreements, tax_year, form, logger) {
  if (nrow(disagreements) == 0L) return(invisible(NULL))
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(PATHS$logs,
                        sprintf("merge_disagreements_%d_%s.csv", tax_year, form))
  fwrite(disagreements, out_path)
  log4r::info(logger, sprintf("WROTE %s (%d disagreement rows)",
                              out_path, nrow(disagreements)))
  invisible(out_path)
}

run_legacy_merge <- function(legacy_root = PATHS$harmonized_legacy,
                             soi_root    = PATHS$harmonized,
                             dest_root   = PATHS$harmonized_merged,
                             forms       = MERGE_FORMS) {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "04_legacy_merge_log.txt"))

  if (!dir.exists(legacy_root) && !dir.exists(soi_root)) {
    log4r::error(logger,
                 sprintf("Neither legacy_root (%s) nor soi_root (%s) exists. Run phases 1-3 of both pipelines first.",
                         legacy_root, soi_root))
    stop("No harmonized input available for merge.")
  }

  for (form in forms) {
    legacy_yrs <- if (dir.exists(legacy_root)) list_partitions(legacy_root, form) else integer()
    soi_yrs    <- if (dir.exists(soi_root))    list_partitions(soi_root,    form) else integer()
    yrs <- sort(unique(c(legacy_yrs, soi_yrs)))
    log4r::info(logger,
                sprintf("==== FORM %s: %d tax_years (legacy=%d soi=%d) ====",
                        form, length(yrs), length(legacy_yrs), length(soi_yrs)))
    for (yr in yrs) {
      log4r::info(logger, sprintf("---- %d / %s ----", yr, form))
      legacy_dt <- if (yr %in% legacy_yrs) read_partition(legacy_root, yr, form) else NULL
      soi_dt    <- if (yr %in% soi_yrs)    read_partition(soi_root,    yr, form) else NULL
      result <- merge_partition(legacy_dt, soi_dt, yr, form, logger)
      if (is.null(result$merged)) next
      write_partition(result$merged, yr, form, dest_root, logger)
      write_disagreements(result$disagreements, yr, form, logger)
    }
  }

  invisible(NULL)
}

if (sys.nframe() == 0L) run_legacy_merge()
