# R/09_parquet.R
# Phase 9: write a .parquet copy next to every processed .csv under
# data/processed/. Parquet outputs are intended for API serving and R-package
# consumption — much faster column-wise reads than CSV at the cost of being
# binary (less convenient for ad-hoc inspection).
#
# Idempotent: a .parquet that exists, is at least as new as its .csv source,
# AND already matches the pinned cross-vintage schema is skipped. Pass
# overwrite=TRUE to regenerate all.
#
# Cross-vintage schema pinning (ADR 0027, route A)
# ------------------------------------------------
# Each (tax_year, form) file used to be type-inferred independently by fread,
# so a column could land INT one year and DOUBLE another (e.g.
# gross_income_other: INT in early years, DOUBLE in 2015, out of INT32 range).
# DuckDB infers a glob's schema from the first file and fails the cast, so
# multi-year readers needed read_parquet(..., union_by_name=True).
#
# We retire that workaround: files are grouped into schema-families (same form,
# all vintages), one canonical type is derived per column that is wide enough to
# hold every vintage WITHOUT losing values (widen INT->INT64->DOUBLE; string-
# widen genuine numeric/string conflicts, the vintage-stacking stance), and each
# vintage's frame is cast to that pinned schema before write_parquet. Columns
# whose inferred type is already stable across vintages are written unchanged.
# Column names, the field set, the grain, and the CSV tier are untouched — only
# parquet column *types* stabilize.
#
# Pipeline-agnostic: works against any directory tree the SOI-current or
# legacy pipelines write to under PATHS$processed.

suppressPackageStartupMessages({
  library(here)
  library(parallel)
})

source(here("R", "config.R"))
source(here("R", "utils.R"))
source(here("R", "create_logger.R"))

# ---- Schema-family grouping --------------------------------------------------

#' Schema-family key for a processed CSV: its basename with the 4-digit tax year
#' stripped, so every vintage of one form collapses to one key
#' (core_2012_990.csv, core_2013_990.csv -> "core__990.csv") while distinct
#' forms and the dictionary sidecars stay separate
#' ("core__990pf.csv", "core__990_dictionary.csv"). Files with no embedded year
#' group by their own basename (singleton family -> written as-is).
.schema_group_key <- function(csv_path) {
  sub("_(\\d{4})_", "__", basename(csv_path))
}

# ---- Type-widening lattice ---------------------------------------------------
# Rank the fread-inferred class of a column so the canonical type per column can
# be the widest observed across vintages, chosen to never drop values:
#   logical (all-NA / boolean) < integer(int32) < integer64 < double < character
# A class outside this lattice (e.g. Date) ranks at the top so that, IF it
# conflicts with another class across vintages, the family string-widens (the
# safe vintage-stacking stance) rather than silently coercing.

.rank_of_class <- function(cls) {
  switch(cls,
         logical    = 0L,
         integer    = 1L,
         integer64  = 2L,
         numeric    = 3L,
         double     = 3L,
         character  = 4L,
         4L)  # unknown / exotic -> string-widen on conflict
}

.arrow_type_for_rank <- function(r) {
  if (r <= 0L)      arrow::boolean()
  else if (r == 1L) arrow::int32()
  else if (r == 2L) arrow::int64()
  else if (r == 3L) arrow::float64()
  else              arrow::utf8()
}

#' First-class of each column of a CSV, as fread infers it. fread reads the
#' whole file, so the returned class reflects every row (a late out-of-INT32
#' value surfaces as integer64/numeric, which is exactly the drift we pin).
.fread_classes <- function(csv_path) {
  df <- data.table::fread(csv_path)
  vapply(df, function(x) class(x)[1L], character(1L))
}

#' Reduce a schema-family's per-file class vectors to the pinned arrow type for
#' every column that DRIFTS across vintages. Stable columns are omitted (caller
#' writes them unchanged). Returns a named list: column -> arrow DataType.
#'
#' @param class_vectors list of named character vectors (one per file in family)
.pin_family_types <- function(class_vectors) {
  cols <- unique(unlist(lapply(class_vectors, names)))
  pinned <- list()
  for (col in cols) {
    classes <- unlist(lapply(class_vectors, function(cv) cv[[col]]),
                      use.names = FALSE)
    classes <- classes[!is.na(classes)]
    if (length(unique(classes)) <= 1L) next            # stable -> leave as-is
    max_rank <- max(vapply(classes, .rank_of_class, integer(1L)))
    pinned[[col]] <- .arrow_type_for_rank(max_rank)
  }
  pinned
}

#' Human-readable per-column drift detail for the log inventory:
#' "col: integer(2012,2013) | numeric(2015) -> double".
.describe_drift <- function(class_vectors, pinned) {
  if (length(pinned) == 0L) return(character(0L))
  years <- vapply(class_vectors, function(cv) {
    y <- regmatches(attr(cv, "stem"), regexpr("\\d{4}", attr(cv, "stem")))
    if (length(y)) y else ""
  }, character(1L))
  vapply(names(pinned), function(col) {
    obs <- vapply(class_vectors, function(cv) cv[[col]] %||% NA_character_,
                  character(1L))
    by_class <- split(years, obs)
    parts <- vapply(names(by_class), function(cls)
      sprintf("%s(%s)", cls, paste(sort(by_class[[cls]]), collapse = ",")),
      character(1L))
    sprintf("%s: %s -> %s", col, paste(parts, collapse = " | "),
            pinned[[col]]$ToString())
  }, character(1L))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- Write one Parquet, cast to the pinned family schema ---------------------

#' TRUE if the existing parquet already conforms to the pinned types for the
#' drifting columns (so an up-to-date file need not be rewritten).
.parquet_matches_pins <- function(pq_path, pinned) {
  if (length(pinned) == 0L) return(TRUE)
  sch <- tryCatch(arrow::open_dataset(pq_path, format = "parquet")$schema,
                  error = function(e) NULL)
  if (is.null(sch)) return(FALSE)
  for (col in names(pinned)) {
    f <- tryCatch(sch$GetFieldByName(col), error = function(e) NULL)
    if (is.null(f) || !f$type$Equals(pinned[[col]])) return(FALSE)
  }
  TRUE
}

#' Convert one CSV to a sibling Parquet, casting drifting columns to the pinned
#' family schema. @return TRUE if written or already up-to-date, FALSE on error.
.csv_to_parquet <- function(csv_path, pinned, overwrite, logger) {
  pq_path <- sub("\\.csv$", ".parquet", csv_path, ignore.case = TRUE)
  if (pq_path == csv_path) {
    log4r::warn(logger, sprintf("SKIP no .csv suffix: %s", csv_path))
    return(FALSE)
  }
  if (!overwrite && file.exists(pq_path) &&
      file.info(pq_path)$mtime >= file.info(csv_path)$mtime &&
      .parquet_matches_pins(pq_path, pinned)) {
    log4r::info(logger, sprintf("SKIP up-to-date: %s", pq_path))
    return(TRUE)
  }
  tryCatch({
    df  <- data.table::fread(csv_path)
    tbl <- arrow::arrow_table(df)
    if (length(pinned)) {
      new_fields <- lapply(names(df), function(nm) {
        ty <- if (!is.null(pinned[[nm]])) pinned[[nm]]
              else tbl$schema$GetFieldByName(nm)$type
        arrow::field(nm, ty)
      })
      tbl <- tbl$cast(arrow::schema(new_fields))
    }
    arrow::write_parquet(tbl, pq_path)
    log4r::info(logger, sprintf("OK %s (%.1f MB csv -> %.1f MB parquet)",
                                pq_path,
                                file.info(csv_path)$size / 1e6,
                                file.info(pq_path)$size / 1e6))
    TRUE
  }, error = function(e) {
    log4r::error(logger, sprintf("FAIL %s: %s", csv_path, conditionMessage(e)))
    if (file.exists(pq_path)) unlink(pq_path)
    FALSE
  })
}

# ---- Orchestration -----------------------------------------------------------

#' Walk a tree, write a pinned-schema Parquet next to every CSV found.
#'
#' Two passes so the cross-vintage schema is derived once and applied to all:
#'   1. read each CSV's per-column classes; reduce per schema-family to the
#'      pinned type of every drifting column (logged as a drift inventory).
#'   2. write each CSV's parquet, casting its drifting columns to the pin.
#'
#' @param processed_root directory to walk. Defaults to PATHS$processed.
#' @param overwrite if TRUE, regenerate parquets even if newer than their csv.
run_parquet <- function(processed_root = PATHS$processed,
                        overwrite = FALSE,
                        workers = NULL) {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "09_parquet_log.txt"))

  if (!dir.exists(processed_root)) {
    log4r::warn(logger, sprintf("processed_root does not exist: %s", processed_root))
    return(invisible(NULL))
  }

  csvs <- list.files(processed_root, pattern = "\\.csv$", recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  n_workers <- resolve_workers("NCCS_PARQUET_WORKERS", workers)
  log4r::info(logger, sprintf("Walking %s: %d CSVs, %d worker(s)",
                              processed_root, length(csvs), n_workers))
  if (length(csvs) == 0L) {
    log4r::info(logger, "=== run_parquet done: 0 ok, 0 failed ===")
    return(invisible(list(n_ok = 0L, n_fail = 0L)))
  }

  groups <- vapply(csvs, .schema_group_key, character(1L), USE.NAMES = FALSE)

  # ---- Pass 1: derive the pinned schema per family (once) --------------------
  class_list <- parallel_map(as.list(csvs), function(p) {
    cv <- tryCatch(.fread_classes(p), error = function(e) NULL)
    if (!is.null(cv)) attr(cv, "stem") <- basename(p)
    cv
  }, n_workers)

  pinned_by_group <- list()
  for (g in unique(groups)) {
    cvs <- class_list[groups == g]
    cvs <- cvs[!vapply(cvs, is.null, logical(1L))]
    if (length(cvs) < 2L) next                         # need >=2 vintages to drift
    pinned <- .pin_family_types(cvs)
    if (length(pinned) == 0L) next
    pinned_by_group[[g]] <- pinned
    for (line in .describe_drift(cvs, pinned))
      log4r::info(logger, sprintf("DRIFT [%s] %s", g, line))
  }
  n_drift_cols <- sum(vapply(pinned_by_group, length, integer(1L)))
  log4r::info(logger, sprintf(
    "Pinned schema across %d families: %d column(s) widened for cross-vintage stability",
    length(pinned_by_group), n_drift_cols))

  # ---- Pass 2: write each parquet cast to its family's pinned schema ---------
  tasks <- Map(function(p, g) list(path = p, pinned = pinned_by_group[[g]]),
               csvs, groups)
  results <- parallel_map(unname(tasks), function(t) {
    .csv_to_parquet(t$path, pinned = t$pinned %||% list(),
                    overwrite = overwrite, logger = logger)
  }, n_workers)

  ok_mask <- vapply(results, isTRUE, logical(1L))
  n_ok <- sum(ok_mask); n_fail <- length(results) - n_ok
  log4r::info(logger, sprintf("=== run_parquet done: %d ok, %d failed ===",
                              n_ok, n_fail))
  invisible(list(n_ok = n_ok, n_fail = n_fail))
}

if (sys.nframe() == 0L) run_parquet()
