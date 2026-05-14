# R/quality/post_checks.R
# Post-harmonization validation, per (tax_year, form). Per IMPLEMENTATION_PLAN.md §7.
#
# Hard checks (abort if STRICT_QUALITY_GATES and any fail):
#   - schema: every harmonized col from FINAL crosswalk present
#   - EIN format: hyphenated XX-XXXXXXX, no NA, no duplicates
#   - tax_period format: 6 chars, parseable, tax_year in [1989, current+1]
#   - subsection_cd: in KNOWN_SUBSECTION_CODES
#   - type: numeric / logical / integer per declaration
#
# Soft checks (always warn, never abort):
#   - row-count plausibility: within ±20% of prior-year baseline (skipped if no baseline)
#   - null-rate historical bounds (skipped if no baseline)
#
# Returns the report-data list described in plan §7.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

source(here::here("R", "utils.R"))
source(here::here("R", "quality", "stat_helpers.R"))

EIN_FORMAT_REGEX     <- "^[0-9]{2}-[0-9]{7}$"
TAX_PERIOD_REGEX     <- "^[0-9]{6}$"
ROW_COUNT_TOLERANCE  <- 0.20   # ±20% vs prior year (soft)

# Universal pipeline columns expected in every output regardless of vintage.
UNIVERSAL_COLS <- c("ein", "tax_period", "tax_year", "tax_month", "subsection_cd",
                    "is_501c3", "extract_year", "is_amendment", "soi_year",
                    "source_form")

#' Parse a "years_present" cell from a crosswalk row into an integer vector.
#' Handles "2012", "2012-2024", "2017,2020-2022".
parse_years_present <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(integer(0))
  parts <- trimws(strsplit(s, ",")[[1]])
  unlist(lapply(parts, function(p) {
    if (grepl("-", p, fixed = TRUE)) {
      rng <- suppressWarnings(as.integer(strsplit(p, "-", fixed = TRUE)[[1]]))
      if (any(is.na(rng))) return(integer(0))
      seq.int(rng[1], rng[2])
    } else {
      as.integer(p)
    }
  }))
}

#' Harmonized column names expected for (source-form, extract_year) per the
#' FINAL crosswalk's `years_present` field. Crosswalk synonyms collapse: a
#' harmonized name is expected if ANY of its source vars covers extract_year.
expected_harmonized_cols <- function(xwalk, extract_year) {
  if (!"years_present" %in% names(xwalk)) return(unique(xwalk$harmonized_name))
  in_year <- vapply(xwalk$years_present,
                    function(s) extract_year %in% parse_years_present(s),
                    logical(1))
  unique(xwalk$harmonized_name[in_year])
}

#' Vintage-aware overall completeness. Computes per-cohort completeness over
#' the harmonized columns expected for that cohort's (extract_year, source_form)
#' vintage, then row-count-weights across cohorts.
compute_vintage_aware_completeness <- function(dt, form) {
  if (!"extract_year" %in% names(dt)) {
    return(list(overall = NA_real_, per_cohort = list()))
  }
  is_combined <- identical(form, "990combined")
  if (is_combined && !"source_form" %in% names(dt)) {
    return(list(overall = NA_real_, per_cohort = list()))
  }

  shared_cols <- if (is_combined) {
    x990   <- fread(CROSSWALK_FILES[["990"]])
    x990ez <- fread(CROSSWALK_FILES[["990ez"]])
    intersect(unique(x990$harmonized_name), unique(x990ez$harmonized_name))
  } else NULL

  cohorts <- if (is_combined) unique(dt[, .(extract_year, source_form)])
             else             unique(dt[, .(extract_year)])[, source_form := form][]

  per_cohort <- list()
  for (i in seq_len(nrow(cohorts))) {
    ey <- cohorts$extract_year[i]
    sf <- cohorts$source_form[i]
    if (is.na(ey) || is.na(sf)) next
    sub <- if (is_combined) dt[extract_year == ey & source_form == sf]
           else             dt[extract_year == ey]
    if (nrow(sub) == 0L) next

    xw_path <- CROSSWALK_FILES[[sf]]
    if (is.null(xw_path)) next
    xw <- fread(xw_path)
    expected <- expected_harmonized_cols(xw, ey)
    if (is_combined) expected <- intersect(expected, shared_cols)
    expected <- intersect(union(expected, UNIVERSAL_COLS), names(sub))
    if (length(expected) == 0L) next

    # is_blank() comes from R/utils.R, sourced at the top of this file.
    comps <- vapply(expected,
                    function(c) 100 * sum(!is_blank(sub[[c]])) / nrow(sub),
                    numeric(1))
    per_cohort[[length(per_cohort) + 1L]] <- list(
      extract_year     = ey,
      source_form      = sf,
      n_rows           = nrow(sub),
      n_expected_cols  = length(expected),
      completeness_pct = round(mean(comps), 2)
    )
  }

  if (length(per_cohort) == 0L) return(list(overall = NA_real_, per_cohort = list()))

  weights <- vapply(per_cohort, function(x) x$n_rows, numeric(1))
  comps   <- vapply(per_cohort, function(x) x$completeness_pct, numeric(1))
  list(overall = round(weighted.mean(comps, weights), 2),
       per_cohort = per_cohort)
}

# ---- Crosswalk-driven category derivation ----

#' Parse `location` from FINAL crosswalk into a coarse category key.
#' Examples:
#'   "990 Core_Pt VIII-1A"        -> "part_viii_revenue"
#'   "990 Sch A_Pt II-4(f)"       -> "sched_a"
#'   "Form 990 Header"            -> "header"
#'   "990-PF Pt I-1(a)"           -> "part_i"
location_to_category <- function(loc) {
  if (is.na(loc) || !nzchar(loc)) return("other")
  l <- tolower(loc)
  if (grepl("header|item a|top of form|basic info|classification", l))   return("header")
  if (grepl("sch(?:edule)? a", l))                                       return("sched_a")
  if (grepl("sch(?:edule)? b", l))                                       return("sched_b")
  # SOI-current style: "Pt II-4(f)"; legacy style: "Part II", "Part XIII"
  m <- regmatches(l, regexec("(?:pt|part) ([ivx]+|\\d+)", l))[[1]]
  if (length(m) == 2L && nzchar(m[2])) return(sprintf("part_%s", m[2]))
  "other"
}

#' Build a {category -> {col_name -> col_type}} map from a FINAL crosswalk + dt.
#' col_type inferred from the harmonized data.table's actual column class.
build_categories <- function(dt, xwalk_path) {
  xw <- fread(xwalk_path)
  # Legacy crosswalks expose category context in `section` instead of `location`.
  if (!"location" %in% names(xw) && "section" %in% names(xw)) {
    xw[, location := section]
  }
  xw[, category := vapply(location, location_to_category, character(1))]
  xw <- xw[harmonized_name %in% names(dt)]

  type_of <- function(x) {
    if (is.logical(x))   return("boolean")
    if (is.numeric(x))   return("numeric")
    if (is.integer(x))   return("numeric")
    if (is.character(x)) return("character")
    "other"
  }
  by_cat <- split(xw, xw$category)
  lapply(by_cat, function(rows) {
    cols <- unique(rows$harmonized_name)
    setNames(vapply(cols, function(c) type_of(dt[[c]]), character(1)), cols)
  })
}

# ---- Hard validators ----

check_schema <- function(dt, xwalk_path, form = NULL) {
  xw <- fread(xwalk_path)
  # Legacy crosswalks include rows with harmonized_name="" (BMF-origin drops);
  # those columns are filtered out at harmonize time and aren't part of the
  # output schema. Filter them out here too so they don't show up as "missing".
  if ("harmonized_name" %in% names(xw)) {
    xw <- xw[!is.na(harmonized_name) & harmonized_name != ""]
  }
  required <- unique(xw$harmonized_name)
  # SOI-current 990combined intentionally projects to a 53-col intersect of
  # 990 ∩ 990ez. Legacy 990combined uses the full legacy_pz schema (no
  # intersect — it's its own crosswalk, not a derivation).
  if (identical(form, "990combined") && !is_legacy_crosswalk_path(xwalk_path)) {
    x990ez <- fread(CROSSWALK_FILES[["990ez"]])
    required <- intersect(required, unique(x990ez$harmonized_name))
  }
  missing <- setdiff(required, names(dt))
  extras  <- setdiff(names(dt), c(required,
                "tax_year", "tax_month", "is_501c3", "extract_year",
                "is_amendment", "source_form", "soi_year",
                "source_subsection_class"))
  list(passed = length(missing) == 0L, missing = missing, extras = extras)
}

check_ein_format <- function(dt) {
  v <- dt$ein
  malformed  <- !is.na(v) & !grepl(EIN_FORMAT_REGEX, v)
  null_count <- sum(is.na(v))
  # Duplicates are legitimate (same EIN + tax_period for an amendment), so this
  # is reported but does NOT contribute to `passed`. Only format + nulls are hard.
  dup_count  <- nrow(dt) - data.table::uniqueN(v[!is.na(v)])
  list(
    passed          = sum(malformed) == 0L && null_count == 0L,
    malformed       = sum(malformed),
    null_count      = null_count,
    duplicate_count = dup_count
  )
}

check_tax_period <- function(dt) {
  v <- dt$tax_period
  malformed <- !is.na(v) & !grepl(TAX_PERIOD_REGEX, v)
  yr <- suppressWarnings(as.integer(substr(v, 1L, 4L)))
  bounds <- tax_year_range()
  out_of_range <- !is.na(yr) & (yr < bounds[1] | yr > bounds[2])
  list(
    passed = sum(malformed) == 0L && sum(out_of_range) == 0L,
    malformed_count    = sum(malformed),
    out_of_range_count = sum(out_of_range)
  )
}

check_subsection_cd <- function(dt) {
  v <- dt$subsection_cd
  unknown <- !is.na(v) & !(v %in% KNOWN_SUBSECTION_CODES)
  list(passed = sum(unknown) == 0L, unknown_count = sum(unknown))
}

check_column_types <- function(dt) {
  issues <- character(0)
  if (!is.character(dt$ein))           issues <- c(issues, "ein is not character")
  if (!is.character(dt$tax_period))    issues <- c(issues, "tax_period is not character")
  if (!is.integer(dt$tax_year))        issues <- c(issues, "tax_year is not integer")
  if (!is.integer(dt$tax_month))       issues <- c(issues, "tax_month is not integer")
  if (!is.integer(dt$subsection_cd))   issues <- c(issues, "subsection_cd is not integer")
  if (!is.logical(dt$is_501c3))        issues <- c(issues, "is_501c3 is not logical")
  if (any(is.na(dt$is_501c3)))         issues <- c(issues, "is_501c3 contains NA (must be strict boolean)")
  list(passed = length(issues) == 0L, issues = issues)
}

# ---- Soft validators ----

check_row_count_vs_prior <- function(n_rows, baseline_path) {
  if (is.null(baseline_path) || !file.exists(baseline_path)) {
    return(list(status = "no_baseline", prior = NA_integer_,
                delta_pct = NA_real_, within_bounds = NA))
  }
  prior <- readRDS(baseline_path)$row_count
  if (is.null(prior) || prior == 0L) {
    return(list(status = "invalid_baseline", prior = prior, delta_pct = NA_real_, within_bounds = NA))
  }
  delta <- (n_rows - prior) / prior
  list(status        = if (abs(delta) <= ROW_COUNT_TOLERANCE) "ok" else "outside_bounds",
       prior         = prior,
       delta_pct     = round(100 * delta, 2),
       within_bounds = abs(delta) <= ROW_COUNT_TOLERANCE)
}

# ---- Summary stats ----

financial_summary <- function(dt) {
  pick <- function(name) if (name %in% names(dt) && is.numeric(dt[[name]])) dt[[name]] else NA_real_
  totals <- list(
    revenue  = pick("total_revenue"),
    expenses = pick("total_expenses"),
    assets   = pick("total_assets_eoy")
  )
  lapply(totals, function(v) {
    if (all(is.na(v))) return(list(total = NA, median = NA))
    list(total = sum(v, na.rm = TRUE), median = median(v, na.rm = TRUE))
  })
}

subsection_distribution <- function(dt, top_n = 10L) {
  if (!"subsection_cd" %in% names(dt)) return(list())
  calc_code_stats(dt$subsection_cd, top_n = top_n)
}

tax_period_year_distribution <- function(dt) {
  if (!"tax_year" %in% names(dt)) return(list())
  as.list(table(dt$tax_year, useNA = "ifany"))
}

# ---- Main entry point ----

#' Run all post-checks for one harmonized (tax_year, form) file.
#' @param dt data.table (harmonized output)
#' @param form character: "990" | "990ez" | "990pf" | "990combined"
#' @param tax_year integer
#' @param xwalk_path path to FINAL crosswalk for this form
#' @param baseline_path optional path to prior-year RDS report for soft comparisons
#' @param strict logical: if TRUE, returned `passed = FALSE` causes the caller to abort
#' @param logger optional log4r logger
#' @return list with the report-data structure described in IMPLEMENTATION_PLAN.md §7
run_post_checks <- function(dt, form, tax_year, xwalk_path,
                            baseline_path = NULL, strict = TRUE, logger = NULL) {

  n_rows <- nrow(dt)
  n_cols <- ncol(dt)

  schema <- check_schema(dt, xwalk_path, form = form)
  ein    <- check_ein_format(dt)
  tp     <- check_tax_period(dt)
  sub    <- check_subsection_cd(dt)
  types  <- check_column_types(dt)

  hard_passed <- schema$passed && ein$passed && tp$passed && sub$passed && types$passed

  yoy <- check_row_count_vs_prior(n_rows, baseline_path)

  # Per-column completeness (raw, all-rows / all-cols)
  col_comp <- lapply(names(dt), function(nm) {
    x <- dt[[nm]]
    stats <- calc_completeness_stats(x, n_rows)
    stats$source_columns <- NA_character_
    stats
  })
  names(col_comp) <- names(dt)
  overall_completeness_raw <- round(mean(sapply(col_comp,
                                                function(c) c$completeness_pct)), 2)

  # Vintage-aware completeness (default headline metric): per-cohort completeness
  # over only the columns expected for that (extract_year, source_form) vintage,
  # then row-count-weighted across cohorts.
  vintage_completeness <- compute_vintage_aware_completeness(dt, form)
  overall_completeness <- vintage_completeness$overall

  # Category reports from crosswalk location
  cats <- build_categories(dt, xwalk_path)
  category_reports <- if (length(cats)) {
    setNames(lapply(names(cats),
                    function(k) generate_category_report(dt, k, cats[[k]])),
             names(cats))
  } else list()

  # Source column map + descriptions from crosswalk.
  # Legacy crosswalks use `source_column`; SOI-current uses `source_var`.
  xw <- fread(xwalk_path)
  src_col <- if ("source_var" %in% names(xw)) "source_var" else "source_column"
  source_column_map  <- split(xw[[src_col]], xw$harmonized_name)
  column_descriptions <- setNames(xw$description, xw$harmonized_name)

  critical_field_issues <- list()
  if (!ein$passed)  critical_field_issues$ein         <- ein
  if (!tp$passed)   critical_field_issues$tax_period  <- tp
  if (!sub$passed)  critical_field_issues$subsection  <- sub
  if (!types$passed) critical_field_issues$types      <- types

  report <- list(
    passed               = if (strict) hard_passed else TRUE,
    strict_mode          = strict,
    hard_passed          = hard_passed,
    timestamp            = Sys.time(),
    form                 = form,
    tax_year             = tax_year,
    row_count            = n_rows,
    column_count         = n_cols,
    overall_completeness        = overall_completeness,
    overall_completeness_raw    = overall_completeness_raw,
    completeness_by_cohort      = vintage_completeness$per_cohort,
    row_preservation     = NA,   # filled in by validate_step caller if mid-pipeline
    summary_stats = list(
      unique_eins                   = data.table::uniqueN(dt$ein[!is.na(dt$ein)]),
      duplicate_eins                = ein$duplicate_count,
      subsection_distribution       = subsection_distribution(dt),
      financial                     = financial_summary(dt),
      tax_period_year_distribution  = tax_period_year_distribution(dt),
      year_over_year_delta          = yoy
    ),
    column_completeness   = col_comp,
    category_reports      = category_reports,
    source_column_map     = source_column_map,
    column_descriptions   = column_descriptions,
    missing_columns       = schema$missing,
    critical_field_issues = critical_field_issues,
    extra_columns         = schema$extras
  )

  if (!is.null(logger)) {
    log4r::info(logger, sprintf("post_checks %s/%d: rows=%s cols=%d completeness=%.1f%% hard_passed=%s yoy=%s",
                                form, tax_year, format(n_rows, big.mark = ","),
                                n_cols, overall_completeness, hard_passed, yoy$status))
    if (!schema$passed) log4r::error(logger, sprintf("  schema: missing %d cols (%s)",
                                                     length(schema$missing),
                                                     paste(head(schema$missing, 5), collapse = ", ")))
    if (!ein$passed)    log4r::error(logger, sprintf("  ein: malformed=%d null=%d dup=%d",
                                                     ein$malformed, ein$null_count, ein$duplicate_count))
    if (!tp$passed)     log4r::error(logger, sprintf("  tax_period: malformed=%d out_of_range=%d",
                                                     tp$malformed_count, tp$out_of_range_count))
    if (!sub$passed)    log4r::error(logger, sprintf("  subsection_cd: unknown_count=%d", sub$unknown_count))
    if (!types$passed)  log4r::error(logger, sprintf("  types: %s", paste(types$issues, collapse = "; ")))
    if (yoy$status == "outside_bounds") log4r::warn(logger,
        sprintf("  yoy delta %.1f%% outside ±%.0f%% (prior=%d)",
                yoy$delta_pct, ROW_COUNT_TOLERANCE * 100, yoy$prior))
  }

  report
}
