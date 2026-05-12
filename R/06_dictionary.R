# R/06_dictionary.R
# Phase 7a: auto-generate a per-output data dictionary CSV at
# data/processed/{tax_year}/{form}/core_{tax_year}_{form}_dictionary.csv.
# Per IMPLEMENTATION_PLAN.md §8 column set.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "utils.R"))
source(here("R", "create_logger.R"))

# Universal pipeline-added columns that don't have a crosswalk row but should
# still appear in the per-output dictionary. Descriptions are pipeline-owned.
UNIVERSAL_DICTIONARY_ROWS <- function() {
  data.table(
    harmonized_name = c("tax_year", "tax_month", "is_501c3",
                       "extract_year", "is_amendment", "source_form", "soi_year"),
    description = c(
      "Calendar year the filing's fiscal period ended (substr(tax_period, 1, 4)).",
      "Month the filing's fiscal period ended (substr(tax_period, 5, 6)).",
      "TRUE if subsection_cd == 3, else FALSE. Strict boolean, never NA.",
      "Processing year of the IRS SOI extract that supplied this row.",
      "TRUE if the same (ein, tax_period) appeared in an earlier extract_year for this series.",
      "990combined only: '990' or '990ez' indicating which source form this row came from.",
      "990-PF only: IRS-assigned SOI year (calendar year covering most of activity)."
    ),
    source_var      = NA_character_,
    source_location = "pipeline-derived",
    years_present   = "all"
  )
}

infer_dictionary_type <- function(x) {
  if (is.logical(x))                                            return("boolean")
  if (is.integer(x))                                            return("integer")
  if (is.numeric(x))                                            return("numeric")
  if (is.character(x))                                          return("character")
  "other"
}

#' Build the dictionary CSV for one harmonized file.
build_dictionary_one <- function(dt, form, tax_year, xwalk_path, logger = NULL) {
  xw <- fread(xwalk_path)
  # Collapse synonyms: harmonized_name -> {source_var(s), description, location, years_present}
  xw_collapsed <- xw[, .(
    source_var      = paste(unique(source_var), collapse = "|"),
    description     = first(description),
    source_location = first(location),
    years_present   = paste(unique(years_present), collapse = "|")
  ), by = harmonized_name]
  setnames(xw_collapsed, "source_location", "source_location")

  universal <- UNIVERSAL_DICTIONARY_ROWS()
  setnames(universal, c("source_location"), c("source_location"))
  # Restrict universal rows to those actually present in dt
  universal <- universal[harmonized_name %in% names(dt)]

  # Per-column stats from the actual data
  per_col <- rbindlist(lapply(names(dt), function(nm) {
    x <- dt[[nm]]
    is_num <- is.numeric(x)
    blanks <- is_blank(x)
    n_nonnull <- sum(!blanks)
    data.table(
      harmonized_name = nm,
      data_type       = infer_dictionary_type(x),
      n_rows          = length(x),
      n_nonnull       = n_nonnull,
      null_pct        = round(100 * (1 - n_nonnull / length(x)), 2),
      n_distinct      = data.table::uniqueN(x[!blanks]),
      min_value       = if (is_num) suppressWarnings(min(x, na.rm = TRUE)) else NA_real_,
      max_value       = if (is_num) suppressWarnings(max(x, na.rm = TRUE)) else NA_real_
    )
  }))

  # Join metadata onto stats
  meta <- rbindlist(list(
    xw_collapsed[, .(harmonized_name, description, source_var, source_location, years_present)],
    universal[,    .(harmonized_name, description, source_var, source_location, years_present)]
  ))
  out <- meta[per_col, on = "harmonized_name"]
  setcolorder(out, c("harmonized_name", "description", "source_var",
                     "source_location", "data_type", "n_rows", "n_nonnull",
                     "null_pct", "n_distinct", "min_value", "max_value",
                     "years_present"))
  # Replace -Inf/Inf (from numeric all-NA) with NA for cleaner CSV
  out[is.infinite(min_value), min_value := NA_real_]
  out[is.infinite(max_value), max_value := NA_real_]
  out
}

run_dictionary <- function(harmonized_root = PATHS$harmonized,
                           processed_root  = PATHS$processed,
                           forms = c("990", "990ez", "990pf", "990combined")) {
  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "06_dictionary_log.txt"))

  tax_year_dirs <- sort(list.dirs(harmonized_root, recursive = FALSE, full.names = TRUE))
  n <- 0L
  for (yd in tax_year_dirs) {
    tax_year <- as.integer(basename(yd))
    for (form in forms) {
      f <- file.path(yd, form, sprintf("core_%d_%s.csv", tax_year, form))
      if (!file.exists(f)) next
      dt <- fread(f, colClasses = c(ein = "character", tax_period = "character"))
      dict <- build_dictionary_one(dt, form, tax_year, CROSSWALK_FOR_SERIES(form), logger)

      out_dir <- file.path(processed_root, tax_year, form)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_path <- file.path(out_dir, sprintf("core_%d_%s_dictionary.csv", tax_year, form))
      fwrite(dict, out_path)
      n <- n + 1L
      log4r::info(logger, sprintf("WROTE %s (%d rows)", out_path, nrow(dict)))
    }
  }
  log4r::info(logger, sprintf("Dictionary run complete: %d files written", n))
  invisible(NULL)
}

if (sys.nframe() == 0L) run_dictionary()
