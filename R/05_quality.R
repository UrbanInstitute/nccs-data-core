# R/05_quality.R
# Phase 6: run post-checks on every harmonized (tax_year, form) file and save
# the report RDS at data/logs/quality_{form}_{tax_year}.rds for downstream
# rendering (Phase 7).
# Pre-checks are also available (R/quality/pre_checks.R::run_pre_checks_one) and
# can be wired into the harmonize step or invoked separately on unpacked sources.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))
source(here("R", "quality", "pre_checks.R"))
source(here("R", "quality", "post_checks.R"))

# Crosswalk to use for each output series. 990combined uses 990's crosswalk
# (the shared schema is its 53-col intersect with 990ez; 990's row covers all
# of those harmonized names).
CROSSWALK_FOR_SERIES <- function(form) {
  switch(form,
    "990"         = CROSSWALK_FILES[["990"]],
    "990ez"       = CROSSWALK_FILES[["990ez"]],
    "990pf"       = CROSSWALK_FILES[["990pf"]],
    "990combined" = CROSSWALK_FILES[["990"]],
    stop(sprintf("No crosswalk for form '%s'", form))
  )
}

# Baseline = the SAME (form, tax_year) report from a prior pipeline run,
# archived elsewhere. Comparing different tax years against each other is
# apples-to-oranges (different filer cohorts) and was an earlier bug.
# Until we have run-archive infrastructure, this returns NULL and YoY check
# reports `status = "no_baseline"`.
BASELINE_PATH <- function(form, tax_year) NULL

REPORT_PATH <- function(form, tax_year) {
  file.path(PATHS$logs, sprintf("quality_%s_%d.rds", form, tax_year))
}

#' @param strict logical: hard-failure on schema/EIN/type errors halts the run.
run_quality <- function(harmonized_root = PATHS$harmonized,
                        forms = c("990", "990ez", "990pf", "990combined"),
                        strict = TRUE) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "05_quality_log.txt"))

  tax_year_dirs <- sort(list.dirs(harmonized_root, recursive = FALSE, full.names = TRUE))

  any_hard_failure <- FALSE
  n_reports <- 0L

  for (yd in tax_year_dirs) {
    tax_year <- as.integer(basename(yd))
    for (form in forms) {
      f <- file.path(yd, form, sprintf("core_%d_%s.csv", tax_year, form))
      if (!file.exists(f)) next

      dt <- fread(f, colClasses = c(ein = "character", tax_period = "character"))
      report <- run_post_checks(
        dt            = dt,
        form          = form,
        tax_year      = tax_year,
        xwalk_path    = CROSSWALK_FOR_SERIES(form),
        baseline_path = BASELINE_PATH(form, tax_year),
        strict        = strict,
        logger        = logger
      )
      saveRDS(report, REPORT_PATH(form, tax_year))
      n_reports <- n_reports + 1L
      if (!report$hard_passed) any_hard_failure <- TRUE
    }
  }

  log4r::info(logger, sprintf("Quality run complete: %d reports written; any_hard_failure=%s",
                              n_reports, any_hard_failure))
  if (strict && any_hard_failure) {
    stop("STRICT_QUALITY_GATES: hard-failure check did not pass. See data/logs/05_quality_log.txt for details.")
  }
  invisible(NULL)
}

if (sys.nframe() == 0L) run_quality(strict = CONFIG$STRICT_QUALITY_GATES)
