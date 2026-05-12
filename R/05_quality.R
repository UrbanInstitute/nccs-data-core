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

# CROSSWALK_FOR_SERIES is defined in R/data.R (sourced above) — the 990combined
# series shares the 990 crosswalk because its schema is the 990 + 990-EZ
# intersect (53 cols), all of which appear in the 990 crosswalk.

# Baseline strategy: snapshot-on-each-run. Before a run starts, every
# `quality_{form}_{tax_year}.rds` is renamed to `.prev.rds`, overwriting any
# previous baseline. The current run then writes fresh `.rds` files; post_checks
# compares against the `.prev.rds` snapshot. Only one historical step is kept —
# enough to catch deploy-time accidents (data loss, double-ingest), not
# long-term drift.

REPORT_PATH <- function(form, tax_year) {
  file.path(PATHS$logs, sprintf("quality_%s_%d.rds", form, tax_year))
}

BASELINE_PATH <- function(form, tax_year) {
  file.path(PATHS$logs, sprintf("quality_%s_%d.prev.rds", form, tax_year))
}

#' Promote current quality RDS reports to the `.prev.rds` baseline slot, so
#' this run can compare its output against the previous run's output.
snapshot_prior_reports <- function(logger = NULL) {
  current <- list.files(PATHS$logs, pattern = "^quality_.*[^v]\\.rds$",
                        full.names = TRUE)
  current <- current[!grepl("\\.prev\\.rds$", current)]
  if (length(current) == 0L) {
    if (!is.null(logger)) log4r::info(logger, "No prior reports to snapshot (first run)")
    return(invisible(NULL))
  }
  for (f in current) {
    dest <- sub("\\.rds$", ".prev.rds", f)
    file.rename(f, dest)
  }
  if (!is.null(logger)) {
    log4r::info(logger, sprintf("Snapshotted %d prior reports to .prev.rds baseline",
                                length(current)))
  }
  invisible(NULL)
}

#' @param strict logical: hard-failure on schema/EIN/type errors halts the run.
run_quality <- function(harmonized_root = PATHS$harmonized,
                        forms = c("990", "990ez", "990pf", "990combined"),
                        strict = TRUE) {

  dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(PATHS$logs, "05_quality_log.txt"))

  snapshot_prior_reports(logger)

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
