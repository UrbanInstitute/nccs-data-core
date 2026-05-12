# R/07_render_report.R
# Phase 7b: render docs/quality_report_template.qmd per (form, tax_year) using
# the RDS reports produced in Phase 6. Output:
#   data/processed/{tax_year}/{form}/core_{tax_year}_{form}_quality.html
#
# Renders run in parallel via parallel::mclapply. Each render is isolated by
# copying the template to a unique path in tempdir() — Quarto keys its
# .quarto/ intermediate cache off the input file's stem, so a shared template
# path would cause concurrent renders to collide on cache / lock files.
# The template's format block (theme: cosmo, embed-resources: true) is fully
# self-sufficient, so rendering outside the docs/ Quarto project produces
# identical HTML to rendering inside it.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(parallel)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))

TEMPLATE_PATH <- here::here("docs", "quality_report_template.qmd")

#' Resolve the worker count for parallel rendering.
#'
#' Order of precedence:
#'   1. NCCS_RENDER_WORKERS environment variable (if set and parseable as a
#'      positive integer) — overrides the function arg, useful for tuning
#'      cron jobs without code changes.
#'   2. `workers` function arg (if non-NULL and positive).
#'   3. Default: min(detectCores() - 1, 8), floor 1. The cap exists because
#'      Quarto render time is dominated by subprocess startup, not CPU, so
#'      returns diminish past ~8 workers.
resolve_render_workers <- function(workers = NULL) {
  env_val <- Sys.getenv("NCCS_RENDER_WORKERS", unset = NA_character_)
  if (!is.na(env_val) && nzchar(env_val)) {
    n <- suppressWarnings(as.integer(env_val))
    if (!is.na(n) && n >= 1L) return(n)
  }
  if (!is.null(workers)) {
    n <- suppressWarnings(as.integer(workers))
    if (!is.na(n) && n >= 1L) return(n)
  }
  max(1L, min(parallel::detectCores() - 1L, 8L))
}

#' Render a single quality report. Isolates Quarto's intermediate cache by
#' copying the template to a unique tempdir path before rendering, so
#' concurrent calls do not collide on `.quarto/<template-stem>/`.
render_one_report <- function(rds_path, out_path, logger = NULL) {
  out_dir <- dirname(out_path)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Per-render template copy: tempfile() returns a unique path per call within
  # an R process and per process — safe across forked mclapply workers.
  tpl_copy <- tempfile(pattern = "quality_report_", fileext = ".qmd")
  file.copy(TEMPLATE_PATH, tpl_copy, overwrite = TRUE)
  on.exit(unlink(tpl_copy), add = TRUE)

  out_basename <- paste0(tools::file_path_sans_ext(basename(tpl_copy)), ".html")
  rendered_at  <- file.path(dirname(tpl_copy), out_basename)

  ok <- tryCatch({
    quarto::quarto_render(
      input          = tpl_copy,
      output_format  = "html",
      execute_params = list(report_data_path = normalizePath(rds_path)),
      quiet          = TRUE
    )
    TRUE
  }, error = function(e) {
    if (!is.null(logger)) {
      log4r::error(logger, sprintf("Render failed for %s: %s",
                                   basename(rds_path), conditionMessage(e)))
    }
    FALSE
  })

  if (!isTRUE(ok)) return(invisible(FALSE))

  if (!file.exists(rendered_at)) {
    if (!is.null(logger)) {
      log4r::error(logger, sprintf("Render produced no output for %s", rds_path))
    }
    return(invisible(FALSE))
  }
  # file.rename() fails silently on cross-filesystem moves (EXDEV), which
  # happens when tempdir() resolves to tmpfs (/tmp) and the repo lives on a
  # different mount — common inside Docker containers on EC2. Fall back to
  # copy+delete so the move is robust regardless of mount topology.
  moved <- file.rename(rendered_at, out_path)
  if (!isTRUE(moved)) {
    moved <- file.copy(rendered_at, out_path, overwrite = TRUE)
    if (isTRUE(moved)) unlink(rendered_at)
  }
  if (!isTRUE(moved) || !file.exists(out_path)) {
    if (!is.null(logger)) {
      log4r::error(logger, sprintf("Failed to place render output at %s", out_path))
    }
    unlink(rendered_at)
    return(invisible(FALSE))
  }
  if (!is.null(logger)) log4r::info(logger, sprintf("WROTE %s", out_path))
  invisible(TRUE)
}

run_render_reports <- function(logs_dir       = PATHS$logs,
                               processed_root = PATHS$processed,
                               workers        = NULL) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("'quarto' R package is required. Install with install.packages('quarto')")
  }
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(logs_dir, "07_render_report_log.txt"))

  rds_files <- list.files(logs_dir, pattern = "^quality_[^.]+\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0L) {
    log4r::warn(logger, "No quality RDS reports found; run R/05_quality.R first.")
    return(invisible(NULL))
  }

  # Build the task list up front so failures parsing one filename don't abort
  # the whole run.
  tasks <- list()
  for (rds in rds_files) {
    base <- sub("^quality_", "", sub("\\.rds$", "", basename(rds)))
    parts <- regmatches(base, regexec("^(.+)_([0-9]{4})$", base))[[1]]
    if (length(parts) != 3L) {
      log4r::warn(logger, sprintf("Skipping un-parseable RDS name: %s", basename(rds)))
      next
    }
    form     <- parts[2]
    tax_year <- as.integer(parts[3])
    out_path <- file.path(processed_root, tax_year, form,
                          sprintf("core_%d_%s_quality.html", tax_year, form))
    tasks[[length(tasks) + 1L]] <- list(rds = rds, out = out_path)
  }

  n_workers <- resolve_render_workers(workers)
  log4r::info(logger, sprintf("Rendering %d reports with %d worker(s)",
                              length(tasks), n_workers))

  render_task <- function(task) {
    render_one_report(task$rds, task$out, logger = logger)
  }

  results <- if (n_workers <= 1L) {
    lapply(tasks, render_task)
  } else {
    # mc.preschedule = FALSE forks per task, so a Quarto/pandoc subprocess
    # misbehavior in one render does not corrupt a worker that still has
    # pending tasks. The per-task fork overhead (sub-second) is negligible
    # compared to the 5-7s typical Quarto render time.
    parallel::mclapply(tasks, render_task,
                       mc.cores = n_workers,
                       mc.preschedule = FALSE)
  }

  # Defensive: mclapply can return "try-error" objects in pathological cases
  # that escape the inner tryCatch (e.g. a worker SIGKILL). Treat anything
  # not literally TRUE as a failure.
  n_ok <- sum(vapply(results, isTRUE, logical(1)))
  n_fail <- length(results) - n_ok
  if (n_fail > 0L) {
    log4r::warn(logger, sprintf("Render run complete: %d HTML reports written, %d failed",
                                n_ok, n_fail))
  } else {
    log4r::info(logger, sprintf("Render run complete: %d HTML reports written", n_ok))
  }
  invisible(NULL)
}

if (sys.nframe() == 0L) run_render_reports()
