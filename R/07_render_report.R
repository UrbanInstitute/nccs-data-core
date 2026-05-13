# R/07_render_report.R
# Phase 7b: render docs/quality_report_template.qmd per (form, tax_year) using
# the RDS reports produced in Phase 6. Output:
#   docs/quality-reports/{tax_year}/{form}/core_{tax_year}_{form}_quality.html
#
# Renders run in parallel via parallel::mclapply. Each render is isolated by
# copying the template to a unique path in tempdir() — Quarto keys its
# .quarto/ intermediate cache off the input file's stem, so a shared template
# path would cause concurrent renders to collide on cache / lock files.
#
# The template uses embed-resources: false. After all renders complete, we
# dedupe: copy one render's <stem>_files/libs/ tree to docs/quality-reports/
# _libs/ and rewrite every HTML's per-render asset references to point to it.
# Without dedup, each report bloats to ~1.95 MB (mostly an inlined Source
# Sans Pro TTF); with sharing, each is ~51 KB and one shared _libs/ is
# ~912 KB total.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(parallel)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))

TEMPLATE_PATH        <- here::here("docs", "quality_report_template.qmd")
QUALITY_REPORTS_ROOT <- here::here("docs", "quality-reports")
SHARED_LIBS_DIRNAME  <- "_libs"

#' Move a freshly rendered file into its final location, with a copy+unlink
#' fallback when `file.rename` fails. The fallback exists because
#' `file.rename` fails (silently returning FALSE) on cross-filesystem moves
#' (EXDEV) — which happens when tempdir() resolves to tmpfs (/tmp) and the
#' repo lives on a different mount, common inside Docker containers on EC2.
#'
#' `rename_fn` / `copy_fn` are injectable for unit testing only; production
#' callers use the defaults.
#'
#' Returns TRUE only if the destination exists after the move.
place_render_output <- function(src, dest,
                                rename_fn = base::file.rename,
                                copy_fn   = base::file.copy) {
  moved <- suppressWarnings(rename_fn(src, dest))
  if (!isTRUE(moved)) {
    moved <- copy_fn(src, dest, overwrite = TRUE)
    if (isTRUE(moved)) unlink(src)
  }
  isTRUE(moved) && file.exists(dest)
}

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
#' copying the template to a unique tempdir path before rendering.
#'
#' Returns a list `(ok, out_path, stem, files_dir)` for the post-process step
#' to dedupe assets and rewrite references. On failure returns
#' `list(ok = FALSE)`.
render_one_report <- function(rds_path, out_path, logger = NULL) {
  out_dir <- dirname(out_path)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  tpl_copy <- tempfile(pattern = "quality_report_", fileext = ".qmd")
  file.copy(TEMPLATE_PATH, tpl_copy, overwrite = TRUE)
  # The .qmd is no longer needed after render; the _files/ sibling is kept
  # for the dedup pass and cleaned up there.
  on.exit(unlink(tpl_copy), add = TRUE)

  stem         <- tools::file_path_sans_ext(basename(tpl_copy))
  rendered_at  <- file.path(dirname(tpl_copy), paste0(stem, ".html"))
  files_dir    <- file.path(dirname(tpl_copy), paste0(stem, "_files"))

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

  if (!isTRUE(ok)) return(list(ok = FALSE))

  if (!file.exists(rendered_at)) {
    if (!is.null(logger)) {
      log4r::error(logger, sprintf("Render produced no output for %s", rds_path))
    }
    return(list(ok = FALSE))
  }
  if (!place_render_output(rendered_at, out_path)) {
    if (!is.null(logger)) {
      log4r::error(logger, sprintf("Failed to place render output at %s", out_path))
    }
    unlink(rendered_at)
    return(list(ok = FALSE))
  }
  if (!is.null(logger)) log4r::info(logger, sprintf("WROTE %s", out_path))
  list(ok = TRUE, out_path = out_path, stem = stem, files_dir = files_dir)
}

#' Relative path from `from_file`'s directory up to `to_dir`. Both must share
#' a common ancestor (which they do here — both live under QUALITY_REPORTS_ROOT).
relative_path_to <- function(from_file, to_dir) {
  from_parts <- strsplit(normalizePath(dirname(from_file), mustWork = FALSE), .Platform$file.sep, fixed = TRUE)[[1]]
  to_parts   <- strsplit(normalizePath(to_dir,             mustWork = FALSE), .Platform$file.sep, fixed = TRUE)[[1]]
  # Strip common prefix.
  i <- 1L
  while (i <= min(length(from_parts), length(to_parts)) &&
         from_parts[i] == to_parts[i]) i <- i + 1L
  ups   <- rep("..", length(from_parts) - i + 1L)
  downs <- to_parts[i:length(to_parts)]
  paste(c(ups, downs), collapse = "/")
}

#' Collapse all per-render `<stem>_files/libs/` trees into a single shared
#' libs dir under QUALITY_REPORTS_ROOT/_libs/, then rewrite each HTML's
#' references from `<stem>_files/libs/` to the relative path to `_libs/`.
#' Returns the absolute path to the shared libs dir, or NULL if nothing to do.
dedup_render_assets <- function(results, reports_root = QUALITY_REPORTS_ROOT,
                                logger = NULL) {
  ok_results <- Filter(function(r) is.list(r) && isTRUE(r$ok), results)
  if (length(ok_results) == 0L) return(invisible(NULL))

  shared_libs <- file.path(reports_root, SHARED_LIBS_DIRNAME)
  unlink(shared_libs, recursive = TRUE)
  dir.create(shared_libs, recursive = TRUE, showWarnings = FALSE)

  # Pick the first render with an intact libs/ subdir as the source of truth.
  src_libs <- NULL
  for (r in ok_results) {
    candidate <- file.path(r$files_dir, "libs")
    if (dir.exists(candidate)) { src_libs <- candidate; break }
  }
  if (is.null(src_libs)) {
    if (!is.null(logger)) log4r::warn(logger, "No per-render libs/ dir found; HTMLs will have broken asset refs")
    return(invisible(shared_libs))
  }

  ok_copy <- file.copy(list.files(src_libs, full.names = TRUE),
                       shared_libs, recursive = TRUE)
  if (!all(ok_copy)) {
    if (!is.null(logger)) log4r::warn(logger, "Some shared-libs files failed to copy")
  }
  if (!is.null(logger)) {
    n_files <- length(list.files(shared_libs, recursive = TRUE))
    log4r::info(logger, sprintf("Shared assets: %d file(s) under %s", n_files, shared_libs))
  }

  for (r in ok_results) {
    rel_libs <- relative_path_to(r$out_path, shared_libs)
    pattern  <- paste0(r$stem, "_files/libs/")
    html     <- readLines(r$out_path, warn = FALSE)
    html     <- gsub(pattern, paste0(rel_libs, "/"), html, fixed = TRUE)
    writeLines(html, r$out_path)
  }
  if (!is.null(logger)) {
    log4r::info(logger, sprintf("Rewrote asset references in %d HTML(s)", length(ok_results)))
  }

  # Clean up the per-render tempdirs.
  for (r in ok_results) unlink(r$files_dir, recursive = TRUE)

  invisible(shared_libs)
}

run_render_reports <- function(logs_dir       = PATHS$logs,
                               reports_root   = QUALITY_REPORTS_ROOT,
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
    out_path <- file.path(reports_root, tax_year, form,
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
  # without ok=TRUE as a failure.
  is_ok <- function(r) is.list(r) && isTRUE(r$ok)
  n_ok   <- sum(vapply(results, is_ok, logical(1)))
  n_fail <- length(results) - n_ok
  if (n_fail > 0L) {
    log4r::warn(logger, sprintf("Render run complete: %d HTML reports written, %d failed",
                                n_ok, n_fail))
  } else {
    log4r::info(logger, sprintf("Render run complete: %d HTML reports written", n_ok))
  }

  if (n_ok > 0L) dedup_render_assets(results, reports_root, logger)

  invisible(NULL)
}

if (sys.nframe() == 0L) run_render_reports()
