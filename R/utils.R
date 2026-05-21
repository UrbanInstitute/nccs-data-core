# R/utils.R
# Shared utility predicates used across pipeline phases.

#' Element-wise "is this cell missing or empty?" predicate.
#'
#' Characters are blank if they are NA or the empty string. Other atomic types
#' are blank only if NA. Used by phase 5 quality post-checks (per-cohort
#' completeness) and phase 6 dictionary stats (n_nonnull / null_pct).
#'
#' @param x atomic vector.
#' @return logical vector of the same length as x.
is_blank <- function(x) {
  if (is.character(x)) is.na(x) | x == "" else is.na(x)
}

#' Resolve the worker count for a parallel phase.
#'
#' Precedence (first hit wins):
#'   1. Phase-specific env var (e.g. NCCS_QUALITY_WORKERS) — lets a cron job
#'      tune one phase without touching code.
#'   2. NCCS_WORKERS env var — umbrella override for "saturate this box".
#'   3. `workers` function arg.
#'   4. Default: max(1, detectCores() - 1) — leave one core for parent + OS.
#'
#' Phase 7's resolve_render_workers predates this helper and stays distinct
#' (it has a Quarto-specific cap consideration); new phases use this one.
resolve_workers <- function(phase_env_var, workers = NULL) {
  for (var in c(phase_env_var, "NCCS_WORKERS")) {
    if (is.null(var) || !nzchar(var)) next
    val <- Sys.getenv(var, unset = NA_character_)
    if (!is.na(val) && nzchar(val)) {
      n <- suppressWarnings(as.integer(val))
      if (!is.na(n) && n >= 1L) return(n)
    }
  }
  if (!is.null(workers)) {
    n <- suppressWarnings(as.integer(workers))
    if (!is.na(n) && n >= 1L) return(n)
  }
  max(1L, parallel::detectCores() - 1L)
}

#' Map a function over tasks, parallel when n_workers > 1, serial otherwise.
#' Uses mc.preschedule = FALSE so one bad task can't starve a worker that
#' still has pending work in its queue.
parallel_map <- function(tasks, fn, n_workers) {
  if (length(tasks) == 0L) return(list())
  if (n_workers <= 1L) return(lapply(tasks, fn))
  parallel::mclapply(tasks, fn, mc.cores = n_workers, mc.preschedule = FALSE)
}
