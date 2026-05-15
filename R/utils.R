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
