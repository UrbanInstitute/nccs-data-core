# R/transforms/ein.R
# Normalize EINs to canonical IRS display format: XX-XXXXXXX (10 chars).
# Strips embedded hyphens and whitespace, then re-inserts the hyphen after digit 2.
# The hyphen forces character typing on CSV re-read (avoids leading-zero loss).
# Invalid -> NA, logged.

#' @param dt data.table containing an `ein` column.
#' @param logger optional log4r logger.
#' @return dt, modified in place (also returned invisibly).
transform_ein <- function(dt, logger = NULL) {
  stopifnot(data.table::is.data.table(dt), "ein" %in% names(dt))

  raw <- as.character(dt$ein)
  clean <- gsub("[^0-9]", "", trimws(raw))

  ok <- nchar(clean) >= 1L & nchar(clean) <= 9L & clean != ""
  padded9 <- ifelse(ok, formatC(as.integer(clean), width = 9L, flag = "0", format = "d"),
                    NA_character_)
  hyphenated <- ifelse(is.na(padded9), NA_character_,
                       paste0(substr(padded9, 1L, 2L), "-", substr(padded9, 3L, 9L)))

  dt[, ein := hyphenated]

  n_bad <- sum(is.na(hyphenated) & !is.na(raw))
  if (n_bad > 0L && !is.null(logger)) {
    log4r::warn(logger, sprintf("ein: %d / %d rows have invalid EIN format",
                                n_bad, nrow(dt)))
  }

  invisible(dt)
}

# ============================================================================
# Additive EIN renderings (ADR 0036)
#
# Coercion-safe, additive renderings of the canonical dashed `ein`. The
# canonical `ein` (XX-XXXXXXX) is externally load-bearing and is left
# UNCHANGED; these are extra columns, not a reformat. Both are bijective with
# `ein` (conventions/ein-format.md §3, §7):
#   ein_prefixed = "ein-" + ein   (e.g. ein-38-2787387) — lowercase house key
#   EIN2         = "EIN-" + ein   (e.g. EIN-38-2787387) — legacy-compat alias
#
# These two renderers are TWINS of nccs-data-bmf/R/ein.R::ein_to_prefixed /
# ein_to_ein2 (ADR 0036 N1). They MUST produce byte-identical strings to the
# BMF order's output (same casing, same prefixing). If you touch one, touch the
# other; consolidating the two formatters into a single shared module is a known
# backlog item (N1), not a unilateral change. NOTE: only these *renderings* are
# kept in parity here — the upstream canonical `ein` formatter (transform_ein
# above) has drifted from the BMF one (e.g. empty -> NA here vs "00-0000000" in
# BMF); that drift is the N1 consolidation flag, tracked, not fixed here.
# ============================================================================

#' Render the lowercase coercion-safe EIN key (`ein_prefixed`).
#'
#' @param ein Character vector of canonical dashed EINs (XX-XXXXXXX).
#' @return Character vector `ein-XX-XXXXXXX` (NA preserved).
ein_to_prefixed <- function(ein) {
  ifelse(is.na(ein), NA_character_, paste0("ein-", ein))
}

#' Render the uppercase legacy-compat EIN alias (`EIN2`).
#'
#' @param ein Character vector of canonical dashed EINs (XX-XXXXXXX).
#' @return Character vector `EIN-XX-XXXXXXX` (NA preserved).
ein_to_ein2 <- function(ein) {
  ifelse(is.na(ein), NA_character_, paste0("EIN-", ein))
}

#' Add the two additive EIN renderings (`ein_prefixed`, `EIN2`) in place.
#'
#' Derives both from the already-canonicalized `ein` column. Call AFTER
#' transform_ein() and AFTER any blanket financial coercion, so these character
#' columns are never swept into numeric coercion.
#'
#' @param dt data.table containing a canonical `ein` column.
#' @return dt, modified in place (also returned invisibly).
add_ein_renderings <- function(dt) {
  stopifnot(data.table::is.data.table(dt), "ein" %in% names(dt))
  dt[, ein_prefixed := ein_to_prefixed(ein)]
  dt[, EIN2         := ein_to_ein2(ein)]
  invisible(dt)
}
