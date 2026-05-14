# Add `soi_current_candidate` column to legacy OVERRIDES files.
#
# For each legacy OVERRIDES row, propose the best-matching harmonized_name
# from the SOI-current FINAL crosswalks. Output is a verify-or-overrule
# suggestion, not a final decision.
#
# Targets:
#   PZ legacy -> 990combined target = soi_990 + soi_990ez harmonized names.
#   PF legacy -> soi_990pf harmonized names.
#
# Matching layers (highest-precedence first):
#   1. Exact source-name match (case-insensitive). Legacy SOURCE_COLUMN
#      matches SOI source_var verbatim. E.g., legacy "EIN" -> SOI "ein".
#   2. Description-token Jaccard similarity above threshold. Tokens are
#      lowercased words >= 3 chars after stripping punctuation.
#
# Rows with harmonized_name == "" (BMF-origin pre-marks) are skipped — no
# candidate proposed, since the schema-parity decision drops them.
#
# Per feedback_never_overwrite_user_edits: this script reads OVERRIDES,
# regenerates with the new candidate column, and writes back ONLY if the
# OVERRIDES file's user-editable columns (harmonized_name) match what the
# initial builder would emit. If the user has started editing harmonized
# names, the script aborts with a diff.

suppressPackageStartupMessages({
  library(data.table)
})

JACCARD_MIN <- 0.30   # token-set overlap threshold for a description match
TOKEN_MIN_LEN <- 3L

tokenize <- function(s) {
  if (is.na(s) || s == "") return(character(0))
  s <- tolower(s)
  s <- gsub("[^a-z0-9 ]+", " ", s)
  toks <- strsplit(s, "\\s+", fixed = FALSE)[[1]]
  toks <- toks[nchar(toks) >= TOKEN_MIN_LEN]
  unique(toks)
}

jaccard <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  length(intersect(a, b)) / length(union(a, b))
}

build_soi_target <- function(paths_with_form) {
  parts <- lapply(paths_with_form, function(pf) {
    dt <- fread(pf$path)
    dt[, form := pf$form]
    # Keep only columns we need; SOI 990ez/pf have extra 'source' column
    dt <- dt[, .(source_var, harmonized_name, description, location, form)]
    dt
  })
  rbindlist(parts, use.names = TRUE, fill = TRUE)
}

best_candidate <- function(legacy_row, soi) {
  # Layer 1: exact source-name match (case-insensitive)
  src_match <- soi[tolower(source_var) == tolower(legacy_row$source_column)]
  if (nrow(src_match) > 0) {
    # If multiple forms have the same source_var, prefer the one with
    # non-empty harmonized_name and break ties by form (990 > 990ez > 990pf).
    src_match <- src_match[!is.na(harmonized_name) & harmonized_name != ""]
    if (nrow(src_match) > 0) {
      src_match[, form_rank := match(form, c("990", "990ez", "990pf"))]
      setorder(src_match, form_rank)
      return(list(
        candidate = src_match$harmonized_name[1],
        match_type = "exact_source_name",
        match_score = NA_real_,
        match_via = paste0(src_match$form[1], ":", src_match$source_var[1])
      ))
    }
  }

  # Layer 2: description-token Jaccard
  legacy_text <- paste(c(legacy_row$label, legacy_row$description), collapse = " ")
  legacy_toks <- tokenize(legacy_text)
  if (length(legacy_toks) == 0) {
    return(list(candidate = NA_character_, match_type = "no_legacy_text",
                match_score = NA_real_, match_via = NA_character_))
  }
  scores <- vapply(soi$desc_tokens, function(t) jaccard(legacy_toks, t), numeric(1))
  best_idx <- which.max(scores)
  if (length(best_idx) == 0 || scores[best_idx] < JACCARD_MIN) {
    return(list(candidate = NA_character_, match_type = "below_threshold",
                match_score = if (length(best_idx) > 0) scores[best_idx] else NA_real_,
                match_via = NA_character_))
  }
  list(
    candidate   = soi$harmonized_name[best_idx],
    match_type  = "description_jaccard",
    match_score = round(scores[best_idx], 3),
    match_via   = paste0(soi$form[best_idx], ":", soi$source_var[best_idx])
  )
}

process <- function(overrides_path, soi_target) {
  cat(sprintf("\n=== %s ===\n", overrides_path))
  if (!file.exists(overrides_path)) {
    cat("  Not found, skipping.\n")
    return(invisible())
  }

  ov <- fread(overrides_path)
  # Detect user edits: if harmonized_name disagrees with tolower(source_column)
  # for any non-pre-marked row, the user has started authoring. Abort.
  pre_marked <- ov$harmonized_name == ""
  expected <- tolower(ov$source_column)
  edited_idx <- which(!pre_marked & ov$harmonized_name != expected)
  if (length(edited_idx) > 0) {
    cat(sprintf("  ABORT: %d rows appear hand-edited (harmonized_name differs from tolower(source)).\n",
                length(edited_idx)))
    cat("  This script will not overwrite user edits. Inspect:\n")
    print(ov[edited_idx[1:min(5, length(edited_idx))],
             .(source_column, harmonized_name, tolower_source = tolower(source_column))])
    return(invisible())
  }

  # Pre-tokenize SOI descriptions once
  if (!"desc_tokens" %in% names(soi_target)) {
    soi_target[, desc_tokens := lapply(description, tokenize)]
  }

  n <- nrow(ov)
  cand <- vector("list", n)
  for (i in seq_len(n)) {
    if (pre_marked[i]) {
      cand[[i]] <- list(candidate = NA_character_, match_type = "skipped_bmf_origin",
                        match_score = NA_real_, match_via = NA_character_)
    } else {
      cand[[i]] <- best_candidate(ov[i], soi_target)
    }
  }

  ov[, soi_current_candidate := vapply(cand, `[[`, character(1), "candidate")]
  ov[, candidate_match_type  := vapply(cand, `[[`, character(1), "match_type")]
  ov[, candidate_match_score := vapply(cand, function(x) x$match_score, numeric(1))]
  ov[, candidate_match_via   := vapply(cand, `[[`, character(1), "match_via")]

  # Reorder so candidate sits next to harmonized_name
  setcolorder(ov, c("source_column", "harmonized_name", "soi_current_candidate",
                    "candidate_match_type", "candidate_match_score", "candidate_match_via",
                    "label", "description", "section", "dtype",
                    "n_tax_years", "years_present", "first_year", "last_year",
                    "scope_observed"))

  fwrite(ov, overrides_path)

  cat(sprintf("  rows: %d\n", n))
  cat(sprintf("    skipped (BMF-origin):        %d\n",
              sum(ov$candidate_match_type == "skipped_bmf_origin")))
  cat(sprintf("    exact source-name match:     %d\n",
              sum(ov$candidate_match_type == "exact_source_name", na.rm = TRUE)))
  cat(sprintf("    description-jaccard >= %.2f: %d\n", JACCARD_MIN,
              sum(ov$candidate_match_type == "description_jaccard", na.rm = TRUE)))
  cat(sprintf("    below threshold / no text:   %d\n",
              sum(ov$candidate_match_type %in% c("below_threshold", "no_legacy_text"),
                  na.rm = TRUE)))
}

soi_pz_target <- build_soi_target(list(
  list(path = "data/crosswalks/soi_990_crosswalk_FINAL.csv",   form = "990"),
  list(path = "data/crosswalks/soi_990ez_crosswalk_FINAL.csv", form = "990ez")
))
soi_pf_target <- build_soi_target(list(
  list(path = "data/crosswalks/soi_990pf_crosswalk_FINAL.csv", form = "990pf")
))

process("data/crosswalks/legacy_pz_crosswalk_OVERRIDES.csv", soi_pz_target)
process("data/crosswalks/legacy_pf_crosswalk_OVERRIDES.csv", soi_pf_target)

cat("\nDone. Review the soi_current_candidate column. To accept a candidate,\n")
cat("copy its value into harmonized_name. To overrule, write a different name.\n")
