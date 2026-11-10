# R/transforms/repeated_returns.R
# One return, one row (ADR 0056 finding; legacy pipeline).
#
# Why this exists: each yearly NCCS CORE file carried forward returns from
# earlier files, so the same return (same EIN, same period end) appears in
# two or more yearly files. The legacy pipeline assigns rows to years by the
# period end, so those copies land in the same output file. For 2005 that
# was 60,293 repeated rows among 324,768 charity rows.

#' Keep one row per return, preferring the newest NCCS file
#'
#' @param dt A data.table holding at least `ein`, `tax_period` and
#'   `source_file_year` (the year of the NCCS CORE file the row came from).
#' @return The same data.table with one row per (`ein`, `tax_period`). When
#'   a return appears more than once, the row from the highest
#'   `source_file_year` is kept, since a later file carries any correction
#'   NCCS made. Rows whose `tax_period` is missing are never treated as
#'   repeats and are all kept. The number of rows dropped is attached as
#'   the attribute `n_dropped`.
#' @details Stops with an error if `source_file_year` is absent, because
#'   without it there is no way to say which copy is newer.
drop_repeated_returns <- function(dt) {
  if (!"source_file_year" %in% names(dt)) {
    stop("drop_repeated_returns needs source_file_year to know which copy is newer")
  }

  has_period <- !is.na(dt$tax_period)
  with_period <- dt[has_period]
  without_period <- dt[!has_period]

  data.table::setorder(with_period, ein, tax_period, -source_file_year)
  kept <- unique(with_period, by = c("ein", "tax_period"))

  out <- data.table::rbindlist(list(kept, without_period), use.names = TRUE)
  data.table::setattr(out, "n_dropped", nrow(dt) - nrow(out))

  out
}
