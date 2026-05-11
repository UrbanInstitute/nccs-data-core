# R/quality/stat_helpers.R
# Type-specific column-statistics helpers used by post-checks.
# Pure functions — no CORE-specific logic, no I/O.
# Extracted from BMF's post_checks.R (which is being rewritten alongside this).

suppressPackageStartupMessages({
  library(data.table)
})

#' @param col_data vector of column data
#' @param n_rows total row count for percentage calculations
#' @return list with completeness metrics
calc_completeness_stats <- function(col_data, n_rows) {
  is_na    <- is.na(col_data)
  is_empty <- if (is.character(col_data)) col_data == "" & !is_na else rep(FALSE, length(col_data))
  non_null <- sum(!is_na & !is_empty)

  list(
    total            = n_rows,
    non_null         = non_null,
    null_count       = sum(is_na),
    empty_count      = sum(is_empty),
    completeness_pct = round(100 * non_null / n_rows, 2),
    missing_pct      = round(100 * (sum(is_na) + sum(is_empty)) / n_rows, 2)
  )
}

calc_numeric_stats <- function(col_data) {
  v <- col_data[!is.na(col_data)]
  if (length(v) == 0) {
    return(list(min = NA, max = NA, mean = NA, median = NA, sd = NA,
                q1 = NA, q3 = NA, iqr = NA,
                zero_count = 0, negative_count = 0, valid_count = 0))
  }
  q <- quantile(v, probs = c(0.25, 0.75), na.rm = TRUE)
  list(
    min            = min(v),
    max            = max(v),
    mean           = round(mean(v), 2),
    median         = median(v),
    sd             = round(sd(v), 2),
    q1             = q[1],
    q3             = q[2],
    iqr            = q[2] - q[1],
    zero_count     = sum(v == 0),
    negative_count = sum(v < 0),
    valid_count    = length(v)
  )
}

calc_character_stats <- function(col_data) {
  v <- col_data[!is.na(col_data) & col_data != ""]
  if (length(v) == 0) {
    return(list(unique_count = 0, min_length = NA, max_length = NA,
                avg_length = NA, valid_count = 0))
  }
  L <- nchar(v)
  list(
    unique_count = data.table::uniqueN(v),
    min_length   = min(L),
    max_length   = max(L),
    avg_length   = round(mean(L), 1),
    valid_count  = length(v)
  )
}

calc_code_stats <- function(col_data, top_n = 10L) {
  v <- col_data[!is.na(col_data) & col_data != ""]
  if (length(v) == 0) return(list(unique_count = 0, top_values = list(), valid_count = 0))

  freq <- data.table(value = v)[, .N, by = value][order(-N)]
  top  <- head(freq, top_n)
  top_list <- lapply(seq_len(nrow(top)), function(i) {
    list(value = as.character(top$value[i]),
         count = top$N[i],
         pct   = round(100 * top$N[i] / length(v), 2))
  })
  list(unique_count = data.table::uniqueN(v),
       top_values   = top_list,
       valid_count  = length(v))
}

calc_boolean_stats <- function(col_data, n_rows) {
  t <- sum(col_data == TRUE,  na.rm = TRUE)
  f <- sum(col_data == FALSE, na.rm = TRUE)
  n <- sum(is.na(col_data))
  list(
    true_count  = t,
    false_count = f,
    na_count    = n,
    true_pct    = round(100 * t / n_rows, 2),
    false_pct   = round(100 * f / n_rows, 2),
    na_pct      = round(100 * n / n_rows, 2)
  )
}

calc_date_stats <- function(col_data) {
  v <- col_data[!is.na(col_data)]
  if (length(v) == 0) {
    return(list(min_date = NA, max_date = NA, range_days = NA, valid_count = 0))
  }
  list(
    min_date    = as.character(min(v)),
    max_date    = as.character(max(v)),
    range_days  = as.integer(max(v) - min(v)),
    valid_count = length(v)
  )
}

#' Compute per-column report given a name and declared type.
#' @return list with completeness + type_stats slots.
generate_column_report <- function(dt, col_name, col_type) {
  if (!col_name %in% names(dt)) {
    return(list(column_name = col_name, column_type = col_type,
                present = FALSE, completeness = NULL, type_stats = NULL))
  }
  col_data <- dt[[col_name]]
  n_rows   <- nrow(dt)
  type_stats <- switch(col_type,
    "numeric"   = calc_numeric_stats(col_data),
    "character" = calc_character_stats(col_data),
    "code"      = calc_code_stats(col_data),
    "boolean"   = calc_boolean_stats(col_data, n_rows),
    "date"      = calc_date_stats(col_data),
    list()
  )
  list(
    column_name  = col_name,
    column_type  = col_type,
    present      = TRUE,
    completeness = calc_completeness_stats(col_data, n_rows),
    type_stats   = type_stats
  )
}

#' Aggregate column reports into a category report.
#' @param category_columns named character vector: col_name -> col_type
generate_category_report <- function(dt, category_name, category_columns) {
  col_names <- names(category_columns)
  reports <- lapply(col_names, function(nm) {
    generate_column_report(dt, nm, unname(category_columns[nm]))
  })
  names(reports) <- col_names

  present <- sum(sapply(reports, function(x) x$present))
  comp <- sapply(reports, function(x) if (!is.null(x$completeness)) x$completeness$completeness_pct else NA)
  list(
    category_name    = category_name,
    column_count     = length(col_names),
    columns_present  = present,
    avg_completeness = round(mean(comp, na.rm = TRUE), 2),
    columns          = reports
  )
}
