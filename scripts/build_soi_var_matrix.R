suppressPackageStartupMessages({
  library(data.table)
})

dict_dir <- "data/raw/soi_dictionaries"
all <- fread(file.path(dict_dir, "_all_variables.csv"))

# Per-form presence matrix
for (frm in c("990", "990EZ", "990PF")) {
  sub <- all[form == frm]
  if (nrow(sub) == 0) next
  yrs <- sort(unique(sub$year))
  mat <- dcast(sub, variable_name ~ year, value.var = "variable_name",
               fun.aggregate = function(x) if (length(x) > 0) "Y" else "")
  setcolorder(mat, c("variable_name", as.character(yrs)))
  fwrite(mat, file.path(dict_dir, sprintf("_var_matrix_%s.csv", frm)))

  # Summary: var category buckets
  yr_cols <- as.character(yrs)
  presence <- mat[, ..yr_cols]
  n_years <- rowSums(presence == "Y")
  mat[, n_years_present := n_years]
  cat(sprintf("\n=== %s ===\n", frm))
  cat(sprintf("years parsed: %s (n=%d)\n", paste(yrs, collapse=","), length(yrs)))
  cat(sprintf("total unique vars: %d\n", nrow(mat)))
  cat(sprintf("present in ALL %d years: %d\n", length(yrs), sum(n_years == length(yrs))))
  cat(sprintf("present in 2013-2024 only (excl 2012, %d years): %d\n",
              length(yrs) - 1, sum(n_years == length(yrs) - 1 &
                                   mat[[as.character(yrs[1])]] == "")))
  cat("vars by # years present:\n")
  print(table(n_years, dnn = "n_years_present"))
}

# 2012 vs 2013 deltas (since 2012 parser may have missed rows)
cat("\n=== 2012 vs 2013 var count by form (sanity check) ===\n")
print(all[year %in% c(2012, 2013), .N, by = .(year, form)][order(form, year)])

# Write a recommended canonical reference: union of vars across the post-2012 stable era
canonical <- all[year >= 2013, .(variable_name, form, description, location)][
  , .(years_seen = paste(sort(unique(all[year >= 2013 & form == .BY$form &
                                         variable_name == .BY$variable_name]$year)),
                         collapse = ","),
      description = first(description),
      location = first(location)),
  by = .(variable_name, form)
]
fwrite(canonical, file.path(dict_dir, "_canonical_2013plus.csv"))
cat("\nWrote _canonical_2013plus.csv (", nrow(canonical), "rows)\n")
