suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
})

dict_dir <- "data/raw/soi_dictionaries"
out_dir  <- "data/raw/soi_dictionaries"
files <- list.files(dict_dir, pattern = "\\.xlsx?$", full.names = TRUE)

results <- list()
for (f in files) {
  yy <- sub("eofinextractdoc.*", "", basename(f))
  year <- as.integer(paste0("20", yy))
  sheets <- excel_sheets(f)
  for (s in sheets) {
    df <- tryCatch(
      suppressMessages(read_excel(f, sheet = s, col_names = FALSE, .name_repair = "minimal")),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0) next
    results[[length(results) + 1L]] <- data.table(
      year = year,
      file = basename(f),
      sheet = s,
      n_rows = nrow(df),
      n_cols = ncol(df)
    )
  }
}
sheet_index <- rbindlist(results)
fwrite(sheet_index, file.path(out_dir, "_sheet_index.csv"))
cat("=== sheets per file ===\n")
print(sheet_index)
