suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
})

dict_dir <- "data/raw/soi_dictionaries"
files <- list.files(dict_dir, pattern = "\\.xlsx?$", full.names = TRUE)

# Reads a sheet, locates the row that says "Element Name" in column 1,
# returns variable_name + description + location starting from the next row.
parse_form_sheet <- function(path, sheet, year, form) {
  raw <- as.data.table(suppressMessages(
    read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  ))
  if (nrow(raw) == 0) return(NULL)
  col1 <- as.character(raw[[1]])
  hdr <- which(trimws(col1) == "Element Name")[1]
  if (is.na(hdr)) return(NULL)
  body <- raw[(hdr + 1L):.N]
  body <- body[!is.na(body[[1]]) & nzchar(trimws(as.character(body[[1]])))]
  # Drop footnote rows: real SOI variable names start with an alphanumeric;
  # the IRS occasionally tucks an asterisk-prefixed note ("*Element locations ...")
  # below the variable list, which would otherwise be ingested as a variable.
  body <- body[grepl("^[A-Za-z0-9_]", trimws(as.character(body[[1]])))]
  if (nrow(body) == 0) return(NULL)
  data.table(
    year = year,
    form = form,
    variable_name = trimws(as.character(body[[1]])),
    description = if (ncol(body) >= 2) trimws(as.character(body[[2]])) else NA_character_,
    location = if (ncol(body) >= 3) trimws(as.character(body[[3]])) else NA_character_
  )
}

# 2012 file: "Record Layout" sheet contains all forms in one block.
# Look at it: section headers like "Data Items on Form 990 Annual Masterfile Extract" / "...990-EZ..." / "...990-PF..."
parse_2012 <- function(path) {
  raw <- as.data.table(suppressMessages(
    read_excel(path, sheet = "Record Layout", col_names = FALSE, .name_repair = "minimal")
  ))
  col1 <- as.character(raw[[1]])
  # Identify form-section start rows
  form_rows <- which(grepl("Data Items on Form 990(-EZ|-PF)? Annual", col1))
  form_labels <- col1[form_rows]
  forms <- ifelse(grepl("990-EZ", form_labels), "990EZ",
           ifelse(grepl("990-PF", form_labels), "990PF", "990"))
  out <- list()
  for (i in seq_along(form_rows)) {
    start <- form_rows[i]
    end <- if (i < length(form_rows)) form_rows[i + 1L] - 1L else nrow(raw)
    block <- raw[start:end]
    blk_col1 <- as.character(block[[1]])
    hdr <- which(trimws(blk_col1) == "Element Name")[1]
    if (is.na(hdr)) next
    body <- block[(hdr + 1L):.N]
    body <- body[!is.na(body[[1]]) & nzchar(trimws(as.character(body[[1]])))]
    body <- body[grepl("^[A-Za-z0-9_]", trimws(as.character(body[[1]])))]
    if (nrow(body) == 0) next
    out[[length(out) + 1L]] <- data.table(
      year = 2012L,
      form = forms[i],
      variable_name = trimws(as.character(body[[1]])),
      description = if (ncol(body) >= 2) trimws(as.character(body[[2]])) else NA_character_,
      location = if (ncol(body) >= 4) trimws(as.character(body[[4]])) else NA_character_
    )
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

results <- list()
for (f in files) {
  yy <- sub("eofinextractdoc.*", "", basename(f))
  year <- as.integer(paste0("20", yy))
  if (year == 2012) {
    df <- parse_2012(f)
    if (!is.null(df) && nrow(df) > 0) results[[length(results) + 1L]] <- df
    next
  }
  for (s in excel_sheets(f)) {
    form <- switch(s, "990" = "990", "990-EZ" = "990EZ", "990-PF" = "990PF", NA_character_)
    if (is.na(form)) next
    df <- tryCatch(parse_form_sheet(f, s, year, form), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) results[[length(results) + 1L]] <- df
  }
}

all <- rbindlist(results, use.names = TRUE, fill = TRUE)
fwrite(all, file.path(dict_dir, "_all_variables.csv"))

cat("=== variable counts per (year, form) ===\n")
counts <- all[, .(n_vars = .N), by = .(year, form)][order(form, year)]
print(counts)

cat("\n=== union variable count per form (across all years) ===\n")
print(all[, .(n_unique_vars = uniqueN(variable_name)), by = form])

cat("\n=== variables only in some years (per form) ===\n")
for (frm in unique(all$form)) {
  sub <- all[form == frm]
  per_var <- sub[, .(years = paste(sort(unique(year)), collapse = ",")), by = variable_name]
  all_years <- paste(sort(unique(sub$year)), collapse = ",")
  inconsistent <- per_var[years != all_years]
  cat("--- form:", frm, "(years present:", all_years, ") ---\n")
  cat("  n vars present in all years:", per_var[years == all_years, .N], "\n")
  cat("  n vars NOT in all years:", nrow(inconsistent), "\n")
  if (nrow(inconsistent) > 0 && nrow(inconsistent) <= 30) {
    print(inconsistent)
  } else if (nrow(inconsistent) > 30) {
    cat("  (showing first 30)\n")
    print(inconsistent[1:30])
  }
}
