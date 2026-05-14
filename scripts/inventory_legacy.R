# Inventory legacy CORE file headers from s3://nccsdata/legacy/core/.
#
# Scope: pre-2012 raw legacy files only. PZ (501c3 + 501ce) + PF, tax years
# 1989-2011. The 2012+ files in the same bucket are NCCS+SOI hybrids and are
# out of scope (handled by the SOI-current pipeline).
#
# Header rows are streamed via S3 Range requests (first 256 KB) so we never
# pull a full multi-hundred-MB CSV. Output:
#   data/raw/legacy_inventory/headers_by_file.tsv
# Columns: filename, tax_year, subsection_class, scope, column_position,
#          column_name_raw

suppressPackageStartupMessages({
  library(data.table)
  library(aws.s3)
  library(stringr)
})

BUCKET     <- "nccsdata"
PREFIX     <- "legacy/core/"
OUT_DIR    <- "data/raw/legacy_inventory"
OUT_FILE   <- file.path(OUT_DIR, "headers_by_file.tsv")
YEAR_LO    <- 1989L
YEAR_HI    <- 2011L
RANGE_SIZE <- 262143L  # bytes 0-262143 = 256 KB; ample for 276-col CSV header

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- List bucket ----
listing <- aws.s3::get_bucket_df(bucket = BUCKET, prefix = PREFIX, max = Inf)
files <- basename(listing$Key)

# Filename: CORE-{YYYY}-{SUBSECTION_CLASS}-{SCOPE}.csv
# subsection_class: 501C3-CHARITIES | 501CE-NONPROFIT | 501C3-PRIVFOUND
# scope: PC | PZ | PF
pat <- "^CORE-(\\d{4})-(501C3-CHARITIES|501CE-NONPROFIT|501C3-PRIVFOUND)-(PC|PZ|PF)\\.csv$"
m <- str_match(files, pat)
ok <- !is.na(m[, 1])

inv <- data.table(
  s3_key            = listing$Key[ok],
  filename          = files[ok],
  tax_year          = as.integer(m[ok, 2]),
  subsection_class  = m[ok, 3],
  scope             = m[ok, 4]
)

inv <- inv[scope %in% c("PZ", "PF") & tax_year >= YEAR_LO & tax_year <= YEAR_HI]
setorder(inv, tax_year, subsection_class, scope)

cat(sprintf("In-scope files (PZ/PF, %d-%d): %d\n", YEAR_LO, YEAR_HI, nrow(inv)))

# ---- Stream first-row headers via Range request ----
fetch_header <- function(key) {
  raw <- aws.s3::get_object(
    object  = key,
    bucket  = BUCKET,
    headers = list(Range = sprintf("bytes=0-%d", RANGE_SIZE))
  )
  body <- rawToChar(raw)
  first_line <- sub("\r?\n.*$", "", body)
  # Strip BOM if present
  first_line <- sub("^\xef\xbb\xbf", "", first_line, useBytes = TRUE)
  cols <- scan(text = first_line, what = character(), sep = ",",
               quote = "\"", quiet = TRUE, strip.white = TRUE)
  cols
}

rows <- vector("list", nrow(inv))
for (i in seq_len(nrow(inv))) {
  fn <- inv$filename[i]
  cat(sprintf("  [%2d/%d] %s ... ", i, nrow(inv), fn))
  cols <- tryCatch(fetch_header(inv$s3_key[i]),
                   error = function(e) { cat("ERROR: ", conditionMessage(e), "\n"); NULL })
  if (is.null(cols)) next
  cat(sprintf("%d cols\n", length(cols)))
  rows[[i]] <- data.table(
    filename         = fn,
    tax_year         = inv$tax_year[i],
    subsection_class = inv$subsection_class[i],
    scope            = inv$scope[i],
    column_position  = seq_along(cols),
    column_name_raw  = cols
  )
}

headers <- rbindlist(rows, use.names = TRUE, fill = TRUE)
fwrite(headers, OUT_FILE, sep = "\t")

cat(sprintf("\nWrote %d header rows (%d files) to %s\n",
            nrow(headers), headers[, uniqueN(filename)], OUT_FILE))

cat("\nColumn count per file:\n")
print(headers[, .(n_cols = .N), by = .(tax_year, subsection_class, scope)])
