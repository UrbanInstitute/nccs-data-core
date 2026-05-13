# scripts/download_irs_forms.R
# One-shot archiver for blank IRS Form 990, 990-EZ, 990-PF + their schedules
# and instructions, across 1989–2024. The IRS exposes prior-year PDFs at:
#
#   https://www.irs.gov/pub/irs-prior/<basename>--<YYYY>.pdf
#
# and the current year at:
#
#   https://www.irs.gov/pub/irs-pdf/<basename>.pdf
#
# Schedule B has an idiosyncratic basename (`f990ezb`), used by 990, 990-EZ,
# and 990-PF — despite the "ez" in the filename, it's not EZ-specific. All
# other schedules follow the regular pattern `f990s<letter>` (and `i990s<letter>`
# for instructions).
#
# Output layout (matches the existing data/raw/forms/ convention):
#   data/raw/forms/<basename>_<YYYY>.pdf
#
# Modes:
#   --dry-run     HEAD each candidate URL, write manifest CSV, no downloads.
#   (default)     HEAD-then-GET for missing files. Idempotent: existing local
#                 files are skipped without re-checking the URL.
#
# Usage:
#   Rscript scripts/download_irs_forms.R --dry-run
#   Rscript scripts/download_irs_forms.R
#   Rscript scripts/download_irs_forms.R --years 2020-2024 --forms f990,f990ez
#
# Politeness: sequential requests with a 0.1 s sleep between hits. ~38 form
# codes × 36 years × 2 modes = ~2,700 candidates, ~7 min if everything is a
# miss. Subsequent runs only HEAD candidates we don't yet have on disk.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

# ---- Configuration ----------------------------------------------------------

YEAR_RANGE_DEFAULT <- 1989:2024L

# 3 main forms + 16 schedules. The `f`/`i` prefix is added separately for
# (form, instructions). Schedule B's quirky `990ezb` basename is preserved
# verbatim — that's the IRS canonical name, used by 990 / 990-EZ / 990-PF
# alike (the "ez" is misleading).
FORM_BASENAMES <- c(
  "990", "990ez", "990pf",
  "990sa", "990ezb", "990sc", "990sd", "990se", "990sf", "990sg",
  "990sh", "990si", "990sj", "990sk", "990sl", "990sm", "990sn",
  "990so", "990sr"
)

# Some schedules postdate the 2008 990 redesign — older years will 404.
SCHEDULE_EARLIEST_YEAR <- c(
  "990sh" = 2008L, "990si" = 2008L, "990sj" = 2008L,
  "990sk" = 2008L, "990sm" = 2008L, "990sr" = 2008L
)

PUB_PRIOR_URL   <- "https://www.irs.gov/pub/irs-prior/%s--%d.pdf"
PUB_CURRENT_URL <- "https://www.irs.gov/pub/irs-pdf/%s.pdf"
USER_AGENT      <- "nccs-data-core/1.0 (Urban Institute / NCCS form archiver)"
SLEEP_SEC       <- 0.1

FORMS_DIR     <- here::here("data", "raw", "forms")
MANIFEST_PATH <- here::here("data", "raw", "forms", "_manifest.csv")

# ---- CLI parsing ------------------------------------------------------------

parse_args <- function(argv) {
  out <- list(dry_run = FALSE, years = YEAR_RANGE_DEFAULT, forms = NULL)
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[i]
    if (a == "--dry-run") {
      out$dry_run <- TRUE
    } else if (a == "--years" && i < length(argv)) {
      spec <- argv[i + 1L]
      if (grepl("-", spec, fixed = TRUE)) {
        rng <- as.integer(strsplit(spec, "-", fixed = TRUE)[[1]])
        out$years <- rng[1]:rng[2]
      } else {
        out$years <- as.integer(strsplit(spec, ",", fixed = TRUE)[[1]])
      }
      i <- i + 1L
    } else if (a == "--forms" && i < length(argv)) {
      out$forms <- strsplit(argv[i + 1L], ",", fixed = TRUE)[[1]]
      i <- i + 1L
    }
    i <- i + 1L
  }
  out
}

# ---- Candidate enumeration --------------------------------------------------

build_candidates <- function(years, forms_filter = NULL) {
  bases <- if (is.null(forms_filter)) FORM_BASENAMES else
    intersect(FORM_BASENAMES, sub("^[fi]", "", forms_filter))
  rows <- list()
  for (base in bases) {
    earliest <- SCHEDULE_EARLIEST_YEAR[base]
    keep_years <- if (is.na(earliest)) years else years[years >= earliest]
    for (kind in c("f", "i")) {
      basename <- paste0(kind, base)
      for (yr in keep_years) {
        url <- sprintf(PUB_PRIOR_URL, basename, yr)
        local <- file.path(FORMS_DIR, sprintf("%s_%d.pdf", basename, yr))
        rows[[length(rows) + 1L]] <- list(
          basename = basename, year = yr, kind = kind, schedule = base,
          url = url, local = local
        )
      }
    }
  }
  rbindlist(rows)
}

# ---- HTTP helpers -----------------------------------------------------------

http_status <- function(url) {
  # Returns list(status = int, size = integer or NA).
  res <- system2(
    "curl",
    args = c("-s", "-o", "/dev/null", "-w", shQuote("%{http_code} %{size_download}"),
             "-I", "-A", shQuote(USER_AGENT), shQuote(url)),
    stdout = TRUE, stderr = FALSE
  )
  parts <- strsplit(res, " ", fixed = TRUE)[[1]]
  list(status = as.integer(parts[1]),
       size   = suppressWarnings(as.integer(parts[2])))
}

http_get <- function(url, dest) {
  # Returns integer exit code (0 = ok).
  rc <- system2(
    "curl",
    args = c("-s", "-A", shQuote(USER_AGENT), "-o", shQuote(dest), shQuote(url)),
    stdout = FALSE, stderr = FALSE
  )
  if (!is.null(attr(rc, "status"))) attr(rc, "status") else rc
}

# ---- Main -------------------------------------------------------------------

main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  opt <- parse_args(argv)
  dir.create(FORMS_DIR, recursive = TRUE, showWarnings = FALSE)

  cand <- build_candidates(opt$years, opt$forms)
  cand[, status := NA_integer_]
  cand[, size   := NA_integer_]
  cand[, on_disk_pre := file.exists(local)]
  cand[, downloaded  := FALSE]

  cat(sprintf("Candidates: %d  (years %d-%d, forms=%d)\n",
              nrow(cand), min(opt$years), max(opt$years),
              uniqueN(cand$basename)))
  cat(sprintf("Already on disk: %d\n", sum(cand$on_disk_pre)))
  cat(sprintf("Mode: %s\n\n", if (isTRUE(opt$dry_run)) "DRY RUN (HEAD only)" else "DOWNLOAD"))

  t0 <- Sys.time()
  n_ok <- 0L; n_404 <- 0L; n_err <- 0L; n_dl <- 0L

  for (i in seq_len(nrow(cand))) {
    row <- cand[i]
    if (isTRUE(row$on_disk_pre) && !isTRUE(opt$dry_run)) {
      cand[i, status := 200L]
      next
    }
    s <- tryCatch(http_status(row$url), error = function(e) list(status = NA_integer_, size = NA_integer_))
    cand[i, `:=`(status = s$status, size = s$size)]
    if (isTRUE(s$status == 200L)) {
      n_ok <- n_ok + 1L
      if (!isTRUE(opt$dry_run)) {
        rc <- http_get(row$url, row$local)
        if (rc == 0L && file.exists(row$local) && file.info(row$local)$size > 0L) {
          cand[i, downloaded := TRUE]
          n_dl <- n_dl + 1L
        } else {
          if (file.exists(row$local)) unlink(row$local)
          n_err <- n_err + 1L
        }
      }
    } else if (isTRUE(s$status == 404L)) {
      n_404 <- n_404 + 1L
    } else {
      n_err <- n_err + 1L
    }
    if (i %% 100L == 0L) {
      cat(sprintf("[%5d/%d]  200=%d  404=%d  err=%d  dl=%d  (%.1fs)\n",
                  i, nrow(cand), n_ok, n_404, n_err, n_dl,
                  as.numeric(Sys.time() - t0, units = "secs")))
    }
    Sys.sleep(SLEEP_SEC)
  }

  cat(sprintf("\nDone in %.1fs:  200=%d  404=%d  err=%d  downloaded=%d\n",
              as.numeric(Sys.time() - t0, units = "secs"),
              n_ok, n_404, n_err, n_dl))

  # Write manifest.
  cand[, on_disk_post := file.exists(local)]
  fwrite(cand[, .(basename, year, kind, schedule, url, local,
                  status, size, on_disk_pre, downloaded, on_disk_post)],
         MANIFEST_PATH)
  cat(sprintf("Manifest: %s\n", MANIFEST_PATH))

  invisible(cand)
}

if (sys.nframe() == 0L) main()
