# R/07_render_report.R
# Phase 7b: render docs/quality_report_template.qmd per (form, tax_year) using
# the RDS reports produced in Phase 6. Output:
#   data/processed/{tax_year}/{form}/core_{tax_year}_{form}_quality.html

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "config.R"))
source(here("R", "data.R"))
source(here("R", "create_logger.R"))

TEMPLATE_PATH <- here::here("docs", "quality_report_template.qmd")

render_one_report <- function(rds_path, out_path, logger = NULL) {
  out_dir <- dirname(out_path)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # quarto::quarto_render writes the output beside the template by default.
  # Use a temp output path then move into place.
  tmp_out <- tempfile(fileext = ".html")

  quarto::quarto_render(
    input          = TEMPLATE_PATH,
    output_format  = "html",
    output_file    = basename(tmp_out),
    execute_params = list(report_data_path = normalizePath(rds_path)),
    quiet          = TRUE
  )

  # Quarto writes next to the template; move + rename to the requested output_path.
  rendered <- file.path(dirname(TEMPLATE_PATH), basename(tmp_out))
  if (!file.exists(rendered)) {
    if (!is.null(logger)) log4r::error(logger, sprintf("Render produced no output for %s", rds_path))
    return(invisible(FALSE))
  }
  file.rename(rendered, out_path)
  if (!is.null(logger)) log4r::info(logger, sprintf("WROTE %s", out_path))
  invisible(TRUE)
}

run_render_reports <- function(logs_dir = PATHS$logs,
                               processed_root = PATHS$processed) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("'quarto' R package is required. Install with install.packages('quarto')")
  }
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  logger <- create_logger(file.path(logs_dir, "07_render_report_log.txt"))

  rds_files <- list.files(logs_dir, pattern = "^quality_[^.]+\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0L) {
    log4r::warn(logger, "No quality RDS reports found; run R/05_quality.R first.")
    return(invisible(NULL))
  }

  n <- 0L
  for (rds in rds_files) {
    base <- sub("^quality_", "", sub("\\.rds$", "", basename(rds)))
    parts <- regmatches(base, regexec("^(.+)_([0-9]{4})$", base))[[1]]
    if (length(parts) != 3L) {
      log4r::warn(logger, sprintf("Skipping un-parseable RDS name: %s", basename(rds)))
      next
    }
    form     <- parts[2]
    tax_year <- as.integer(parts[3])

    out_path <- file.path(processed_root, tax_year, form,
                          sprintf("core_%d_%s_quality.html", tax_year, form))
    ok <- render_one_report(rds, out_path, logger)
    if (isTRUE(ok)) n <- n + 1L
  }
  log4r::info(logger, sprintf("Render run complete: %d HTML reports written", n))
  invisible(NULL)
}

if (sys.nframe() == 0L) run_render_reports()
