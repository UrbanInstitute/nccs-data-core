# R/13_pre_2012_copy_aside.R
#
# Copies the published pre-2012 CORE files aside, once, before the
# corrected files replace them (ADR 0057 Decision 5).
#
# Terms used in this file
#
#   live folder        One of the two S3 folders users download from that
#                      this correction changes: processed_legacy/core/ (the
#                      pre-2012 CORE) and processed_merged/core/ (the merged
#                      panel). Each is handled on its own.
#   year folder        A folder of one tax year inside a live folder, for
#                      example 2005/. Only the year folders 1987 to 2011
#                      change.
#   original           A file that sits in a year folder of a live folder
#                      before the corrected files are uploaded.
#   copy-aside folder  _superseded-2026-11/ inside the live folder. Every
#                      original is copied there under the same key and
#                      kept for good.
#   copy-aside plan    A JSON file listing every original with its sha256
#                      and size. It is written once, before anything is
#                      copied, and never rewritten.
#
# "Key" and "storage" are defined at the top of R/11_harmonized_upload.R.
# The copying itself is the same code that copies the harmonized CORE
# originals aside (`copy_originals_aside()` in that file).
#
# What this file does not do: it does not replace, change or delete any
# file in a year folder. The corrected files are uploaded afterwards by
# the pipeline's own upload step, and only after the `copy` step here has
# finished. Every write to S3 needs the maintainer's go-ahead first.
#
# Usage (from the repo root, on the EC2 instance that holds the build):
#   Rscript R/13_pre_2012_copy_aside.R plan <live_folder_name> <work_root>
#   Rscript R/13_pre_2012_copy_aside.R copy <live_folder_name> <work_root>
#
#   live_folder_name  processed_legacy or processed_merged
#   plan  downloads every original and writes the copy-aside plan. Changes
#         nothing on S3.
#   copy  copies the originals into the copy-aside folder and reads every
#         copy back. Run again, it only reads the copies back.

source(file.path("R", "11_harmonized_upload.R"))

# ---- Constants ----

PRE_2012_LIVE_S3_URIS <- c(
  processed_legacy = "s3://nccsdata/processed_legacy/core",
  processed_merged = "s3://nccsdata/processed_merged/core"
)

PRE_2012_COPY_ASIDE_FOLDER <- "_superseded-2026-11"

# The year folders the pre-2012 CORE holds. The same years of the merged
# panel are built from them.
PRE_2012_YEARS <- 1987:2011

PRE_2012_COPY_ASIDE_DESCRIPTIONS <- c(
  processed_legacy = "The pre-2012 CORE files as published until the ADR 0057 correction, which keeps one row per return.",
  processed_merged = "The merged panel files for tax years 1987 to 2011 as published until the ADR 0057 correction, which keeps one row per return."
)

# ---- The copy-aside plan ----

#' List every original in the year folders of a live folder
#'
#' @details Stops when a year folder holds no file. An empty listing for a
#'   year the folder is known to hold means the listing went wrong, and a
#'   plan written from it would leave that year's files out.
#'
#' @param storage The live folder's storage.
#' @param years The year folders to copy aside.
#' @return A character vector of keys.
list_original_keys <- function(storage, years) {
  year_folders <- as.character(years)

  keys_by_year <- purrr::map(year_folders, storage$list_keys)

  empty_years <- year_folders[lengths(keys_by_year) == 0]

  if (length(empty_years) > 0) {
    stop("No file found in year folder(s): ", paste(empty_years, collapse = ", "))
  }

  return(unlist(keys_by_year))
}

#' Download one original and describe it for the copy-aside plan
#'
#' @details The local copy is kept: the `copy` step uploads from it.
#'
#' @param key The key of the original.
#' @param originals_root Local folder the originals are downloaded into.
#' @param storage The live folder's storage.
#' @return A one-row tibble: `key`, `original_sha256` and `original_bytes`.
describe_original <- function(key, originals_root, storage) {
  original_path <- file.path(originals_root, key)

  storage$download(key, original_path)

  original_description <- tibble::tibble(
    key = key,
    original_sha256 = sha256_of_file(original_path),
    original_bytes = as.numeric(file.size(original_path))
  )

  return(original_description)
}

#' Write the copy-aside plan, once
#'
#' @details
#' Stops if the plan file already exists. A plan written after the
#' corrected files were uploaded would record corrected files as
#' "originals", and the copy-aside folder would then keep the wrong files.
#' To start over before anything is copied, a person removes the plan file
#' by hand.
#'
#' @param storage The live folder's storage.
#' @param originals_root Local folder the originals are downloaded into.
#' @param plan_path Where to write the plan (JSON).
#' @param years The year folders to copy aside.
#' @return The plan as a tibble: `key`, `original_sha256`, `original_bytes`.
write_copy_aside_plan <- function(storage, originals_root, plan_path, years = PRE_2012_YEARS) {
  if (file.exists(plan_path)) {
    stop("The copy-aside plan already exists and is never rewritten: ", plan_path)
  }

  original_keys <- list_original_keys(storage, years)

  copy_aside_plan <- original_keys |>
    purrr::map(describe_original, originals_root = originals_root, storage = storage) |>
    purrr::list_rbind()

  jsonlite::write_json(copy_aside_plan, plan_path, pretty = TRUE, digits = NA)

  return(copy_aside_plan)
}

#' Read the copy-aside plan
#'
#' @param plan_path The plan file written by `write_copy_aside_plan()`.
#' @return The plan as a tibble.
read_copy_aside_plan <- function(plan_path) {
  if (!file.exists(plan_path)) {
    stop("No copy-aside plan at ", plan_path, ". Run the plan step first.")
  }

  copy_aside_plan <- tibble::as_tibble(jsonlite::fromJSON(plan_path))

  return(copy_aside_plan)
}

# ---- Copying ----

#' Copy the originals of one live folder aside, or verify an earlier copy
#'
#' @details Uses `copy_originals_aside()`, with this correction's
#'   copy-aside folder. On a first run it uploads each original from its
#'   local copy, reads every copy back, and writes the small file
#'   `_superseded-2026-11/_manifest.json` last. Run again, it writes
#'   nothing: it reads the copies back and stops if any differs from the
#'   plan.
#'
#' @param work_root Local folder holding the copy-aside plan
#'   (`copy_aside_plan.json`), the downloaded originals (`originals/`) and
#'   short-lived files (`scratch/`).
#' @param storage The live folder's storage.
#' @param copy_aside_description What the small file says the folder holds.
#' @return "copied" when this call made the copies, "verified" when they
#'   were already there.
run_pre_2012_copy_aside <- function(work_root, storage, copy_aside_description) {
  copy_aside_plan <- read_copy_aside_plan(file.path(work_root, "copy_aside_plan.json"))

  copy_aside_result <- copy_originals_aside(
    copy_aside_plan,
    originals_root = file.path(work_root, "originals"),
    storage = storage,
    scratch_root = file.path(work_root, "scratch"),
    copy_aside_folder = PRE_2012_COPY_ASIDE_FOLDER,
    copy_aside_description = copy_aside_description
  )

  return(copy_aside_result)
}

# ---- Command line ----

if (sys.nframe() == 0) {
  arguments <- commandArgs(trailingOnly = TRUE)

  step <- arguments[[1]]
  live_folder_name <- arguments[[2]]
  work_root <- arguments[[3]]

  if (!live_folder_name %in% names(PRE_2012_LIVE_S3_URIS)) {
    stop("The live folder must be one of: ", paste(names(PRE_2012_LIVE_S3_URIS), collapse = ", "))
  }

  live_storage <- s3_storage(PRE_2012_LIVE_S3_URIS[[live_folder_name]])

  dir.create(work_root, recursive = TRUE, showWarnings = FALSE)

  if (step == "plan") {
    copy_aside_plan <- write_copy_aside_plan(
      live_storage,
      file.path(work_root, "originals"),
      file.path(work_root, "copy_aside_plan.json")
    )

    message(nrow(copy_aside_plan), " originals, ", sum(copy_aside_plan$original_bytes), " bytes")
  }

  if (step == "copy") {
    copy_aside_result <- run_pre_2012_copy_aside(
      work_root,
      live_storage,
      PRE_2012_COPY_ASIDE_DESCRIPTIONS[[live_folder_name]]
    )

    message("Copy-aside folder: ", copy_aside_result)
  }
}
