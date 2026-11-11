# R/11_harmonized_upload.R
#
# Replaces the damaged "harmonized" CORE files on S3 with the corrected
# files, once, in the order and with the safeguards of ADR 0056 Decision 13.
#
# Terms used in this file
#
#   live folder         s3://nccsdata/harmonized/core/, the folder users
#                       download from.
#   replacement folder  A local folder that mirrors the live folder and holds
#                       the corrected files, as written by
#                       R/run_harmonized_rewrite.R.
#   key                 The path of one file inside the live folder, for
#                       example 501c3-pz/marts/CORE-2005-501C3-CHARITIES-PZ-HRMN-V1.csv.
#   replacement         The corrected file for a key.
#   original            The file that sits at a key in the live folder before
#                       this upload starts.
#   added file          A replacement whose key has no original (the parquet
#                       file beside each yearly CSV).
#   copy-aside folder   _superseded-2025-04/ inside the live folder. Every
#                       original is copied there, under the same key, before
#                       anything is replaced.
#   upload plan         A JSON file listing every key with the sha256 and
#                       size of its replacement and of its original. It is
#                       written once, before the first upload, and never
#                       rewritten. Every later step works from it.
#   action              What the upload does with one key, decided by
#                       comparing the live file with the upload plan:
#                       "replace", "already_replaced" or "stop".
#   storage             A list of three functions (exists, download, upload)
#                       that read and write one folder of files by key. The
#                       live folder on S3 is one storage; the tests use a
#                       local folder as another.
#
# What this file does not do: it does not build any file, it does not delete
# anything, and it does nothing unless a person runs it. Every upload to the
# live folder needs the maintainer's go-ahead first.
#
# Usage (from the repo root, on the EC2 instance that holds the build):
#   Rscript R/11_harmonized_upload.R plan    <replacement_root> <work_root>
#   Rscript R/11_harmonized_upload.R check   <replacement_root> <work_root>
#   Rscript R/11_harmonized_upload.R upload  <replacement_root> <work_root>
#
#   plan    downloads every original and writes the upload plan. Changes
#           nothing on S3.
#   check   prints the action for every key. Changes nothing on S3.
#   upload  copies the originals aside, then replaces the files.

# ---- Constants ----

HARMONIZED_LIVE_S3_URI <- "s3://nccsdata/harmonized/core"

COPY_ASIDE_FOLDER <- "_superseded-2025-04"

COPY_ASIDE_MANIFEST_KEY <- file.path(COPY_ASIDE_FOLDER, "_manifest.json")

FOLDER_MANIFEST_KEY <- "_manifest.json"

NOTICE_KEY <- "README.txt"

# The last year the live folder holds. Later years built locally are left
# out (ADR 0056 Decision 3).
LAST_YEAR_REPLACED <- 2023L

# The three groups of replacement files, in upload order, each with the
# pattern its keys match.
UPLOAD_GROUP_PATTERNS <- c(
  yearly = "^[^/]+/marts/CORE-[0-9]{4}-.+-HRMN-V1\\.(csv|parquet)$",
  all_years = "^[^/]+/full/CORE-FULL-.+-HRMN-V1\\.parquet$",
  dictionary = "^([^/]+/dd/DD-CORE-.+-HRMN-V1\\.csv|dd/HARMONIZED-COLUMN-MAP\\.csv)$"
)

ACTION_REPLACE <- "replace"
ACTION_ALREADY_REPLACED <- "already_replaced"
ACTION_STOP <- "stop"

# ---- Storage ----

#' Make a storage that reads and writes a local folder
#'
#' @details Used by the tests in place of S3, and usable for a trial run.
#'
#' @param root The local folder.
#' @return A storage: a list with `exists(key)`, `download(key, local_path)`
#'   and `upload(local_path, key)`.
folder_storage <- function(root) {
  storage <- list(
    exists = function(key) {
      return(file.exists(file.path(root, key)))
    },
    download = function(key, local_path) {
      copy_file_creating_folder(file.path(root, key), local_path)

      return(invisible(local_path))
    },
    upload = function(local_path, key) {
      copy_file_creating_folder(local_path, file.path(root, key))

      return(invisible(key))
    }
  )

  return(storage)
}

#' Make a storage that reads and writes one S3 folder with the AWS CLI
#'
#' @details Each function stops with an error when the AWS CLI reports a
#'   failure, except `exists`, which returns FALSE when the key is absent.
#'
#' @param s3_uri The S3 folder, for example `s3://nccsdata/harmonized/core`.
#' @return A storage: a list with `exists(key)`, `download(key, local_path)`
#'   and `upload(local_path, key)`.
s3_storage <- function(s3_uri) {
  storage <- list(
    exists = function(key) {
      status <- run_aws(c("s3", "ls", paste0(s3_uri, "/", key)), stop_on_failure = FALSE)

      return(status == 0L)
    },
    download = function(key, local_path) {
      dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)

      run_aws(c("s3", "cp", paste0(s3_uri, "/", key), local_path, "--only-show-errors"))

      return(invisible(local_path))
    },
    upload = function(local_path, key) {
      run_aws(c("s3", "cp", local_path, paste0(s3_uri, "/", key), "--only-show-errors"))

      return(invisible(key))
    }
  )

  return(storage)
}

#' Run one AWS CLI command
#'
#' @param arguments The arguments after `aws`, as a character vector.
#' @param stop_on_failure When TRUE, stop with an error if the command
#'   fails.
#' @param command The program to run. Always "aws" outside the tests.
#' @return The exit status of the command, 0 for success.
run_aws <- function(arguments, stop_on_failure = TRUE, command = "aws") {
  status <- system2(command, args = shQuote(arguments), stdout = FALSE, stderr = FALSE)

  if (stop_on_failure && status != 0L) {
    stop("AWS command failed: aws ", paste(arguments, collapse = " "))
  }

  return(status)
}

#' Copy one file, creating the destination folder first
#'
#' @param from The file to copy.
#' @param to The path to copy it to.
#' @return `to`, invisibly. Stops if the copy fails.
copy_file_creating_folder <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)

  was_copied <- file.copy(from, to, overwrite = TRUE)

  if (!was_copied) {
    stop("Could not copy ", from, " to ", to)
  }

  return(invisible(to))
}

# ---- Reading files ----

#' Give the sha256 of a file, or NA when the file does not exist
#'
#' @param path One local path.
#' @return The sha256 as text, or `NA_character_`.
sha256_of_file <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  sha256 <- digest::digest(path, algo = "sha256", file = TRUE)

  return(sha256)
}

#' Give the sha256 of the file at a key in a storage, or NA when absent
#'
#' @param key One key.
#' @param storage The storage to read.
#' @param scratch_root A local folder the file is downloaded into. The
#'   download is removed again before returning.
#' @return The sha256 as text, or `NA_character_` when the key is absent.
sha256_of_stored_file <- function(key, storage, scratch_root) {
  if (!storage$exists(key)) {
    return(NA_character_)
  }

  local_path <- file.path(scratch_root, key)

  storage$download(key, local_path)

  sha256 <- sha256_of_file(local_path)

  unlink(local_path)

  return(sha256)
}

# ---- The list of replacement files ----

#' Give the upload group of each key, or NA for a key that is not uploaded
#'
#' @param keys Keys relative to the replacement folder.
#' @return A character vector the same length: "yearly", "all_years",
#'   "dictionary" or NA.
upload_group_of <- function(keys) {
  is_yearly <- stringr::str_detect(keys, UPLOAD_GROUP_PATTERNS[["yearly"]])

  is_all_years <- stringr::str_detect(keys, UPLOAD_GROUP_PATTERNS[["all_years"]])

  is_dictionary <- stringr::str_detect(keys, UPLOAD_GROUP_PATTERNS[["dictionary"]])

  upload_group <- dplyr::case_when(
    is_yearly ~ "yearly",
    is_all_years ~ "all_years",
    is_dictionary ~ "dictionary",
    .default = NA_character_
  )

  return(upload_group)
}

#' List the replacement files in upload order
#'
#' @details
#' Keeps three groups of file and nothing else: the yearly files (CSV and
#' the parquet beside it), the all-years parquet files, and the
#' dictionaries with the name table. Yearly files after `last_year` are
#' left out (ADR 0056 Decision 3), and so are the build summaries.
#'
#' The order is the one ADR 0056 Decision 13 sets: yearly files one folder
#' at a time, oldest year first; then the all-years files; then the
#' dictionaries.
#'
#' @param replacement_root The replacement folder.
#' @param last_year The last year to upload.
#' @return A tibble with one row per replacement file: `key`,
#'   `upload_group`, `tax_year` (NA outside the yearly group) and
#'   `upload_order`.
list_replacement_files <- function(replacement_root, last_year = LAST_YEAR_REPLACED) {
  all_keys <- list.files(replacement_root, recursive = TRUE)

  group_order <- names(UPLOAD_GROUP_PATTERNS)

  replacement_files <- tibble::tibble(key = all_keys) |>
    dplyr::mutate(
      upload_group = upload_group_of(key),
      tax_year = as.integer(stringr::str_match(key, "/marts/CORE-([0-9]{4})-")[, 2]),
      is_after_last_year = !is.na(tax_year) & tax_year > last_year,
      group_position = match(upload_group, group_order),
      folder = dirname(key)
    ) |>
    dplyr::filter(!is.na(upload_group), !is_after_last_year) |>
    dplyr::arrange(group_position, folder, tax_year, key) |>
    dplyr::mutate(upload_order = dplyr::row_number()) |>
    dplyr::select(key, upload_group, tax_year, upload_order)

  return(replacement_files)
}

# ---- The upload plan ----

#' Describe one key for the upload plan
#'
#' @details Downloads the original, when there is one, into
#'   `originals_root` under the same key and keeps it there: the copy-aside
#'   step uploads from that local copy, never from the live folder.
#'
#' @param key One key.
#' @param replacement_root The replacement folder.
#' @param originals_root Local folder the originals are downloaded into.
#' @param storage The live folder's storage.
#' @return A one-row tibble: `key`, `replacement_sha256`,
#'   `replacement_bytes`, `original_sha256` and `original_bytes` (both NA
#'   for an added file).
describe_key_for_plan <- function(key, replacement_root, originals_root, storage) {
  replacement_path <- file.path(replacement_root, key)

  original_path <- file.path(originals_root, key)

  has_original <- storage$exists(key)

  if (has_original) {
    storage$download(key, original_path)
  }

  key_description <- tibble::tibble(
    key = key,
    replacement_sha256 = sha256_of_file(replacement_path),
    replacement_bytes = as.numeric(file.size(replacement_path)),
    original_sha256 = sha256_of_file(original_path),
    original_bytes = as.numeric(file.size(original_path))
  )

  return(key_description)
}

#' Write the upload plan, once
#'
#' @details
#' Records, for every replacement file, the sha256 and size of the
#' replacement and of the original it will replace. Changes nothing in the
#' live folder.
#'
#' Stops if the plan file already exists. A plan written after some files
#' were replaced would record replacement bytes as "originals", and the
#' copy-aside would then keep the wrong files (ADR 0056 Decision 13). To
#' start over before any upload, a person removes the plan file by hand.
#'
#' Also stops if there is nothing to upload.
#'
#' @param replacement_root The replacement folder.
#' @param originals_root Local folder the originals are downloaded into.
#' @param storage The live folder's storage.
#' @param plan_path Where to write the plan (JSON).
#' @return The plan as a tibble: the columns of `list_replacement_files()`
#'   and of `describe_key_for_plan()`.
write_upload_plan <- function(replacement_root, originals_root, storage, plan_path) {
  if (file.exists(plan_path)) {
    stop("The upload plan already exists and is never rewritten: ", plan_path)
  }

  replacement_files <- list_replacement_files(replacement_root)

  if (nrow(replacement_files) == 0) {
    stop("No replacement files found in ", replacement_root)
  }

  key_descriptions <- purrr::map(
    replacement_files$key,
    describe_key_for_plan,
    replacement_root = replacement_root,
    originals_root = originals_root,
    storage = storage
  ) |>
    purrr::list_rbind()

  upload_plan <- dplyr::left_join(replacement_files, key_descriptions, by = "key")

  jsonlite::write_json(upload_plan, plan_path, pretty = TRUE, na = "null", digits = NA)

  return(upload_plan)
}

#' Read the upload plan
#'
#' @param plan_path The plan file written by `write_upload_plan()`.
#' @return The plan as a tibble.
read_upload_plan <- function(plan_path) {
  if (!file.exists(plan_path)) {
    stop("No upload plan at ", plan_path, ". Run the plan step first.")
  }

  plan_rows <- jsonlite::fromJSON(plan_path)

  upload_plan <- tibble::as_tibble(plan_rows)

  return(upload_plan)
}

# ---- The action for each key ----

#' Decide what the upload does with each key
#'
#' @details
#' The rule of ADR 0056 Decision 13, for a first run and for a retry alike:
#' \itemize{
#'   \item The live file is the replacement: "already_replaced", skip it.
#'   \item The live file is the original (or the key is absent and has no
#'     original): "replace".
#'   \item Anything else: "stop". The live file is neither, so a person
#'     must look before anything more is written.
#' }
#'
#' @param live_sha256 sha256 of the file now at each key in the live
#'   folder; NA where the key is absent.
#' @param original_sha256 sha256 of each original from the upload plan; NA
#'   for an added file.
#' @param replacement_sha256 sha256 of each replacement from the upload
#'   plan.
#' @return A character vector of actions, one per key.
decide_action <- function(live_sha256, original_sha256, replacement_sha256) {
  is_live_absent <- is.na(live_sha256)

  has_no_original <- is.na(original_sha256)

  is_live_the_replacement <- dplyr::coalesce(live_sha256 == replacement_sha256, FALSE)

  is_live_the_original <- dplyr::coalesce(live_sha256 == original_sha256, FALSE)

  is_absent_as_planned <- is_live_absent & has_no_original

  action <- dplyr::case_when(
    is_live_the_replacement ~ ACTION_ALREADY_REPLACED,
    is_live_the_original ~ ACTION_REPLACE,
    is_absent_as_planned ~ ACTION_REPLACE,
    .default = ACTION_STOP
  )

  return(action)
}

#' Read the live folder and add the action for every key to the plan
#'
#' @details Downloads each live file to take its sha256. Changes nothing in
#'   the live folder.
#'
#' @param upload_plan The upload plan.
#' @param storage The live folder's storage.
#' @param scratch_root Local folder for the short-lived downloads.
#' @return The plan with two more columns: `live_sha256` and `action`.
add_actions_to_plan <- function(upload_plan, storage, scratch_root) {
  live_sha256 <- purrr::map_chr(
    upload_plan$key,
    sha256_of_stored_file,
    storage = storage,
    scratch_root = scratch_root
  )

  plan_with_actions <- upload_plan |>
    dplyr::mutate(
      live_sha256 = live_sha256,
      action = decide_action(live_sha256, original_sha256, replacement_sha256)
    )

  return(plan_with_actions)
}

# ---- Copying the originals aside ----

#' Give the key of an original's copy in the copy-aside folder
#'
#' @param key One or more keys in the live folder.
#' @return The matching keys inside the copy-aside folder.
copy_aside_key <- function(key) {
  return(file.path(COPY_ASIDE_FOLDER, key))
}

#' Check that every copy in the copy-aside folder is the planned original
#'
#' @param originals_in_plan The rows of the upload plan that have an
#'   original.
#' @param storage The live folder's storage.
#' @param scratch_root Local folder for the short-lived downloads.
#' @return The keys whose copy is absent or differs from the original, as
#'   a character vector. Length zero when every copy is right.
find_wrong_copies <- function(originals_in_plan, storage, scratch_root) {
  copy_sha256 <- purrr::map_chr(
    copy_aside_key(originals_in_plan$key),
    sha256_of_stored_file,
    storage = storage,
    scratch_root = scratch_root
  )

  is_copy_right <- dplyr::coalesce(copy_sha256 == originals_in_plan$original_sha256, FALSE)

  wrong_keys <- originals_in_plan$key[!is_copy_right]

  return(wrong_keys)
}

#' Upload one original from its local copy to the copy-aside folder
#'
#' @details Stops if the local copy no longer has the sha256 the plan
#'   recorded, so a changed or truncated file is never kept as "the
#'   original".
#'
#' @param key The key of the original.
#' @param planned_sha256 The original's sha256 from the upload plan.
#' @param originals_root Local folder holding the downloaded originals.
#' @param storage The live folder's storage.
#' @return `key`, invisibly.
copy_one_original_aside <- function(key, planned_sha256, originals_root, storage) {
  local_path <- file.path(originals_root, key)

  local_sha256 <- sha256_of_file(local_path)

  is_local_copy_right <- dplyr::coalesce(local_sha256 == planned_sha256, FALSE)

  if (!is_local_copy_right) {
    stop("The local copy of the original does not match the upload plan: ", key)
  }

  storage$upload(local_path, copy_aside_key(key))

  return(invisible(key))
}

#' Copy every original aside, once, and verify the copies
#'
#' @details
#' The small file `_superseded-2025-04/_manifest.json` marks a finished
#' copy-aside: it is written last, after every copy has been read back and
#' matched to the upload plan.
#'
#' When that file already exists, nothing is copied. The copies are read
#' back and matched to the upload plan, and the run stops if any differs.
#' So a retry never writes into the copy-aside folder, and never copies a
#' live file there (ADR 0056 Decision 13).
#'
#' When it does not exist, each original is uploaded from its local copy
#' (downloaded by the plan step, before anything was replaced), the copies
#' are read back and matched, and the file is written.
#'
#' @param upload_plan The upload plan.
#' @param originals_root Local folder holding the downloaded originals.
#' @param storage The live folder's storage.
#' @param scratch_root Local folder for short-lived files.
#' @return "copied" when this call made the copies, "verified" when they
#'   were already there. Stops when a copy is absent or wrong.
copy_originals_aside <- function(upload_plan, originals_root, storage, scratch_root) {
  originals_in_plan <- dplyr::filter(upload_plan, !is.na(original_sha256))

  is_copy_aside_finished <- storage$exists(COPY_ASIDE_MANIFEST_KEY)

  if (!is_copy_aside_finished) {
    purrr::walk2(
      originals_in_plan$key,
      originals_in_plan$original_sha256,
      copy_one_original_aside,
      originals_root = originals_root,
      storage = storage
    )
  }

  wrong_keys <- find_wrong_copies(originals_in_plan, storage, scratch_root)

  if (length(wrong_keys) > 0) {
    stop(
      "The copy-aside folder does not hold the planned original for: ",
      paste(wrong_keys, collapse = ", ")
    )
  }

  if (is_copy_aside_finished) {
    return("verified")
  }

  write_copy_aside_manifest(originals_in_plan, storage, scratch_root)

  return("copied")
}

#' Write the small file that records what the copy-aside folder holds
#'
#' @param originals_in_plan The rows of the upload plan that have an
#'   original.
#' @param storage The live folder's storage.
#' @param scratch_root Local folder the file is written into before upload.
#' @return The key of the file, invisibly.
write_copy_aside_manifest <- function(originals_in_plan, storage, scratch_root) {
  copy_aside_files <- originals_in_plan |>
    dplyr::transmute(file = key, bytes = original_bytes, sha256 = original_sha256)

  copy_aside_manifest <- list(
    what = "Originals of the harmonized CORE files replaced under ADR 0056, as published from April 2025 until the replacement.",
    written_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    integrity_basis = "sha256",
    object_count = nrow(copy_aside_files),
    total_bytes = sum(copy_aside_files$bytes),
    files = copy_aside_files
  )

  local_path <- file.path(scratch_root, "copy_aside_manifest.json")

  dir.create(scratch_root, recursive = TRUE, showWarnings = FALSE)

  jsonlite::write_json(copy_aside_manifest, local_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)

  storage$upload(local_path, COPY_ASIDE_MANIFEST_KEY)

  return(invisible(COPY_ASIDE_MANIFEST_KEY))
}

# ---- Replacing the files ----

#' Upload one replacement to its key in the live folder
#'
#' @param key The key.
#' @param replacement_root The replacement folder.
#' @param storage The live folder's storage.
#' @return `key`, invisibly.
upload_one_replacement <- function(key, replacement_root, storage) {
  message("Replacing ", key)

  storage$upload(file.path(replacement_root, key), key)

  return(invisible(key))
}

#' Replace the files in the live folder, in plan order
#'
#' @details
#' Reads every live file first and decides the action for every key. If
#' any key's action is "stop", nothing is uploaded and the run stops with
#' the list of those keys. Otherwise the keys to replace are uploaded in
#' plan order, and keys already replaced by an earlier run are skipped.
#'
#' Stops, uploading nothing, unless the copy-aside folder is finished.
#'
#' @param upload_plan The upload plan.
#' @param replacement_root The replacement folder.
#' @param storage The live folder's storage.
#' @param scratch_root Local folder for short-lived files.
#' @return The plan with `live_sha256` and `action` as they were before
#'   this call uploaded anything.
replace_live_files <- function(upload_plan, replacement_root, storage, scratch_root) {
  if (!storage$exists(COPY_ASIDE_MANIFEST_KEY)) {
    stop("The originals have not been copied aside yet. Nothing was replaced.")
  }

  plan_with_actions <- add_actions_to_plan(upload_plan, storage, scratch_root)

  keys_to_stop_on <- plan_with_actions$key[plan_with_actions$action == ACTION_STOP]

  if (length(keys_to_stop_on) > 0) {
    stop(
      "The live file is neither the original nor the replacement. Nothing was replaced. Look at: ",
      paste(keys_to_stop_on, collapse = ", ")
    )
  }

  keys_to_replace <- plan_with_actions |>
    dplyr::filter(action == ACTION_REPLACE) |>
    dplyr::arrange(upload_order) |>
    dplyr::pull(key)

  purrr::walk(
    keys_to_replace,
    upload_one_replacement,
    replacement_root = replacement_root,
    storage = storage
  )

  return(plan_with_actions)
}

# ---- The notice and the folder's build record ----

#' Write the text of the notice file for the live folder
#'
#' @details The notice is `README.txt` in the live folder (ADR 0056
#'   Decision 12). It is uploaded twice: first saying the replacement is in
#'   progress and listing the files, then saying it is complete.
#'
#' @param upload_plan The upload plan.
#' @param is_complete FALSE for the notice written before the first
#'   replacement, TRUE for the one written after the last.
#' @return The notice as one character string.
build_notice_text <- function(upload_plan, is_complete) {
  status_line <- "STATUS: REPLACEMENT IN PROGRESS. Some files listed below are corrected and some are not yet."

  if (is_complete) {
    status_line <- "STATUS: REPLACEMENT COMPLETE. Every file listed below is corrected."
  }

  notice_lines <- c(
    "NCCS harmonized CORE files: correction notice",
    "",
    status_line,
    paste("Written:", format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")),
    "",
    "What was wrong. In the yearly files published here from April 2025, many",
    "dollar amounts were replaced by a meaningless number very close to zero",
    "(about 1.11e-308).",
    "",
    "What was done. Each damaged file was replaced at the same path with the",
    "values of the current NCCS CORE, under the same column names. Five columns",
    "were added at the end of each file: ein, tax_period, source_form,",
    "is_amendment and extract_year. A parquet copy sits beside each yearly CSV.",
    "",
    paste0("Where the earlier files are. Unchanged copies are in ", COPY_ASIDE_FOLDER, "/"),
    "under the same paths, with a list of their sha256 checksums.",
    "",
    paste0("These files stop at ", LAST_YEAR_REPLACED, " and will not be updated. For newer years and"),
    "later corrections, use the current CORE: https://nccs.urban.org/nccs/catalogs/catalog-core.html",
    "",
    "Private foundation files and the base files (names without -V1) were not",
    "damaged and were not changed.",
    "",
    "Files replaced or added:",
    paste0("  ", upload_plan$key)
  )

  notice_text <- paste(notice_lines, collapse = "\n")

  return(notice_text)
}

#' Upload the notice file to the live folder
#'
#' @param upload_plan The upload plan.
#' @param is_complete See `build_notice_text()`.
#' @param storage The live folder's storage.
#' @param scratch_root Local folder the file is written into before upload.
#' @return The key of the notice, invisibly.
upload_notice <- function(upload_plan, is_complete, storage, scratch_root) {
  local_path <- file.path(scratch_root, NOTICE_KEY)

  dir.create(scratch_root, recursive = TRUE, showWarnings = FALSE)

  writeLines(build_notice_text(upload_plan, is_complete), local_path)

  storage$upload(local_path, NOTICE_KEY)

  return(invisible(NOTICE_KEY))
}

#' Bring the live folder's build record up to date
#'
#' @details
#' The live folder has a `_manifest.json` that lists every file with its
#' size and ETag. This function keeps the entries of the files that were
#' not touched exactly as they are, and gives each replaced or added file
#' a new entry with its size and sha256. It also records the correction.
#'
#' @param existing_manifest The live folder's `_manifest.json`, read with
#'   `jsonlite::fromJSON(simplifyVector = FALSE)`.
#' @param upload_plan The upload plan.
#' @param git_sha The commit of this repository the replacements were
#'   built from.
#' @return The updated record, as a list ready for `jsonlite::write_json()`.
update_folder_manifest <- function(existing_manifest, upload_plan, git_sha) {
  replaced_on <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  new_entries <- purrr::pmap(
    list(
      file = upload_plan$key,
      bytes = upload_plan$replacement_bytes,
      sha256 = upload_plan$replacement_sha256
    ),
    list
  ) |>
    purrr::map(append, list(replaced_on = replaced_on)) |>
    rlang::set_names(upload_plan$key)

  untouched_keys <- setdiff(names(existing_manifest$files), upload_plan$key)

  all_entries <- c(existing_manifest$files[untouched_keys], new_entries)

  all_entries <- all_entries[order(names(all_entries))]

  updated_manifest <- existing_manifest

  updated_manifest$files <- all_entries

  updated_manifest$object_count <- length(all_entries)

  updated_manifest$total_bytes <- sum(purrr::map_dbl(all_entries, "bytes"))

  updated_manifest$integrity_basis <- "sha256 for files replaced under ADR 0056; etag+size for the files not touched"

  updated_manifest$correction <- list(
    adr = "0056",
    replaced_on = replaced_on,
    git_sha = git_sha,
    files_replaced = sum(!is.na(upload_plan$original_sha256)),
    files_added = sum(is.na(upload_plan$original_sha256)),
    originals_copied_to = paste0(COPY_ASIDE_FOLDER, "/")
  )

  return(updated_manifest)
}

#' Download, update and upload the live folder's build record
#'
#' @param upload_plan The upload plan.
#' @param storage The live folder's storage.
#' @param scratch_root Local folder for short-lived files.
#' @param git_sha The commit the replacements were built from.
#' @return The key of the record, invisibly.
upload_folder_manifest <- function(upload_plan, storage, scratch_root, git_sha) {
  local_path <- file.path(scratch_root, "folder_manifest.json")

  storage$download(FOLDER_MANIFEST_KEY, local_path)

  existing_manifest <- jsonlite::fromJSON(local_path, simplifyVector = FALSE)

  updated_manifest <- update_folder_manifest(existing_manifest, upload_plan, git_sha)

  jsonlite::write_json(updated_manifest, local_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)

  storage$upload(local_path, FOLDER_MANIFEST_KEY)

  return(invisible(FOLDER_MANIFEST_KEY))
}

# ---- The whole upload ----

#' Run the upload from start to finish, in the order of ADR 0056 Decision 13
#'
#' @details
#' The order:
#' \enumerate{
#'   \item Copy the originals aside, or verify an earlier copy.
#'   \item Write the notice saying the replacement is in progress.
#'   \item Replace the files.
#'   \item Update the folder's build record.
#'   \item Write the notice saying the replacement is complete.
#' }
#' Safe to run again after an interruption: files already
#' replaced are skipped, and the copy-aside folder is never rewritten.
#'
#' @param replacement_root The replacement folder.
#' @param work_root Local folder holding the upload plan
#'   (`upload_plan.json`), the downloaded originals (`originals/`) and
#'   short-lived files (`scratch/`).
#' @param storage The live folder's storage.
#' @param git_sha The commit the replacements were built from.
#' @return The plan with the action each key had when the run started.
run_harmonized_upload <- function(replacement_root, work_root, storage, git_sha) {
  upload_plan <- read_upload_plan(file.path(work_root, "upload_plan.json"))

  originals_root <- file.path(work_root, "originals")

  scratch_root <- file.path(work_root, "scratch")

  copy_aside_result <- copy_originals_aside(upload_plan, originals_root, storage, scratch_root)

  message("Copy-aside folder: ", copy_aside_result)

  upload_notice(upload_plan, is_complete = FALSE, storage = storage, scratch_root = scratch_root)

  plan_with_actions <- replace_live_files(upload_plan, replacement_root, storage, scratch_root)

  upload_folder_manifest(upload_plan, storage, scratch_root, git_sha)

  upload_notice(upload_plan, is_complete = TRUE, storage = storage, scratch_root = scratch_root)

  return(plan_with_actions)
}

# ---- Command line ----

if (sys.nframe() == 0) {
  arguments <- commandArgs(trailingOnly = TRUE)

  step <- arguments[[1]]
  replacement_root <- arguments[[2]]
  work_root <- arguments[[3]]

  live_storage <- s3_storage(HARMONIZED_LIVE_S3_URI)

  plan_path <- file.path(work_root, "upload_plan.json")

  dir.create(work_root, recursive = TRUE, showWarnings = FALSE)

  if (step == "plan") {
    upload_plan <- write_upload_plan(
      replacement_root,
      file.path(work_root, "originals"),
      live_storage,
      plan_path
    )

    print(dplyr::count(upload_plan, upload_group, has_original = !is.na(original_sha256)))
  }

  if (step == "check") {
    plan_with_actions <- add_actions_to_plan(
      read_upload_plan(plan_path),
      live_storage,
      file.path(work_root, "scratch")
    )

    print(dplyr::count(plan_with_actions, upload_group, action))
  }

  if (step == "upload") {
    git_sha <- system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)

    plan_with_actions <- run_harmonized_upload(replacement_root, work_root, live_storage, git_sha)

    print(dplyr::count(plan_with_actions, upload_group, action))
  }
}
