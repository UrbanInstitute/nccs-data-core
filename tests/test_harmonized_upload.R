# tests/test_harmonized_upload.R
# Tests for R/11_harmonized_upload.R (ADR 0056 Decision 13): which files
# are uploaded and in what order, the upload plan, copying the originals
# aside once, what happens to each file on a first run and on a retry, the
# notice and the folder's build record. A local folder stands in for S3.
# Run from repo root:  Rscript tests/test_harmonized_upload.R
#
# "Key", "original", "replacement" and "copy-aside folder" are defined at
# the top of R/11_harmonized_upload.R.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "11_harmonized_upload.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

error_message <- function(expr) {
  tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
}

# ---- A small live folder and a small replacement folder ----

write_text_file <- function(root, key, text) {
  path <- file.path(root, key)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path)
  return(invisible(path))
}

read_text_file <- function(root, key) {
  return(paste(readLines(file.path(root, key)), collapse = "\n"))
}

key_pz_2005_csv <- "501c3-pz/marts/CORE-2005-501C3-CHARITIES-PZ-HRMN-V1.csv"
key_pz_2005_parquet <- "501c3-pz/marts/CORE-2005-501C3-CHARITIES-PZ-HRMN-V1.parquet"
key_pz_2023_csv <- "501c3-pz/marts/CORE-2023-501C3-CHARITIES-PZ-HRMN-V1.csv"
key_pz_2024_csv <- "501c3-pz/marts/CORE-2024-501C3-CHARITIES-PZ-HRMN-V1.csv"
key_pc_2012_csv <- "501c3-pc/marts/CORE-2012-501C3-CHARITIES-PC-HRMN-V1.csv"
key_pz_all_years <- "501c3-pz/full/CORE-FULL-501C3-CHARITIES-PZ-HRMN-V1.parquet"
key_pz_dictionary <- "501c3-pz/dd/DD-CORE-501C3-CHARITIES-PZ-HRMN-V1.csv"
key_name_table <- "dd/HARMONIZED-COLUMN-MAP.csv"
key_base_file <- "501c3-pz/CORE-2005-501C3-CHARITIES-PZ-HRMN.csv"
key_private_foundation <- "501c3-pf/marts/CORE-2005-501C3-PRIVFOUND-PF-HRMN-V0.csv"

make_folders <- function() {
  top <- tempfile("harmonized_upload_")
  live_root <- file.path(top, "live")
  replacement_root <- file.path(top, "replacement")
  work_root <- file.path(top, "work")
  dir.create(work_root, recursive = TRUE)

  # The live folder: damaged originals, plus two files that are never touched.
  write_text_file(live_root, key_pz_2005_csv, "damaged 2005")
  write_text_file(live_root, key_pz_2023_csv, "damaged 2023")
  write_text_file(live_root, key_pc_2012_csv, "damaged pc 2012")
  write_text_file(live_root, key_pz_all_years, "damaged all years")
  write_text_file(live_root, key_pz_dictionary, "old dictionary")
  write_text_file(live_root, key_base_file, "clean base file")
  write_text_file(live_root, key_private_foundation, "clean private foundation file")
  jsonlite::write_json(
    list(
      vintage = "frozen",
      object_count = 7,
      total_bytes = 1,
      files = list(
        "501c3-pz/CORE-2005-501C3-CHARITIES-PZ-HRMN.csv" = list(
          file = key_base_file, bytes = 16, etag = "abc-1"
        ),
        "501c3-pz/marts/CORE-2005-501C3-CHARITIES-PZ-HRMN-V1.csv" = list(
          file = key_pz_2005_csv, bytes = 13, etag = "def-1"
        )
      )
    ),
    file.path(live_root, "_manifest.json"),
    auto_unbox = TRUE
  )

  # The replacement folder: corrected files, a 2024 file and a build summary.
  write_text_file(replacement_root, key_pz_2005_csv, "corrected 2005")
  write_text_file(replacement_root, key_pz_2005_parquet, "corrected 2005 parquet")
  write_text_file(replacement_root, key_pz_2023_csv, "corrected 2023")
  write_text_file(replacement_root, key_pz_2024_csv, "partial 2024")
  write_text_file(replacement_root, key_pc_2012_csv, "corrected pc 2012")
  write_text_file(replacement_root, key_pz_all_years, "corrected all years")
  write_text_file(replacement_root, key_pz_dictionary, "new dictionary")
  write_text_file(replacement_root, key_name_table, "name table")
  write_text_file(replacement_root, "_build_2005.json", "{}")

  folders <- list(
    live_root = live_root,
    replacement_root = replacement_root,
    work_root = work_root,
    originals_root = file.path(work_root, "originals"),
    scratch_root = file.path(work_root, "scratch"),
    plan_path = file.path(work_root, "upload_plan.json"),
    storage = folder_storage(live_root)
  )

  return(folders)
}

make_plan <- function(folders) {
  upload_plan <- write_upload_plan(
    folders$replacement_root, folders$originals_root, folders$storage, folders$plan_path
  )

  return(upload_plan)
}

run_upload <- function(folders) {
  result <- run_harmonized_upload(
    folders$replacement_root, folders$work_root, folders$storage, git_sha = "abc1234"
  )

  return(result)
}

# ---- Which files are uploaded, and in what order ----

cat("list_replacement_files\n")

folders <- make_folders()
replacement_files <- list_replacement_files(folders$replacement_root)

check(
  "2024 files are left out",
  !key_pz_2024_csv %in% replacement_files$key
)

check(
  "the last year kept is 2023",
  key_pz_2023_csv %in% replacement_files$key
)

check(
  "build summaries are left out",
  !"_build_2005.json" %in% replacement_files$key
)

check(
  "order: yearly files one folder at a time and oldest first, then all-years, then dictionaries",
  identical(
    replacement_files$key,
    c(
      key_pc_2012_csv,
      key_pz_2005_csv, key_pz_2005_parquet, key_pz_2023_csv,
      key_pz_all_years,
      key_pz_dictionary, key_name_table
    )
  )
)

check(
  "a later last year lets later files in",
  key_pz_2024_csv %in% list_replacement_files(folders$replacement_root, last_year = 2024L)$key
)

check(
  "a private foundation file in the replacement folder is never uploaded",
  {
    write_text_file(folders$replacement_root, key_private_foundation, "should not be here")
    !key_private_foundation %in% list_replacement_files(folders$replacement_root)$key
  }
)

# ---- The upload plan ----

cat("write_upload_plan\n")

folders <- make_folders()
upload_plan <- make_plan(folders)
plan_row <- function(key) upload_plan[upload_plan$key == key, ]

check(
  "the plan records the sha256 of the replacement and of the original",
  plan_row(key_pz_2005_csv)$replacement_sha256 ==
    sha256_of_file(file.path(folders$replacement_root, key_pz_2005_csv)) &&
    plan_row(key_pz_2005_csv)$original_sha256 ==
      sha256_of_file(file.path(folders$live_root, key_pz_2005_csv))
)

check(
  "the plan records the sizes",
  plan_row(key_pz_2005_csv)$replacement_bytes ==
    file.size(file.path(folders$replacement_root, key_pz_2005_csv)) &&
    plan_row(key_pz_2005_csv)$original_bytes ==
      file.size(file.path(folders$live_root, key_pz_2005_csv))
)

check(
  "an added file has no original in the plan",
  is.na(plan_row(key_pz_2005_parquet)$original_sha256) &&
    is.na(plan_row(key_pz_2005_parquet)$original_bytes)
)

check(
  "the originals are downloaded and kept locally",
  read_text_file(folders$originals_root, key_pz_2005_csv) == "damaged 2005"
)

check(
  "writing the plan changes nothing in the live folder",
  read_text_file(folders$live_root, key_pz_2005_csv) == "damaged 2005" &&
    !file.exists(file.path(folders$live_root, COPY_ASIDE_FOLDER))
)

check(
  "the plan read back from its file equals the plan written",
  isTRUE(all.equal(
    as.data.frame(read_upload_plan(folders$plan_path)),
    as.data.frame(upload_plan)
  ))
)

check(
  "the plan is never rewritten",
  grepl("never rewritten", error_message(make_plan(folders)))
)

check(
  "an empty replacement folder is refused",
  {
    empty_folders <- make_folders()
    unlink(empty_folders$replacement_root, recursive = TRUE)
    dir.create(empty_folders$replacement_root)
    grepl("No replacement files", error_message(make_plan(empty_folders)))
  }
)

check(
  "reading a plan that was never written is refused",
  grepl("Run the plan step first", error_message(read_upload_plan(file.path(tempdir(), "no_plan.json"))))
)

# ---- The action for each key ----

cat("decide_action\n")

check(
  "live file is the original: replace",
  decide_action("original", "original", "replacement") == "replace"
)

check(
  "live file is the replacement: already replaced",
  decide_action("replacement", "original", "replacement") == "already_replaced"
)

check(
  "live file is neither: stop",
  decide_action("something else", "original", "replacement") == "stop"
)

check(
  "added file not there yet: replace",
  decide_action(NA, NA, "replacement") == "replace"
)

check(
  "added file already there: already replaced",
  decide_action("replacement", NA, "replacement") == "already_replaced"
)

check(
  "a key with an original that has gone missing: stop",
  decide_action(NA, "original", "replacement") == "stop"
)

check(
  "an unplanned file at the key of an added file: stop",
  decide_action("something else", NA, "replacement") == "stop"
)

check(
  "one action per key",
  identical(
    decide_action(c("a", "b", NA), c("a", "x", NA), c("r", "b", "r")),
    c("replace", "already_replaced", "replace")
  )
)

# ---- A first run ----

cat("run_harmonized_upload, first run\n")

folders <- make_folders()
upload_plan <- make_plan(folders)
first_run <- run_upload(folders)

check(
  "every planned file was to be replaced",
  all(first_run$action == "replace")
)

check(
  "the live folder now holds the replacements",
  read_text_file(folders$live_root, key_pz_2005_csv) == "corrected 2005" &&
    read_text_file(folders$live_root, key_pz_2005_parquet) == "corrected 2005 parquet" &&
    read_text_file(folders$live_root, key_pz_all_years) == "corrected all years" &&
    read_text_file(folders$live_root, key_name_table) == "name table"
)

check(
  "the copy-aside folder holds every original under the same key",
  read_text_file(folders$live_root, file.path(COPY_ASIDE_FOLDER, key_pz_2005_csv)) == "damaged 2005" &&
    read_text_file(folders$live_root, file.path(COPY_ASIDE_FOLDER, key_pz_dictionary)) == "old dictionary"
)

check(
  "an added file has no copy in the copy-aside folder",
  !file.exists(file.path(folders$live_root, COPY_ASIDE_FOLDER, key_pz_2005_parquet))
)

check(
  "the 2024 file was not uploaded",
  !file.exists(file.path(folders$live_root, key_pz_2024_csv))
)

check(
  "files outside the plan are untouched",
  read_text_file(folders$live_root, key_base_file) == "clean base file" &&
    read_text_file(folders$live_root, key_private_foundation) == "clean private foundation file"
)

copy_aside_manifest <- jsonlite::fromJSON(file.path(folders$live_root, COPY_ASIDE_MANIFEST_KEY))

check(
  "the copy-aside record lists each original with its sha256 and size",
  setequal(copy_aside_manifest$files$file, upload_plan$key[!is.na(upload_plan$original_sha256)]) &&
    copy_aside_manifest$files$sha256[copy_aside_manifest$files$file == key_pz_2005_csv] ==
      plan_row(key_pz_2005_csv)$original_sha256 &&
    copy_aside_manifest$object_count == 5 &&
    copy_aside_manifest$total_bytes == sum(upload_plan$original_bytes, na.rm = TRUE)
)

notice <- read_text_file(folders$live_root, NOTICE_KEY)

check(
  "the notice says the replacement is complete and lists the files",
  grepl("REPLACEMENT COMPLETE", notice) &&
    !grepl("IN PROGRESS", notice) &&
    grepl(key_pz_2005_csv, notice, fixed = TRUE) &&
    grepl("_superseded-2025-04/", notice, fixed = TRUE)
)

folder_manifest <- jsonlite::fromJSON(file.path(folders$live_root, "_manifest.json"), simplifyVector = FALSE)

check(
  "the folder's build record keeps the entry of an untouched file as it was",
  identical(folder_manifest$files[[key_base_file]]$etag, "abc-1")
)

check(
  "the folder's build record gives a replaced file its new sha256 and size",
  folder_manifest$files[[key_pz_2005_csv]]$sha256 == plan_row(key_pz_2005_csv)$replacement_sha256 &&
    folder_manifest$files[[key_pz_2005_csv]]$bytes == plan_row(key_pz_2005_csv)$replacement_bytes &&
    is.null(folder_manifest$files[[key_pz_2005_csv]]$etag)
)

check(
  "the folder's build record counts every file and names the correction",
  folder_manifest$object_count == 8 &&
    folder_manifest$total_bytes == 16 + sum(upload_plan$replacement_bytes) &&
    grepl("sha256 for files replaced", folder_manifest$integrity_basis) &&
    folder_manifest$correction$originals_copied_to == "_superseded-2025-04/" &&
    folder_manifest$correction$adr == "0056" &&
    folder_manifest$correction$git_sha == "abc1234" &&
    folder_manifest$correction$files_replaced == 5 &&
    folder_manifest$correction$files_added == 2 &&
    folder_manifest$vintage == "frozen"
)

check(
  "the folder's build record lists its files in key order",
  !is.unsorted(names(folder_manifest$files))
)

# ---- Running again ----

cat("run_harmonized_upload, second run\n")

copy_aside_manifest_before <- read_text_file(folders$live_root, COPY_ASIDE_MANIFEST_KEY)
second_run <- run_upload(folders)

check(
  "a second run finds every file already replaced",
  all(second_run$action == "already_replaced")
)

check(
  "a second run never puts replacement bytes in the copy-aside folder",
  read_text_file(folders$live_root, file.path(COPY_ASIDE_FOLDER, key_pz_2005_csv)) == "damaged 2005"
)

check(
  "a second run does not rewrite the copy-aside record",
  read_text_file(folders$live_root, COPY_ASIDE_MANIFEST_KEY) == copy_aside_manifest_before
)

# ---- A run that was interrupted part-way ----

cat("run_harmonized_upload, retry after an interruption\n")

folders <- make_folders()
upload_plan <- make_plan(folders)
copy_originals_aside(upload_plan, folders$originals_root, folders$storage, folders$scratch_root)
folders$storage$upload(file.path(folders$replacement_root, key_pz_2005_csv), key_pz_2005_csv)
retry_run <- run_upload(folders)
retry_action <- function(key) retry_run$action[retry_run$key == key]

check(
  "the retry skips the file replaced before the interruption",
  retry_action(key_pz_2005_csv) == "already_replaced"
)

check(
  "the retry replaces the files that were still original",
  retry_action(key_pz_2023_csv) == "replace" &&
    read_text_file(folders$live_root, key_pz_2023_csv) == "corrected 2023"
)

check(
  "after the retry the copy-aside folder still holds the original",
  read_text_file(folders$live_root, file.path(COPY_ASIDE_FOLDER, key_pz_2005_csv)) == "damaged 2005"
)

# ---- Things that must stop the run ----

cat("stops\n")

folders <- make_folders()
upload_plan <- make_plan(folders)
write_text_file(folders$live_root, key_pz_2023_csv, "changed by someone else")
stop_message <- error_message(run_upload(folders))

check(
  "a live file that is neither original nor replacement stops the run and is named",
  grepl("neither the original nor the replacement", stop_message) &&
    grepl(key_pz_2023_csv, stop_message, fixed = TRUE) &&
    !grepl(key_pz_2005_csv, stop_message, fixed = TRUE)
)

check(
  "when the run stops, no file has been replaced",
  read_text_file(folders$live_root, key_pz_2005_csv) == "damaged 2005" &&
    !file.exists(file.path(folders$live_root, key_pz_2005_parquet))
)

check(
  "when the run stops, the notice says in progress",
  grepl("IN PROGRESS", read_text_file(folders$live_root, NOTICE_KEY))
)

folders <- make_folders()
upload_plan <- make_plan(folders)

check(
  "files are not replaced before the originals are copied aside",
  grepl(
    "not been copied aside",
    error_message(replace_live_files(
      upload_plan, folders$replacement_root, folders$storage, folders$scratch_root
    ))
  ) &&
    read_text_file(folders$live_root, key_pz_2005_csv) == "damaged 2005"
)

folders <- make_folders()
upload_plan <- make_plan(folders)
write_text_file(folders$originals_root, key_pz_2005_csv, "local copy was changed")
changed_copy_message <- error_message(run_upload(folders))

check(
  "a local original that no longer matches the plan stops the copy-aside",
  grepl("does not match the upload plan", changed_copy_message) &&
    grepl(key_pz_2005_csv, changed_copy_message, fixed = TRUE)
)

check(
  "a local original that has gone missing stops the copy-aside",
  {
    missing_folders <- make_folders()
    make_plan(missing_folders)
    unlink(file.path(missing_folders$originals_root, key_pz_2005_csv))
    grepl("does not match the upload plan", error_message(run_upload(missing_folders)))
  }
)

check(
  "and the copy-aside is then not marked finished and nothing is replaced",
  !file.exists(file.path(folders$live_root, COPY_ASIDE_MANIFEST_KEY)) &&
    read_text_file(folders$live_root, key_pz_2005_csv) == "damaged 2005"
)

folders <- make_folders()
upload_plan <- make_plan(folders)
copy_originals_aside(upload_plan, folders$originals_root, folders$storage, folders$scratch_root)
write_text_file(folders$live_root, file.path(COPY_ASIDE_FOLDER, key_pz_2005_csv), "copy was changed")
wrong_copy_message <- error_message(run_upload(folders))

check(
  "a copy-aside file that differs from the original stops the run and is named",
  grepl("does not hold the planned original", wrong_copy_message) &&
    grepl(key_pz_2005_csv, wrong_copy_message, fixed = TRUE) &&
    !grepl(key_pz_2023_csv, wrong_copy_message, fixed = TRUE)
)

check(
  "and nothing is replaced and the wrong copy is not overwritten",
  read_text_file(folders$live_root, key_pz_2005_csv) == "damaged 2005" &&
    read_text_file(folders$live_root, file.path(COPY_ASIDE_FOLDER, key_pz_2005_csv)) == "copy was changed"
)

folders <- make_folders()
upload_plan <- make_plan(folders)

check(
  "copy_originals_aside says whether it copied or only verified",
  copy_originals_aside(upload_plan, folders$originals_root, folders$storage, folders$scratch_root) == "copied" &&
    copy_originals_aside(upload_plan, folders$originals_root, folders$storage, folders$scratch_root) == "verified"
)

unlink(file.path(folders$live_root, COPY_ASIDE_FOLDER, key_pz_2023_csv))

check(
  "a copy that has gone missing stops the run",
  grepl(
    key_pz_2023_csv,
    error_message(copy_originals_aside(
      upload_plan, folders$originals_root, folders$storage, folders$scratch_root
    )),
    fixed = TRUE
  )
)

# ---- Small helpers ----

cat("helpers\n")

check(
  "sha256 of a missing file is NA",
  is.na(sha256_of_file(file.path(tempdir(), "no_such_file")))
)

check(
  "sha256 of a file is the known value",
  {
    known_path <- write_text_file(tempdir(), "known.txt", "abc")
    sha256_of_file(known_path) == digest::digest(known_path, algo = "sha256", file = TRUE) &&
      nchar(sha256_of_file(known_path)) == 64
  }
)

check(
  "reading a stored file's sha256 leaves no download behind",
  {
    helper_folders <- make_folders()
    sha256_of_stored_file(key_base_file, helper_folders$storage, helper_folders$scratch_root)
    !file.exists(file.path(helper_folders$scratch_root, key_base_file))
  }
)

check(
  "a failed copy is an error",
  grepl("Could not copy", error_message(
    suppressWarnings(copy_file_creating_folder(file.path(tempdir(), "no_such_file"), tempfile()))
  ))
)

check(
  "the notice before the first replacement says in progress",
  grepl("IN PROGRESS", build_notice_text(upload_plan, is_complete = FALSE)) &&
    !grepl("REPLACEMENT COMPLETE", build_notice_text(upload_plan, is_complete = FALSE))
)

# The programs "true" and "false" stand in for the AWS CLI: one always
# succeeds and one always fails.
check(
  "a command that succeeds gives status 0",
  run_aws(c("s3", "ls"), command = "true") == 0L
)

check(
  "a command that fails stops with the command in the message",
  grepl("AWS command failed: aws s3 ls", error_message(run_aws(c("s3", "ls"), command = "false")))
)

check(
  "a command that fails gives its status when told not to stop",
  run_aws(c("s3", "ls"), stop_on_failure = FALSE, command = "false") != 0L
)

# ---- Standalone summary ----

if (!exists("TEST_RUN_ALL")) {
  cat(sprintf("\n=== %d passed, %d failed ===\n", PASS, FAIL))
  if (!interactive() && FAIL > 0L) quit(status = 1L)
}
