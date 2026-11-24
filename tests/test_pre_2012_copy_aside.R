# tests/test_pre_2012_copy_aside.R
# Tests for R/13_pre_2012_copy_aside.R (ADR 0057 Decision 5): which files
# are copied aside, the copy-aside plan, a first run, a second run, and
# the cases that must stop. A local folder stands in for S3.
# Run from repo root:  Rscript tests/test_pre_2012_copy_aside.R
#
# "Live folder", "year folder", "original" and "copy-aside folder" are
# defined at the top of R/13_pre_2012_copy_aside.R.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "08_upload.R"))
source(here("R", "13_pre_2012_copy_aside.R"))

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

# ---- A small live folder: two year folders to copy, one to leave ----

write_text_file <- function(root, key, text) {
  path <- file.path(root, key)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path)
  return(invisible(path))
}

read_text_file <- function(root, key) {
  return(paste(readLines(file.path(root, key)), collapse = "\n"))
}

key_2010_csv <- "2010/990combined/core_2010_990combined.csv"
key_2011_csv <- "2011/990combined/core_2011_990combined.csv"
key_2011_pf <- "2011/990pf/core_2011_990pf.parquet"
key_2012_csv <- "2012/990combined/core_2012_990combined.csv"

small_years <- 2010:2011

small_description <- "Small files for the test."

make_folders <- function() {
  top <- tempfile("pre_2012_copy_aside_")
  live_root <- file.path(top, "live")
  work_root <- file.path(top, "work")
  dir.create(work_root, recursive = TRUE)

  write_text_file(live_root, key_2010_csv, "original 2010")
  write_text_file(live_root, key_2011_csv, "original 2011")
  write_text_file(live_root, key_2011_pf, "original 2011 private foundations")
  write_text_file(live_root, key_2012_csv, "a year that does not change")

  folders <- list(
    live_root = live_root,
    work_root = work_root,
    originals_root = file.path(work_root, "originals"),
    plan_path = file.path(work_root, "copy_aside_plan.json"),
    storage = folder_storage(live_root)
  )

  return(folders)
}

make_plan <- function(folders) {
  return(write_copy_aside_plan(folders$storage, folders$originals_root, folders$plan_path, years = small_years))
}

copy_key <- function(key) {
  return(file.path(PRE_2012_COPY_ASIDE_FOLDER, key))
}

# ---- The constants ----

check(
  "the two live folders are the pre-2012 CORE and the merged panel",
  identical(
    PRE_2012_LIVE_S3_URIS,
    c(
      processed_legacy = "s3://nccsdata/processed_legacy/core",
      processed_merged = "s3://nccsdata/processed_merged/core"
    )
  )
)

check("the copy-aside folder is _superseded-2026-11", PRE_2012_COPY_ASIDE_FOLDER == "_superseded-2026-11")

check("the year folders run from 1987 to 2011", identical(PRE_2012_YEARS, 1987:2011))

check(
  "each live folder has its own description for the copy-aside folder",
  setequal(names(PRE_2012_COPY_ASIDE_DESCRIPTIONS), names(PRE_2012_LIVE_S3_URIS))
)

# ---- The copy-aside plan ----

folders <- make_folders()

small_plan <- make_plan(folders)

check(
  "the plan lists every file in the year folders asked for, and no other year",
  setequal(small_plan$key, c(key_2010_csv, key_2011_csv, key_2011_pf))
)

check(
  "the plan records the sha256 and size of each original",
  {
    planned <- small_plan[small_plan$key == key_2010_csv, ]
    live_path <- file.path(folders$live_root, key_2010_csv)
    planned$original_sha256 == sha256_of_file(live_path) && planned$original_bytes == file.size(live_path)
  }
)

check(
  "the plan step keeps a local copy of each original",
  read_text_file(folders$originals_root, key_2011_pf) == "original 2011 private foundations"
)

check(
  "the plan step writes nothing to the live folder",
  !file.exists(file.path(folders$live_root, PRE_2012_COPY_ASIDE_FOLDER))
)

check(
  "the plan read back from disk is the plan that was written",
  isTRUE(all.equal(as.data.frame(read_copy_aside_plan(folders$plan_path)), as.data.frame(small_plan)))
)

check(
  "a second plan is refused, because the plan is never rewritten",
  grepl("never rewritten", error_message(make_plan(folders)))
)

check(
  "a year folder with no file stops the plan and is named",
  {
    empty_year_folders <- make_folders()
    empty_message <- error_message(write_copy_aside_plan(
      empty_year_folders$storage, empty_year_folders$originals_root, empty_year_folders$plan_path,
      years = 2009:2011
    ))
    grepl("No file found in year folder", empty_message) &&
      grepl("2009", empty_message) &&
      !file.exists(empty_year_folders$plan_path)
  }
)

check(
  "copying without a plan is refused",
  grepl(
    "Run the plan step first",
    error_message(run_pre_2012_copy_aside(make_folders()$work_root, folders$storage, small_description))
  )
)

# ---- A first run ----

first_result <- run_pre_2012_copy_aside(folders$work_root, folders$storage, small_description)

check("a first run says it copied", first_result == "copied")

check(
  "every original is in the copy-aside folder under the same key",
  read_text_file(folders$live_root, copy_key(key_2010_csv)) == "original 2010" &&
    read_text_file(folders$live_root, copy_key(key_2011_csv)) == "original 2011" &&
    read_text_file(folders$live_root, copy_key(key_2011_pf)) == "original 2011 private foundations"
)

check(
  "a year that does not change is not copied",
  !file.exists(file.path(folders$live_root, copy_key(key_2012_csv)))
)

check(
  "the originals are still in their year folders, unchanged",
  read_text_file(folders$live_root, key_2010_csv) == "original 2010" &&
    read_text_file(folders$live_root, key_2012_csv) == "a year that does not change"
)

copy_aside_manifest <- jsonlite::fromJSON(file.path(folders$live_root, copy_key("_manifest.json")))

check(
  "the small file in the copy-aside folder lists every copy with its sha256 and says what the folder holds",
  copy_aside_manifest$object_count == 3 &&
    setequal(copy_aside_manifest$files$file, small_plan$key) &&
    setequal(copy_aside_manifest$files$sha256, small_plan$original_sha256) &&
    copy_aside_manifest$what == small_description
)

check(
  "the harmonized CORE copy-aside folder is not written to",
  !file.exists(file.path(folders$live_root, COPY_ASIDE_FOLDER))
)

# ---- Running again, after the corrected files were uploaded ----

write_text_file(folders$live_root, key_2010_csv, "corrected 2010")

manifest_before <- read_text_file(folders$live_root, copy_key("_manifest.json"))

second_result <- run_pre_2012_copy_aside(folders$work_root, folders$storage, small_description)

check("a second run says it only verified", second_result == "verified")

check(
  "a second run does not copy the corrected file over the original's copy",
  read_text_file(folders$live_root, copy_key(key_2010_csv)) == "original 2010"
)

check(
  "a second run does not rewrite the small file",
  read_text_file(folders$live_root, copy_key("_manifest.json")) == manifest_before
)

# ---- Things that must stop the run ----

check(
  "a copy that was changed stops a later run and is named",
  {
    write_text_file(folders$live_root, copy_key(key_2011_csv), "copy was changed")
    changed_message <- error_message(run_pre_2012_copy_aside(folders$work_root, folders$storage, small_description))
    grepl("does not hold the planned original", changed_message) &&
      grepl(key_2011_csv, changed_message, fixed = TRUE)
  }
)

check(
  "a local original that changed after the plan stops the run before anything is copied",
  {
    changed_folders <- make_folders()
    make_plan(changed_folders)
    write_text_file(changed_folders$originals_root, key_2010_csv, "changed on disk")
    changed_message <- error_message(
      run_pre_2012_copy_aside(changed_folders$work_root, changed_folders$storage, small_description)
    )
    grepl("does not match the upload plan", changed_message) &&
      !file.exists(file.path(changed_folders$live_root, copy_key("_manifest.json")))
  }
)

# ---- The upload refuses to run before the copy ----

check(
  "the check passes for a live folder whose copy-aside is finished",
  isTRUE(stop_unless_copied_aside(folders$storage))
)

check(
  "the check stops for a live folder with no finished copy-aside, and says what to run",
  {
    not_copied_message <- error_message(stop_unless_copied_aside(make_folders()$storage))
    grepl("have not been copied aside", not_copied_message) &&
      grepl("R/13_pre_2012_copy_aside.R", not_copied_message, fixed = TRUE)
  }
)

check(
  "a plan alone is not a finished copy-aside",
  {
    planned_folders <- make_folders()
    make_plan(planned_folders)
    grepl("have not been copied aside", error_message(stop_unless_copied_aside(planned_folders$storage)))
  }
)

check(
  "the pre-2012 CORE upload stops before sending anything when the copy-aside is not finished",
  grepl(
    "have not been copied aside",
    error_message(run_upload_legacy(enable_upload = TRUE, live_storage = make_folders()$storage))
  )
)

check(
  "the merged panel upload stops before sending anything when the copy-aside is not finished",
  grepl(
    "have not been copied aside",
    error_message(run_upload_merged(enable_upload = TRUE, live_storage = make_folders()$storage))
  )
)

check(
  "a trial run of the upload, which sends nothing, does not need the copy-aside",
  !grepl(
    "have not been copied aside",
    paste(error_message(stop_unless_copied_aside(make_folders()$storage, dry_run = TRUE)))
  )
)

check(
  "the two live folders are the folders the pipeline uploads to",
  identical(
    paste0(unname(PRE_2012_LIVE_S3_URIS), "/"),
    c(s3_uri(S3$processed_legacy_prefix), s3_uri(S3$processed_merged_prefix))
  )
)

# ---- Listing a folder ----

check(
  "a local storage lists the keys under one folder, with the folder in each key",
  setequal(folders$storage$list_keys("2011"), c(key_2011_csv, key_2011_pf))
)

check(
  "a folder that does not exist lists no keys",
  length(folders$storage$list_keys("1900")) == 0
)

#' Write a stand-in for the aws program that prints fixed text and exits
write_fake_aws <- function(printed_text, exit_status) {
  script_path <- tempfile("fake_aws_")
  writeLines(c("#!/bin/sh", sprintf("printf '%s'", printed_text), sprintf("exit %d", exit_status)), script_path)
  Sys.chmod(script_path, "755")
  return(script_path)
}

check(
  "an S3 listing is turned into keys relative to the live folder",
  {
    fake_aws <- write_fake_aws(
      "processed_legacy/core/2005/990pf/core_2005_990pf.csv\\tprocessed_legacy/core/2005/990pf/core_2005_990pf.parquet\\n", 0
    )
    identical(
      list_s3_keys("s3://nccsdata/processed_legacy/core", "2005", command = fake_aws),
      c("2005/990pf/core_2005_990pf.csv", "2005/990pf/core_2005_990pf.parquet")
    )
  }
)

check(
  "an empty S3 folder, which the listing prints as None, gives no keys",
  length(list_s3_keys("s3://nccsdata/processed_legacy/core", "1900", command = write_fake_aws("None\\n", 0))) == 0
)

check(
  "an S3 listing that fails stops with an error and is not read as an empty folder",
  grepl(
    "Could not list the S3 folder",
    error_message(list_s3_keys("s3://nccsdata/processed_legacy/core", "2005", command = write_fake_aws("ExpiredToken", 255)))
  )
)

# ---- Standalone summary ----

if (!exists("TEST_RUN_ALL")) {
  cat(sprintf("\n=== %d passed, %d failed ===\n", PASS, FAIL))
  if (!interactive() && FAIL > 0L) quit(status = 1L)
}
