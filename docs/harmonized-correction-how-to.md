# How to run the harmonized CORE correction

Run five steps in order, on the EC2 instance that holds the build: build the yearly files, build the all-years files and dictionaries, write the upload plan, check it, then upload. The first four steps change nothing on S3. The fifth replaces the damaged files under `s3://nccsdata/harmonized/core/` and needs the maintainer's go-ahead first.

This correction runs once (ADR 0056 in `nccs-contracts`). After it, the folder is not updated again.

## Before you start

- You are in the repository root on the EC2 instance, on the `main` branch.
- `mirror/processed/core/` and `mirror/processed_legacy/core/` hold local copies of the current CORE files. The pre-2012 copy must be the one rebuilt under ADR 0057, with one row per return.
- The corrected pre-2012 CORE files are already published under `processed_legacy/core/` (ADR 0057). The harmonized upload comes after that one.
- No R script from this guide is running. Never edit a script while `Rscript` runs it.

In the commands below, `out` is the replacement folder: a local folder laid out like `harmonized/core/`. `work` is a local folder for the upload plan and the downloaded originals.

## Steps

1. Build and check the yearly files.

   ```
   Rscript R/run_harmonized_rewrite.R 1989 2023 mirror/processed/core mirror/processed_legacy/core out
   ```

   Expected result: one line per year ending in `passed`, and one file `out/_build_{year}.json` per year. If a year prints `FAILED`, stop. The lines under it say which check failed.

2. Build and check the all-years files, the dictionaries and the name table.

   ```
   Rscript R/12_harmonized_all_years.R out
   ```

   Expected result: the last line is `All-years files, dictionaries and name table: passed`, and `out/_build_all_years.json` lists 9 files. On an `r6i.4xlarge` instance this takes about 8 minutes. If the step stops with "hold values of different types in different years", do not work around it. The message names the column; decide its type in `nccs-contracts` first.

3. Write the upload plan. This downloads every file that will be replaced.

   ```
   Rscript R/11_harmonized_upload.R plan out work
   ```

   Expected result: a table of file counts that adds up to 197, and the file `work/upload_plan.json`. The step refuses to run if a file is absent, if a build check failed, or if a file changed after it was checked. The plan is written once. If you must start over before any upload, remove `work/upload_plan.json` by hand.

4. Check what the upload would do.

   ```
   Rscript R/11_harmonized_upload.R check out work
   ```

   Expected result: every file has the action `replace`. If any file has the action `stop`, the file on S3 is neither the original nor the replacement. Do not upload; find out who changed it.

5. Ask the maintainer for the go-ahead. Then upload.

   ```
   Rscript R/11_harmonized_upload.R upload out work
   ```

   Expected result: the originals are in `harmonized/core/_superseded-2025-04/`, `harmonized/core/README.txt` says `STATUS: REPLACEMENT COMPLETE`, and step 4 run again shows `already_replaced` for every file.

## If the upload is interrupted

Run step 5 again. It skips the files it already replaced and does not write to the copy-aside folder a second time. While the upload is unfinished, `README.txt` says `STATUS: REPLACEMENT IN PROGRESS`.
