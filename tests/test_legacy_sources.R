# tests/test_legacy_sources.R
# Tests for which NCCS source files the pre-2012 CORE is made from: only
# the files for 1989 to 2011, whatever else the source folder holds.
# Run from repo root:  Rscript tests/test_legacy_sources.R
#
# "NCCS source file" means one yearly file of the older NCCS CORE, for
# example CORE-2005-501C3-CHARITIES-PZ.csv. The year in its name is the
# "source file year".

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "03_legacy_harmonize.R"))
source(here("R", "01_legacy_download.R"))

if (!exists("PASS")) PASS <- 0L
if (!exists("FAIL")) FAIL <- 0L

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) { PASS <<- PASS + 1L; cat("  ok  ", label, "\n") }
  else    { FAIL <<- FAIL + 1L; cat("  FAIL", label, "\n") }
}

# ---- A source folder that holds more than it should ----

source_folder <- tempfile("legacy_sources_")
dir.create(source_folder)

file_names_in_folder <- c(
  "CORE-1988-501C3-CHARITIES-PZ.csv",
  "CORE-1989-501C3-CHARITIES-PZ.csv",
  "CORE-2011-501C3-CHARITIES-PZ.csv",
  "CORE-2011-501CE-NONPROFIT-PZ.csv",
  "CORE-2011-501C3-PRIVFOUND-PF.csv",
  "CORE-2012-501C3-CHARITIES-PZ.csv",
  "CORE-2012-501C3-CHARITIES-PC.csv",
  "CORE-2013-501CE-NONPROFIT-PZ.csv",
  "CORE-2013-501C3-PRIVFOUND-PF.csv",
  "notes.txt"
)

file.create(file.path(source_folder, file_names_in_folder))

charity_sources <- list_legacy_sources("990combined", source_folder)

foundation_sources <- list_legacy_sources("990pf", source_folder)

# ---- The source file years ----

check("the source file years are 1989 to 2011", identical(LEGACY_SOURCE_FILE_YEARS, 1989:2011))

check(
  "the charity and nonprofit files of 1989 to 2011 are listed, and no other",
  identical(
    basename(charity_sources),
    c(
      "CORE-1989-501C3-CHARITIES-PZ.csv",
      "CORE-2011-501C3-CHARITIES-PZ.csv",
      "CORE-2011-501CE-NONPROFIT-PZ.csv"
    )
  )
)

check(
  "a 2012 or 2013 file in the folder is not listed",
  !any(grepl("CORE-201[23]-", c(charity_sources, foundation_sources)))
)

check(
  "a file from before 1989 is not listed",
  !any(grepl("CORE-1988-", charity_sources))
)

check(
  "the private foundation file of 2011 is listed, and the 2013 one is not",
  identical(basename(foundation_sources), "CORE-2011-501C3-PRIVFOUND-PF.csv")
)

check(
  "the files left out are reported, so a folder with extra files is seen",
  setequal(
    basename(attr(charity_sources, "left_out")),
    c("CORE-1988-501C3-CHARITIES-PZ.csv", "CORE-2012-501C3-CHARITIES-PZ.csv", "CORE-2013-501CE-NONPROFIT-PZ.csv")
  )
)

check(
  "a folder with only in-scope files reports nothing left out",
  {
    clean_folder <- tempfile("legacy_sources_clean_")
    dir.create(clean_folder)
    file.create(file.path(clean_folder, "CORE-2005-501C3-CHARITIES-PZ.csv"))
    length(attr(list_legacy_sources("990combined", clean_folder), "left_out")) == 0
  }
)

check(
  "the download step and the step that reads the files use the same years",
  identical(
    is_legacy_source_file_year(c(1988L, 1989L, 2011L, 2012L)),
    c(FALSE, TRUE, TRUE, FALSE)
  ) &&
    !exists(".LEGACY_YEAR_HI")
)

# ---- Standalone summary ----

if (!exists("TEST_RUN_ALL")) {
  cat(sprintf("\n=== %d passed, %d failed ===\n", PASS, FAIL))
  if (!interactive() && FAIL > 0L) quit(status = 1L)
}
