# Script to test private foundation harmonization

library(data.table)
library(tidyverse)
library(duckdb)
library(assertr)
library(log4r)

source("R/format_ein.R")

my_logger <- create_logger("data/logs/pf_hrmn_log.txt")

# 990 PF crosswalk with old and new variable names, excluding variables already present in the BMF
xwalk_pf <- rio::import("data/crosswalks/XWALK-990PF-V0.xlsx") |>
  dplyr::filter(is.na(FOUND_IN_BMF)) |>
  dplyr::select(VAR_NAME_OLD,
                VAR_NAME_NEW)

# Iterate across all pf datasets

# Get all PF Dataset list
core_pf_legacy_ls <- list.files("data/raw/core_pf", full.names = TRUE)
soi_pf_legacy_ls <- list.files("data/raw/soi_pf", full.names = TRUE)

log_df <- data.frame()

harmonize_pf <- function(path, xwalk_pf, logger){
  year <- stringr::str_extract(path, "(19|20)\\d{2}")
  
  
  # Read file into data.table object
  pf_legacy_dt <- data.table::fread(path)
  setNames(pf_legacy_dt, toupper(names(pf_legacy_dt)))
  # Get column names
  legacy_colnames <- names(pf_legacy_dt)
  # Create xwalk sample
  xwalk_sample <- xwalk_pf |>
    dplyr::filter(VAR_NAME_OLD %in% legacy_colnames,
                  ! is.na(VAR_NAME_NEW),
                  VAR_NAME_NEW != "")
  # Get columnn names that are absent
  unharmonized_cols <- setdiff(legacy_colnames, xwalk_sample$VAR_NAME_OLD)
  # Perform harmonization
  harmonizable_cols <- unique(xwalk_sample$VAR_NAME_OLD)
  pf_legacy_sample <- pf_legacy_dt[, ..harmonizable_cols]
  data.table::setnames(pf_legacy_sample,
                       xwalk_sample$VAR_NAME_OLD,
                       xwalk_sample$VAR_NAME_NEW
  )
  # Duplicated column names - need to log
  dup_columns <- names(pf_legacy_sample)[duplicated(names(pf_legacy_sample))]
  # Create tax year column and stop if not there
#  stopifnot("F9_00_TAX_PERIOD_END_DATE |TAX_PERIOD not found" = "F9_00_TAX_PERIOD_END_DATE" %in% names(pf_legacy_sample | "TAX_PERIOD" %in% names(pf_legacy_sample)))
#  pf_legacy_sample |> assertr::verify(has_all_names("F9_00_TAX_PERIOD_END_DATE"))
  if ("F9_00_TAX_PERIOD_END_DATE" %in% harmonizable_cols){
    pf_legacy_sample[, TAX_YEAR := substr(F9_00_TAX_PERIOD_END_DATE, 1, 4)]
  } else {
    pf_legacy_sample[, TAX_YEAR := substr(TAX_PERIOD, 1, 4)]
  }
  # Create EIN2 Column with check
  pf_legacy_sample[, EIN2 := format_ein(F9_00_ORG_EIN, to="id")]
  # stopifnot("Not all EINs have 14 characters" = unique(nchar(pf_legacy_sample$EIN2)) != 14)
  # Log outputs
  log4r::info(logger, message = paste("Year", year))
#  log4r::info(logger, message = paste("Unharmonized cols:", paste0(unharmonized_cols, collapse = ", ")))
#  log4r::info(logger, message = paste("Harmonized cols:", paste0(harmonizable_cols, collapse = ", ")))
  log4r::info(logger, message = paste("Duplicated cols:", paste0(dup_columns, collapse = ", ")))
  
  return(pf_legacy_sample)
}

harmonize_pf(core_pf_legacy_ls[[10]], xwalk_pf, my_logger)

rs <- purrr::map(.x = core_pf_legacy_ls,
           .f = harmonize_pf,
           xwalk_pf = xwalk_pf,
           logger = my_logger,
           .progress = TRUE)


soi_rs <- purrr::map(.x = soi_pf_legacy_ls,
                 .f = harmonize_pf,
                 xwalk_pf = xwalk_pf,
                 logger = my_logger,
                 .progress = TRUE)

# TODO: RENAME DUPLICATES, EXTRACT YEAR FOR PF FILES


core_pf_legacy_ls[[10]]

xwalk_pf |> dplyr::filter(VAR_NAME_OLD == "P6EXMPF")


# Add step to log the unharmonized columns in a separate table
  # Name, Fiscal Year, URL to relevant form 990
# Add step to log the names and existing data types of the columns in the data

# harmonization




# Create tax year and put it all into a duckdb database

# Create EIN2 column
# check for 14 characters 
table()

# Put into a DuckDB database
con <- dbConnect(duckdb::duckdb(), dbdir = "data/pf_hrmn.db")

duckdb::dbWriteTable(con, "pf_hrmn", pf_legacy_19_sample, overwrite = TRUE)

# Get schema and data types
schema <- dbGetQuery(con, "PRAGMA table_info(pf_hrmn)")

# Get tax years in table
tax_years <- dbGetQuery(con,
           "SELECT DISTINCT TAX_YEAR FROM pf_hrmn ORDER BY TAX_YEAR DESC")
tax_years

# write to a parquet file

# Select a specific tax year and deduplicated data
pf_2020 <- dbGetQuery(con,
           "SELECT DISTINCT * FROM pf_hrmn WHERE TAX_YEAR = '2020'")

# remove columns with all NA
columns <- names(pf_2020)
column_stats <- dbGetQuery(con, paste0(
  "SELECT ", 
  paste(sprintf("COUNT(%s) AS %s", columns, columns), collapse = ", "),
  " FROM pf_hrmn"
)) |>
  tidyr::pivot_longer(cols = everything(),
                      names_to = "VAR_NAME_NEW",
                      values_to = "NUM_OBS_NOT_NA")

na_cols <- column_stats |>
  dplyr::filter(NUM_OBS_NOT_NA == 0) |>
  dplyr::pull(VAR_NAME_NEW)

# Drop columns with all NA

if (length(na_cols) > 0) {
  for (col in na_cols){
    dbExecute(con, paste0("ALTER TABLE pf_hrmn DROP COLUMN ", col))
  }
}

# Write the results out

dbExecute(con, "COPY pf_hrmn TO 'data/pf_test.csv' (HEADER, DELIMITER ',', QUOTE '\"')")

# save metadata
# number of rows, colu

# end steps
# deduplication

# SCHEMA

# DATASET = 3 COLUMNS {var_name: INT, year: INT, url: STR}
# SQL = CREATE TABLE missing_vars (var_name INT, year INT, url STR)

# Rename columns to match crosswalk

var_rename_df <- xwalk_pf |>
  dplyr::mutate(var_old = toupper(var_old),
                var_new = toupper(var_new)) |>
  filter(var_old %in% toupper(names(pf_legacy_19)),
         var_new != "IRS_SUBSECTOR_CODE",
         var_new != "")

data.table::setnames(pf_legacy_19, 
                     var_rename_df$var_old, 
                     var_rename_df$var_new)

missing_vars <- setdiff(names(pf_legacy_19), var_rename_df$var_new)

pf_legacy_19[, (missing_vars) := NULL]

# tax year
names <- c("column_a", "column_a")
newnames <- make.unique(names(pf_legacy_19))
names(pf_legacy_19) <- newnames

# we need to save other tax years separately
pf_hrmn_19 <- pf_legacy_19 |>
  dplyr::mutate(TAX_YEAR_END = as.character(TAX_YEAR_END)) |>
  dplyr::filter(TAX_YEAR_END == "2019")

table(pf_hrmn_19$TAX_YEAR_END)

# ein - format ein function copy/paste - to create EIN2
pf_hrmn_19$F9_00_ORG_EIN[1:10]

# data types?
column_types <- sapply(pf_hrmn_19, class)

# log what's missing



# post edit of what's missing

data.table::fwrite(pf_hrmn_19, 
                   "data/processed/CORE-2019-501C3-PF-HRMN_V0.csv")


# data type - SQL Database
  # save to .csv on website
  # create SQL database - .parqut, 
  # Preferred SQL database - Postgres, MYSQL
      # SQLite - not recommended for large datasets
      # DUCKDB
# data quality assessments



test_df