# Script to test private foundation harmonization

library(data.table)
library(tidyverse)
library(duckdb)

source("R/format_ein.R")

# 990 PF crosswalk with old and new variable names, excluding variables already present in the BMF
xwalk_pf <- rio::import("data/crosswalks/XWALK-990PF-V0.xlsx") |>
  dplyr::filter(is.na(FOUND_IN_BMF)) |>
  dplyr::select(VAR_NAME_OLD,
                VAR_NAME_NEW)

# Load in test PF dataset
pf_legacy_19 <- data.table::fread(
  "data/raw/core_pf/CORE-2019-501C3-PRIVFOUND-PF.csv"
)

legacy_names <- toupper(names(pf_legacy_19))

xwalk_sample <- xwalk_pf |>
  dplyr::filter(VAR_NAME_OLD %in% legacy_names,
                ! is.na(VAR_NAME_NEW),
                VAR_NAME_NEW != "")

unharmonized_cols <- setdiff(legacy_names, xwalk_sample$VAR_NAME_OLD)

# Add step to log the unharmonized columns in a separate table
  # Name, Fiscal Year, URL to relevant form 990
# Add step to log the names and existing data types of the columns in the data

# harmonization
harmonizable_cols <- unique(xwalk_sample$VAR_NAME_OLD)
pf_legacy_19_sample <- pf_legacy_19[, ..harmonizable_cols]

data.table::setnames(
  pf_legacy_19_sample,
  xwalk_sample$VAR_NAME_OLD,
  xwalk_sample$VAR_NAME_NEW
)

# Duplicated column names - need to log before running it all over again
dup_columns <- names(pf_legacy_19_sample)[duplicated(names(pf_legacy_19_sample))]

# Create tax year and put it all into a duckdb database
pf_legacy_19_sample[, TAX_YEAR := substr(F9_00_TAX_PERIOD_END_DATE, 1, 4)]

# Create EIN2 column
pf_legacy_19_sample[, EIN2 := format_ein(F9_00_ORG_EIN, to="id")]
# check for 14 characters 
table(nchar(pf_legacy_19_sample$EIN2))

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