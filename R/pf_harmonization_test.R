# Script to test private foundation harmonization

library(data.table)
library(tidyverse)
library(duckdb)
library(assertr)
library(log4r)

source("R/format_ein.R")
source("R/data-dictionary_helpers.R")

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

harmonize_pf <- function(path, xwalk_pf, logger){
  
  # Extract the calendar year for the legacy core/soi dataset
  year <- stringr::str_extract(path, "(19|20)\\d{2}")
  if (is.na(year)){year <- paste0("20", stringr::str_extract(path, "\\d{2}"),
                                  collapse = "")}
  
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
  
  # Create column containing tax year
  if ("F9_00_FISCAL_YEAR_END" %in% harmonizable_cols){
    pf_legacy_sample[, TAX_YEAR := substr(F9_00_FISCAL_YEAR_END, 1, 4)]
  } else {
    pf_legacy_sample[, TAX_YEAR := substr(F9_00_TAX_PERIOD_END_DATE, 1, 4)]
  }
  
  # Create EIN2 Column with check
  pf_legacy_sample[, EIN2 := format_ein(F9_00_ORG_EIN, to="id")]
  
  # Log outputs
  log4r::info(logger, message = paste("Year", year))
  log4r::info(logger, message = paste("Unharmonized cols:", paste0(unharmonized_cols, collapse = ", ")))
  log4r::info(logger, message = paste("Harmonized cols:", paste0(harmonizable_cols, collapse = ", ")))
  log4r::info(logger, message = paste("Duplicated cols:", paste0(dup_columns, collapse = ", ")))
  
  return(pf_legacy_sample)
}

pf_ls <- c(core_pf_legacy_ls, soi_pf_legacy_ls)
pf_hrmn_ls <- purrr::map(
  .x = pf_ls,
  .f = harmonize_pf,
  xwalk_pf = xwalk_pf,
  logger = my_logger,
  .progress = TRUE
)

# Put all datasets into a DuckDB database
pf_hrmn_dt <- data.table::rbindlist(pf_hrmn_ls, fill = TRUE)
con <- dbConnect(duckdb::duckdb(), dbdir = "data/pf_hrmn.db")
duckdb::dbWriteTable(con,
                     "pf_hrmn",
                     pf_hrmn_dt,
                     overwrite = TRUE)

# Get schema and data types
schema <- dbGetQuery(con, "PRAGMA table_info(pf_hrmn)")

# Export to parquet

# Add in metadata
harmonized_vars <- schema$name
## unharmonized columns
## years for each variable
coverages <- c()
for (var in harmonized_vars){
  years <- dbGetQuery(con,
                    sprintf("SELECT TAX_YEAR FROM pf_hrmn 
                    WHERE %s IS NOT NULL 
                    GROUP BY TAX_YEAR 
                    ORDER BY TAX_YEAR DESC", var)) |>
    unlist()
  if (length(years) != 0){
    coverage <- format_years_range(years)
  } else {
    coverage <- "None"
  }
  coverages <- c(coverages, coverage)
}
schema$coverage <- coverages
## percentage of NA values
na_percents <- c()
record_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM pf_hrmn")
for (var in harmonized_vars){
  na_count <- dbGetQuery(con,
                      sprintf("SELECT COUNT(*) as null_count FROM pf_hrmn WHERE %s IS NULL", var)) |>
    unlist()
  na_percent <- na_count / record_count * 100
  na_percents <- c(na_percents, na_percent)
}

schema$na_percent <- unlist(na_percents)

# Cardinality
cardinality <- c()
for (var in harmonized_vars){
  cardinality_percent <- dbGetQuery(con, sprintf(
    "SELECT COUNT(DISTINCT %s) * 100.0 / COUNT(%s) AS cardinality_ratio
     FROM pf_hrmn",
    var,
    var
  )) |> unlist()
  cardinality <- c(cardinality, cardinality_percent)
}
schema$cardinality_percent <- cardinality

# Save schema
data.table::fwrite(schema, "data/schemas/PF-HRMN-SCHEMA-V0.csv")

# Deduplicate
dbExecute(con, "
  CREATE OR REPLACE TABLE pf_hrmn AS
  SELECT DISTINCT *
  FROM pf_hrmn
")


# write to a parquet file
dbExecute(con, "COPY pf_hrmn TO 'data/processed/pf/full/CORE-FULL-501C3-PRIVFOUND-PF-HRMN-V0.parquet' (FORMAT PARQUET)")

# Select a specific tax year and deduplicated data

# Get tax years in table
tax_years <- dbGetQuery(con,
                        "SELECT DISTINCT TAX_YEAR FROM pf_hrmn ORDER BY TAX_YEAR DESC")

# Save individual data marts

for (year in tax_years$TAX_YEAR){
  message(paste("Processing", year))
  filename <- sprintf("CORE-%s-501C3-PRIVFOUND-PF-HRMN-V0.csv", year)
  filepath <- paste0("data/processed/pf/full/", filename, collapse = "")
  pf <- dbGetQuery(con,
                   sprintf("SELECT DISTINCT * FROM pf_hrmn 
                            WHERE TAX_YEAR = '%s'", year))
  data.table::fwrite(pf, filepath)
}

# Create data dictionary
concordance <- rio::import("data/concordance/CONCORDANCE-PF-V0.xlsx")
concordance <- concordance |>
  dplyr::select(
    variable_name,
    label,
    description,
    form,
    form_type,
    form_part,
    form_line_number
  ) |>
  dplyr::rename_with(
    toupper
  ) |>
  dplyr::rename(
    VAR_NAME_NEW = VARIABLE_NAME
  )

dd <- schema |>
  dplyr::select(
    name,
    type,
    na_percent,
    coverage,
    cardinality_percent
  ) |>
  dplyr::rename(
    VAR_NAME_NEW = name,
    VAR_DATA_TYPE = type,
    VAR_NA_PERCENT = na_percent,
    VAR_COVERAGE = coverage,
    VAR_CARDINALITY_PERCENT = cardinality_percent
  ) |>
  tidylog::left_join(
    concordance
  )

data.table::fwrite(dd, "data/dd/DD-PF-HRMN-V0.csv")

View(dd)
