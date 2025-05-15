# Script to update the core files


# Packages
library(log4r)
library(paws)
library(purrr)
library(stringr)
library(rio)
library(duckdb)

# Helper Scripts
source("R/utils.R")
source("R/format_ein.R")
source("R/data-dictionary_helpers.R")

# Logging
my_logger <- create_logger("data/logs/data_download_log.txt")

# Step 1: Save core files from s3 bucket to data/harmonized/core/

# Step 2: get list of soi urls

soi_url_ls <- get_s3_bucket_contents(
  bucket_name = "gt990datalake-rawdata",
  bucket_folder = "EfileData/Extracts/Data/",
  bucket_url = "https://gt990datalake-rawdata.s3.amazonaws.com/"
)

# Get url list to update

soi_updt_url_ls <- soi_url_ls[grepl("(22|23).*?(990\\.xlsx|ez\\.xlsx)$", soi_url_ls)]

# Download - need to debug

download_raw_data(url_ls = soi_updt_url_ls[3:4],
                  destfolder = "data/raw/soi/",
                  logger = my_logger)

# Step 3: Begin harmonization

# 3.1 read in crosswalk

xwalk_990ez <- data.table::fread("data/crosswalks/XWALK-990-V2.csv")


# 3.2 read in data

soi_ls <- list.files("data/raw/soi/", full.names = TRUE)

# Harmonize new core files

# function to rename

rename_soi_990ez <- function(soi_dt, xwalk_990ez, logger){
  
  # Set to upper case
  names(soi_dt) <- toupper(names(soi_dt))
  
  # Get column names
  legacy_colnames <- names(soi_dt)
  
  # Create xwalk sample
  xwalk_sample <- xwalk_990ez |>
    dplyr::filter(VAR_NAME_OLD %in% legacy_colnames,
                  VAR_NAME_NEW != "NOT IN CONCORDANCE",
                  VAR_NAME_NEW != "NOT IN DD") |>
    dplyr::distinct()

  # Get columnn names that are absent
  unharmonized_cols <- setdiff(legacy_colnames, xwalk_sample$VAR_NAME_OLD)
  
  # Perform harmonization
  harmonizable_cols <- unique(xwalk_sample$VAR_NAME_OLD)
  soi_sample <- soi_dt[, ..harmonizable_cols]
  data.table::setnames(soi_sample,
                       xwalk_sample$VAR_NAME_OLD,
                       xwalk_sample$VAR_NAME_NEW
  )
  
  # Duplicated column names - need to log
  dup_columns <- names(soi_sample)[duplicated(names(soi_sample))]
  
  # Create column containing tax year
  if ("F9_00_FISCAL_YEAR_END" %in% harmonizable_cols){
    soi_sample[, TAX_YEAR := substr(F9_00_FISCAL_YEAR_END, 1, 4)]
  } else {
    soi_sample[, TAX_YEAR := substr(F9_00_TAX_PERIOD_END_DATE, 1, 4)]
  }
  
  # Create EIN2 Column with check
  soi_sample[, EIN2 := format_ein(F9_00_ORG_EIN, to="id")]
  
  # Log outputs
  log4r::info(logger, message = paste("Unharmonized cols:", paste0(unharmonized_cols, collapse = ", ")))
  log4r::info(logger, message = paste("Harmonized cols:", paste0(harmonizable_cols, collapse = ", ")))
  log4r::info(logger, message = paste("Duplicated cols:", paste0(dup_columns, collapse = ", ")))
  
  return(soi_sample)
  
}

soi_ls <- purrr::map(soi_ls, data.table::fread)

soi_hrmn_ls <- purrr::map(
  soi_ls,
  rename_soi_990ez,
  xwalk_990ez,
  my_logger,
  .progress = TRUE
)


# C3 - PC Scope

c3_pc_old <- purrr::map(
  list.files("data/harmonized/core/501c3-pc/", full.names = TRUE)[1:11],
  data.table::fread
) |>
  data.table::rbindlist(fill = TRUE)

c3_pc_new_ls <- list(
  soi_hrmn_ls[[1]],
  soi_hrmn_ls[[3]]
)

c3_pc_new <- data.table::rbindlist(c3_pc_new_ls) |>
  dplyr::filter(
    F9_00_SUBSECTION_CODE == 3
  ) |>
  dplyr::distinct()

cols_to_exclude <- c(
  "F9_00_TAX_PERIOD_BEGIN_DATE",
  "F9_00_TAX_YEAR",
  "F9_03_PROG_CODE",
  "F9_03_PROG_CODE_2",
  "F9_03_PROG_CODE_3",
  "F9_08_REV_PROG_BIZCODE",
  "F9_08_REV_PROG_BIZCODE_2",
  "F9_08_REV_PROG_BIZCODE_3",
  "F9_08_REV_PROG_BIZCODE_4",
  "F9_08_REV_PROG_BIZCODE_5",
  "F9_09_EXP_OTH_OTH_TOT",
  "F9_10_ASSET_LAND_BLDG_DEPREC",
  "GEO_ZIP5",
  "MISSION_NTEE",
  "DUP_RTRN_X",
  "F9_00_GROUP_EXEMPT_NUM",
  "F9_00_RETURN_TYPE",
  "F9_00_ORG_ADDR_CITY",
  "F9_00_ORG_ADDR_L1",
  "F9_00_ORG_ADDR_STATE",
  "F9_00_ORG_ADDR_ZIP",
  "F9_00_ORG_NAME_DBA_L1",
  "F9_00_ORG_NAME_L1"
)

c3_pc_old <- c3_pc_old |>
  dplyr::select(!any_of(cols_to_exclude)) |>
  dplyr::rename(
    "F9_00_SUBSECTION_CODE" = BMF_SUBSECTION_CODE,
    "F9_08_REV_MISC_BIZCODE_A" = F9_08_REV_MISC_BIZCODE,
    "F9_08_REV_MISC_BIZCODE_B" = F9_08_REV_MISC_BIZCODE_2,
    "F9_08_REV_MISC_BIZCODE_C" = F9_08_REV_MISC_BIZCODE_3,
    "F9_08_REV_MISC_TOT_A" = F9_08_REV_MISC_TOT,
    "F9_08_REV_MISC_TOT_B" = F9_08_REV_MISC_TOT_2,
    "F9_08_REV_MISC_TOT_C" = F9_08_REV_MISC_TOT_3,
    "F9_08_REV_PROG_DESC_A" = F9_08_REV_PROG_DESC,
    "F9_08_REV_PROG_DESC_B" = F9_08_REV_PROG_DESC_2,
    "F9_08_REV_PROG_DESC_C" = F9_08_REV_PROG_DESC_3,
    "F9_08_REV_PROG_DESC_D" = F9_08_REV_PROG_DESC_4,
    "F9_08_REV_PROG_DESC_E" = F9_08_REV_PROG_DESC_5,
    "F9_08_REV_PROG_TOT_A" = F9_08_REV_PROG_TOT,
    "F9_08_REV_PROG_TOT_B" = F9_08_REV_PROG_TOT_2,
    "F9_08_REV_PROG_TOT_C" = F9_08_REV_PROG_TOT_3,
    "F9_08_REV_PROG_TOT_D" = F9_08_REV_PROG_TOT_4,
    "F9_08_REV_PROG_TOT_E" = F9_08_REV_PROG_TOT_5,
    "F9_09_EXP_OTH_TOT_A" = F9_09_EXP_OTH_TOT,
    "F9_09_EXP_OTH_TOT_B" = F9_09_EXP_OTH_TOT_2,
    "F9_09_EXP_OTH_TOT_C" = F9_09_EXP_OTH_TOT_3,
    "F9_09_EXP_OTH_TOT_D" = F9_09_EXP_OTH_TOT_4,
    "F9_09_EXP_OTH_TOT_E" = F9_09_EXP_OTH_TOT_5,
    "F9_09_EXP_OTH_TOT_F" = F9_09_EXP_OTH_TOT_6
  )

c3_pc_full <- list(c3_pc_old, c3_pc_new)

c3_pc_full <- data.table::rbindlist(c3_pc_full, fill = TRUE) |>
  dplyr::distinct()

# Import into duckdb

con <- dbConnect(duckdb(), dbdir = "my_database.duckdb")
dbWriteTable(con, "c3_pc", c3_pc_full, overwrite = TRUE)

# Get tax years
tax_years <- dbGetQuery(con,
                        "SELECT DISTINCT TAX_YEAR FROM c3_pc ORDER BY TAX_YEAR DESC")
tax_years <- tax_years |>
  dplyr::filter(
    TAX_YEAR >= 2012
  )
# Get schema and data types
schema <- dbGetQuery(con, "PRAGMA table_info(c3_pc)")

# Add in metadata
harmonized_vars <- schema$name
## unharmonized columns
## years for each variable
coverages <- c()
for (var in harmonized_vars){
  years <- dbGetQuery(con,
                      sprintf("SELECT TAX_YEAR FROM c3_pc 
                    WHERE %s IS NOT NULL AND CAST(TAX_YEAR AS INTEGER) >= 2012
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
record_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM c3_pc")
for (var in harmonized_vars){
  na_count <- dbGetQuery(con,
                         sprintf("SELECT COUNT(*) as null_count FROM c3_pc 
                                 WHERE %s IS NULL AND CAST(TAX_YEAR AS INTEGER) >= 2012", 
                                 var)) |>
    unlist()
  na_percent <- na_count / record_count * 100
  na_percents <- c(na_percents, na_percent)
}

schema$na_percent <- unlist(na_percents)

# Cardinality
cardinality <- c()
for (var in harmonized_vars){
  cardinality_percent <- dbGetQuery(con, sprintf(
    "SELECT COUNT(DISTINCT %s) * 100.0 / COUNT(%s) AS cardinality_ratio FROM c3_pc
     WHERE CAST(TAX_YEAR AS INTEGER) >= 2012",
    var,
    var
  )) |> unlist()
  cardinality <- c(cardinality, cardinality_percent)
}
schema$cardinality_percent <- cardinality

# Save schema

data.table::fwrite(schema, "data/schemas/C3_PC-HRMN-SCHEMA-V1.csv")

# Deduplicate
dbExecute(con, "
  CREATE OR REPLACE TABLE c3_pc AS
  SELECT DISTINCT *
  FROM c3_pc
")


# write to a parquet file
dbExecute(con, "COPY c3_pc TO 'data/harmonized/core/501c3-pc/full/CORE-FULL-501C3-CHARITIES-PC-HRMN-V1.parquet' (FORMAT PARQUET)")

# Save individual data marts

for (year in tax_years$TAX_YEAR){
  message(paste("Processing", year))
  filename <- sprintf("CORE-%s-501C3-CHARITIES-PC-HRMN-V1.csv", year)
  filepath <- paste0("data/harmonized/core/501c3-pc/marts/", filename, collapse = "")
  pf <- dbGetQuery(con,
                   sprintf("SELECT DISTINCT * FROM c3_pc 
                            WHERE TAX_YEAR = '%s'", year))
  data.table::fwrite(pf, filepath)
}

# Create data dictionary
concordance <- rio::import("data/concordance/CONCORDANCE-990_990EZ-V0.csv")
concordance <- concordance |>
  dplyr::select(
    variable_name,
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

data.table::fwrite(dd, "data/harmonized/core/501c3-pc/dd/DD-CORE-501C3-CHARITIES-PC-HRMN-V1.csv")

# C3 - PZ Scope

c3_pz_old <- purrr::map(
  list.files("data/harmonized/core/501c3-pz/", full.names = TRUE),
  data.table::fread
) |>
  data.table::rbindlist(fill = TRUE)

# For PZ Scope, you want all the SOI extracts
pz_names <- purrr::map(
  .x = list(soi_hrmn_ls[[2]], soi_hrmn_ls[[4]]),
  names
) |>
  unlist() |>
  unique()


c3_pz_new <- data.table::rbindlist(soi_hrmn_ls, fill = TRUE) |>
  dplyr::select(
    dplyr::all_of(pz_names)
  ) |>
  dplyr::filter(
    F9_00_SUBSECTION_CODE == 3
  ) |>
  dplyr::distinct()

cols_to_exclude <- c(
  "F9_00_TAX_PERIOD_BEGIN_DATE",
  "F9_00_TAX_YEAR",
  "F9_03_PROG_CODE",
  "F9_03_PROG_CODE_2",
  "F9_03_PROG_CODE_3",
  "F9_08_REV_PROG_BIZCODE",
  "F9_08_REV_PROG_BIZCODE_2",
  "F9_08_REV_PROG_BIZCODE_3",
  "F9_08_REV_PROG_BIZCODE_4",
  "F9_08_REV_PROG_BIZCODE_5",
  "F9_09_EXP_OTH_OTH_TOT",
  "F9_10_ASSET_LAND_BLDG_DEPREC",
  "GEO_ZIP5",
  "MISSION_NTEE",
  "DUP_RTRN_X",
  "F9_00_GROUP_EXEMPT_NUM",
  "F9_00_RETURN_TYPE",
  "F9_00_ORG_ADDR_CITY",
  "F9_00_ORG_ADDR_L1",
  "F9_00_ORG_ADDR_STATE",
  "F9_00_ORG_ADDR_ZIP",
  "F9_00_ORG_NAME_DBA_L1",
  "F9_00_ORG_NAME_L1"
)

c3_pz_old <- c3_pz_old |>
  dplyr::select(!any_of(cols_to_exclude)) |>
  dplyr::rename(
    "F9_00_SUBSECTION_CODE" = BMF_SUBSECTION_CODE,
    "F9_08_REV_MISC_BIZCODE_A" = F9_08_REV_MISC_BIZCODE,
    "F9_08_REV_MISC_BIZCODE_B" = F9_08_REV_MISC_BIZCODE_2,
    "F9_08_REV_MISC_BIZCODE_C" = F9_08_REV_MISC_BIZCODE_3,
    "F9_08_REV_MISC_TOT_A" = F9_08_REV_MISC_TOT,
    "F9_08_REV_MISC_TOT_B" = F9_08_REV_MISC_TOT_2,
    "F9_08_REV_MISC_TOT_C" = F9_08_REV_MISC_TOT_3,
    "F9_08_REV_PROG_TOT_A" = F9_08_REV_PROG_TOT,
    "F9_08_REV_PROG_TOT_B" = F9_08_REV_PROG_TOT_2,
    "F9_08_REV_PROG_TOT_C" = F9_08_REV_PROG_TOT_3,
    "F9_08_REV_PROG_TOT_D" = F9_08_REV_PROG_TOT_4,
    "F9_09_EXP_OTH_TOT_A" = F9_09_EXP_OTH_TOT,
    "F9_09_EXP_OTH_TOT_B" = F9_09_EXP_OTH_TOT_2,
    "F9_09_EXP_OTH_TOT_C" = F9_09_EXP_OTH_TOT_3,
    "F9_09_EXP_OTH_TOT_D" = F9_09_EXP_OTH_TOT_4
  )

c3_pz_full <- list(c3_pz_old, c3_pz_new)

c3_pz_full <- data.table::rbindlist(c3_pz_full, fill = TRUE) |>
  dplyr::distinct()

# Import into duckdb

con <- dbConnect(duckdb(), dbdir = "my_database.duckdb")
dbWriteTable(con, "c3_pz", c3_pz_full, overwrite = TRUE)

# Get tax years
tax_years <- dbGetQuery(con,
                        "SELECT DISTINCT TAX_YEAR FROM c3_pz ORDER BY TAX_YEAR DESC")
tax_years

tax_years <- tax_years |>
  dplyr::filter(
    TAX_YEAR >= 1989
  )
# Get schema and data types
schema <- dbGetQuery(con, "PRAGMA table_info(c3_pz)")

# Add in metadata
harmonized_vars <- schema$name
## unharmonized columns
## years for each variable
coverages <- c()
for (var in harmonized_vars){
  years <- dbGetQuery(con,
                      sprintf("SELECT TAX_YEAR FROM c3_pz 
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
record_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM c3_pz")
for (var in harmonized_vars){
  na_count <- dbGetQuery(con,
                         sprintf("SELECT COUNT(*) as null_count FROM c3_pz 
                                 WHERE %s IS NULL AND CAST(TAX_YEAR AS INTEGER) >= 1989", 
                                 var)) |>
    unlist()
  na_percent <- na_count / record_count * 100
  na_percents <- c(na_percents, na_percent)
}

schema$na_percent <- unlist(na_percents)

# Cardinality
cardinality <- c()
for (var in harmonized_vars){
  cardinality_percent <- dbGetQuery(con, sprintf(
    "SELECT COUNT(DISTINCT %s) * 100.0 / COUNT(%s) AS cardinality_ratio FROM c3_pz
     WHERE CAST(TAX_YEAR AS INTEGER) >= 1989",
    var,
    var
  )) |> unlist()
  cardinality <- c(cardinality, cardinality_percent)
}
schema$cardinality_percent <- cardinality

# Save schema

data.table::fwrite(schema, "data/schemas/SCHEMA-CORE-501C3-CHARITIES-PZ-HRMN-V1.csv")

# Deduplicate
dbExecute(con, "
  CREATE OR REPLACE TABLE c3_pz AS
  SELECT DISTINCT *
  FROM c3_pz
")


# write to a parquet file
dbExecute(con, "COPY c3_pz TO 'data/harmonized/core/501c3-pz/full/CORE-FULL-501C3-CHARITIES-PZ-HRMN-V1.parquet' (FORMAT PARQUET)")

# Save individual data marts

for (year in tax_years$TAX_YEAR){
  message(paste("Processing", year))
  filename <- sprintf("CORE-%s-501C3-CHARITIES-PZ-HRMN-V1.csv", year)
  filepath <- paste0("data/harmonized/core/501c3-pz/marts/", filename, collapse = "")
  pf <- dbGetQuery(con,
                   sprintf("SELECT DISTINCT * FROM c3_pz 
                            WHERE TAX_YEAR = '%s'", year))
  data.table::fwrite(pf, filepath)
}

# Create data dictionary
concordance <- rio::import("data/concordance/CONCORDANCE-990_990EZ-V0.csv")
concordance <- concordance |>
  dplyr::select(
    variable_name,
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

data.table::fwrite(dd, "data/harmonized/core/501c3-pz/dd/DD-CORE-501C3-CHARITIES-PZ-HRMN-V1.csv")


# CE - PC Scope

ce_pc_old <- purrr::map(
  list.files("data/harmonized/core/501ce-pc/", full.names = TRUE),
  data.table::fread
) |>
  data.table::rbindlist(fill = TRUE)

ce_pc_new_ls <- list(
  soi_hrmn_ls[[1]],
  soi_hrmn_ls[[3]]
)

ce_pc_new <- data.table::rbindlist(ce_pc_new_ls) |>
  dplyr::filter(
    F9_00_SUBSECTION_CODE != 3
  ) |>
  dplyr::distinct()

cols_to_exclude <- c(
  "F9_00_TAX_PERIOD_BEGIN_DATE",
  "F9_00_TAX_YEAR",
  "F9_03_PROG_CODE",
  "F9_03_PROG_CODE_2",
  "F9_03_PROG_CODE_3",
  "F9_08_REV_PROG_BIZCODE",
  "F9_08_REV_PROG_BIZCODE_2",
  "F9_08_REV_PROG_BIZCODE_3",
  "F9_08_REV_PROG_BIZCODE_4",
  "F9_08_REV_PROG_BIZCODE_5",
  "F9_09_EXP_OTH_OTH_TOT",
  "F9_10_ASSET_LAND_BLDG_DEPREC",
  "GEO_ZIP5",
  "MISSION_NTEE",
  "DUP_RTRN_X",
  "F9_00_GROUP_EXEMPT_NUM",
  "F9_00_RETURN_TYPE",
  "F9_00_ORG_ADDR_CITY",
  "F9_00_ORG_ADDR_L1",
  "F9_00_ORG_ADDR_STATE",
  "F9_00_ORG_ADDR_ZIP",
  "F9_00_ORG_NAME_DBA_L1",
  "F9_00_ORG_NAME_L1"
)

ce_pc_old <- ce_pc_old |>
  dplyr::select(!any_of(cols_to_exclude)) |>
  dplyr::rename(
    "F9_00_SUBSECTION_CODE" = BMF_SUBSECTION_CODE,
    "F9_08_REV_MISC_BIZCODE_A" = F9_08_REV_MISC_BIZCODE,
    "F9_08_REV_MISC_BIZCODE_B" = F9_08_REV_MISC_BIZCODE_2,
    "F9_08_REV_MISC_BIZCODE_C" = F9_08_REV_MISC_BIZCODE_3,
    "F9_08_REV_MISC_TOT_A" = F9_08_REV_MISC_TOT,
    "F9_08_REV_MISC_TOT_B" = F9_08_REV_MISC_TOT_2,
    "F9_08_REV_MISC_TOT_C" = F9_08_REV_MISC_TOT_3,
    "F9_08_REV_PROG_DESC_A" = F9_08_REV_PROG_DESC,
    "F9_08_REV_PROG_DESC_B" = F9_08_REV_PROG_DESC_2,
    "F9_08_REV_PROG_DESC_C" = F9_08_REV_PROG_DESC_3,
    "F9_08_REV_PROG_DESC_D" = F9_08_REV_PROG_DESC_4,
    "F9_08_REV_PROG_DESC_E" = F9_08_REV_PROG_DESC_5,
    "F9_08_REV_PROG_TOT_A" = F9_08_REV_PROG_TOT,
    "F9_08_REV_PROG_TOT_B" = F9_08_REV_PROG_TOT_2,
    "F9_08_REV_PROG_TOT_C" = F9_08_REV_PROG_TOT_3,
    "F9_08_REV_PROG_TOT_D" = F9_08_REV_PROG_TOT_4,
    "F9_08_REV_PROG_TOT_E" = F9_08_REV_PROG_TOT_5,
    "F9_09_EXP_OTH_TOT_A" = F9_09_EXP_OTH_TOT,
    "F9_09_EXP_OTH_TOT_B" = F9_09_EXP_OTH_TOT_2,
    "F9_09_EXP_OTH_TOT_C" = F9_09_EXP_OTH_TOT_3,
    "F9_09_EXP_OTH_TOT_D" = F9_09_EXP_OTH_TOT_4,
    "F9_09_EXP_OTH_TOT_E" = F9_09_EXP_OTH_TOT_5,
    "F9_09_EXP_OTH_TOT_F" = F9_09_EXP_OTH_TOT_6
  )

ce_pc_full <- list(ce_pc_old, ce_pc_new)

ce_pc_full <- data.table::rbindlist(ce_pc_full, fill = TRUE) |>
  dplyr::distinct()

# Import into duckdb

dbWriteTable(con, "ce_pc", ce_pc_full, overwrite = TRUE)

# Get tax years
tax_years <- dbGetQuery(con, 
                        "SELECT DISTINCT TAX_YEAR 
                         FROM ce_pc ORDER 
                         BY TAX_YEAR DESC")
tax_years

tax_years <- tax_years |>
  dplyr::filter(
    TAX_YEAR >= 2012
  )

# Get schema and data types
schema <- dbGetQuery(con, "PRAGMA table_info(ce_pc)")

# Add in metadata
harmonized_vars <- schema$name
## unharmonized columns
## years for each variable
coverages <- c()
for (var in harmonized_vars){
  years <- dbGetQuery(con,
                      sprintf("SELECT TAX_YEAR FROM ce_pc 
                    WHERE %s IS NOT NULL AND CAST(TAX_YEAR AS INTEGER) >= 2012
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
record_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM ce_pc")
for (var in harmonized_vars){
  na_count <- dbGetQuery(con,
                         sprintf("SELECT COUNT(*) as null_count FROM ce_pc 
                                 WHERE %s IS NULL AND CAST(TAX_YEAR AS INTEGER) >= 2012", 
                                 var)) |>
    unlist()
  na_percent <- na_count / record_count * 100
  na_percents <- c(na_percents, na_percent)
}

schema$na_percent <- unlist(na_percents)

# Cardinality
cardinality <- c()
for (var in harmonized_vars){
  cardinality_percent <- dbGetQuery(con, sprintf(
    "SELECT COUNT(DISTINCT %s) * 100.0 / COUNT(%s) AS cardinality_ratio FROM ce_pc
     WHERE CAST(TAX_YEAR AS INTEGER) >= 2012",
    var,
    var
  )) |> unlist()
  cardinality <- c(cardinality, cardinality_percent)
}

schema$cardinality_percent <- cardinality

# Save schema

data.table::fwrite(schema, "data/schemas/SCHEMA-CORE-501CE-NONPROFIT-PC-HRMN-V1.csv")

# Deduplicate
dbExecute(con, "
  CREATE OR REPLACE TABLE ce_pc AS
  SELECT DISTINCT *
  FROM ce_pc
")


# write to a parquet file
dbExecute(con, "COPY ce_pc TO 'data/harmonized/core/501ce-pc/full/CORE-FULL-501CE-NONPROFIT-PC-HRMN-V1.parquet' (FORMAT PARQUET)")

# Save individual data marts

for (year in tax_years$TAX_YEAR){
  message(paste("Processing", year))
  filename <- sprintf("CORE-%s-501CE-NONPROFIT-PC-HRMN-V1.csv", year)
  filepath <- paste0("data/harmonized/core/501ce-pc/marts/", filename, collapse = "")
  pf <- dbGetQuery(con,
                   sprintf("SELECT DISTINCT * FROM ce_pc 
                            WHERE TAX_YEAR = '%s'", year))
  data.table::fwrite(pf, filepath)
}

# Create data dictionary
concordance <- rio::import("data/concordance/CONCORDANCE-990_990EZ-V0.csv")
concordance <- concordance |>
  dplyr::select(
    variable_name,
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

data.table::fwrite(dd, "data/harmonized/core/501ce-pc/dd/DD-CORE-501CE-NONPROFIT-PC-HRMN-V1.csv")

# CE - PZ Scope

ce_pz_old <- purrr::map(
  list.files("data/harmonized/core/501ce-pz/", full.names = TRUE)[1:34],
  data.table::fread
) |>
  data.table::rbindlist(fill = TRUE)

# For PZ Scope, you want all the SOI extracts
pz_names <- purrr::map(
  .x = list(soi_hrmn_ls[[2]], soi_hrmn_ls[[4]]),
  names
) |>
  unlist() |>
  unique()

ce_pz_new <- data.table::rbindlist(soi_hrmn_ls, fill = TRUE) |>
  dplyr::select(
    dplyr::all_of(pz_names)
  ) |>
  dplyr::filter(
    F9_00_SUBSECTION_CODE != 3
  ) |>
  dplyr::distinct()

cols_to_exclude <- c(
  "F9_00_TAX_PERIOD_BEGIN_DATE",
  "F9_00_TAX_YEAR",
  "F9_03_PROG_CODE",
  "F9_03_PROG_CODE_2",
  "F9_03_PROG_CODE_3",
  "F9_08_REV_PROG_BIZCODE",
  "F9_08_REV_PROG_BIZCODE_2",
  "F9_08_REV_PROG_BIZCODE_3",
  "F9_08_REV_PROG_BIZCODE_4",
  "F9_08_REV_PROG_BIZCODE_5",
  "F9_09_EXP_OTH_OTH_TOT",
  "F9_10_ASSET_LAND_BLDG_DEPREC",
  "GEO_ZIP5",
  "MISSION_NTEE",
  "DUP_RTRN_X",
  "F9_00_GROUP_EXEMPT_NUM",
  "F9_00_RETURN_TYPE",
  "F9_00_ORG_ADDR_CITY",
  "F9_00_ORG_ADDR_L1",
  "F9_00_ORG_ADDR_STATE",
  "F9_00_ORG_ADDR_ZIP",
  "F9_00_ORG_NAME_DBA_L1",
  "F9_00_ORG_NAME_L1"
)

ce_pz_old <- ce_pz_old |>
  dplyr::select(!any_of(cols_to_exclude)) |>
  dplyr::rename(
    "F9_00_SUBSECTION_CODE" = BMF_SUBSECTION_CODE,
    "F9_08_REV_MISC_BIZCODE_A" = F9_08_REV_MISC_BIZCODE,
    "F9_08_REV_MISC_BIZCODE_B" = F9_08_REV_MISC_BIZCODE_2,
    "F9_08_REV_MISC_BIZCODE_C" = F9_08_REV_MISC_BIZCODE_3,
    "F9_08_REV_MISC_TOT_A" = F9_08_REV_MISC_TOT,
    "F9_08_REV_MISC_TOT_B" = F9_08_REV_MISC_TOT_2,
    "F9_08_REV_MISC_TOT_C" = F9_08_REV_MISC_TOT_3,
    "F9_08_REV_PROG_TOT_A" = F9_08_REV_PROG_TOT,
    "F9_08_REV_PROG_TOT_B" = F9_08_REV_PROG_TOT_2,
    "F9_08_REV_PROG_TOT_C" = F9_08_REV_PROG_TOT_3,
    "F9_08_REV_PROG_TOT_D" = F9_08_REV_PROG_TOT_4,
    "F9_08_REV_PROG_TOT_E" = F9_08_REV_PROG_TOT_5,
    "F9_09_EXP_OTH_TOT_A" = F9_09_EXP_OTH_TOT,
    "F9_09_EXP_OTH_TOT_B" = F9_09_EXP_OTH_TOT_2,
    "F9_09_EXP_OTH_TOT_C" = F9_09_EXP_OTH_TOT_3,
    "F9_09_EXP_OTH_TOT_D" = F9_09_EXP_OTH_TOT_4
  )

ce_pz_full <- list(ce_pz_old, ce_pz_new)

ce_pz_full <- data.table::rbindlist(ce_pz_full, fill = TRUE) |>
  dplyr::distinct()

# Import into duckdb

dbWriteTable(con, "ce_pz", ce_pz_full, overwrite = TRUE)

# Get tax years
tax_years <- dbGetQuery(con, 
                        "SELECT DISTINCT TAX_YEAR 
                         FROM ce_pz ORDER 
                         BY TAX_YEAR DESC")
tax_years
# Get schema and data types
schema <- dbGetQuery(con, "PRAGMA table_info(ce_pz)")

# Add in metadata
harmonized_vars <- schema$name
## unharmonized columns
## years for each variable
coverages <- c()
for (var in harmonized_vars){
  years <- dbGetQuery(con,
                      sprintf("SELECT TAX_YEAR FROM ce_pz 
                    WHERE %s IS NOT NULL AND CAST(TAX_YEAR AS INTEGER) >= 1989
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
record_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM ce_pz")
for (var in harmonized_vars){
  na_count <- dbGetQuery(con,
                         sprintf("SELECT COUNT(*) as null_count FROM ce_pz 
                                 WHERE %s IS NULL AND CAST(TAX_YEAR AS INTEGER) >= 1989", 
                                 var)) |>
    unlist()
  na_percent <- na_count / record_count * 100
  na_percents <- c(na_percents, na_percent)
}

schema$na_percent <- unlist(na_percents)

# Cardinality
cardinality <- c()
for (var in harmonized_vars){
  cardinality_percent <- dbGetQuery(con, sprintf(
    "SELECT COUNT(DISTINCT %s) * 100.0 / COUNT(%s) AS cardinality_ratio FROM ce_pz
     WHERE CAST(TAX_YEAR AS INTEGER) >= 1989",
    var,
    var
  )) |> unlist()
  cardinality <- c(cardinality, cardinality_percent)
}

schema$cardinality_percent <- cardinality

# Exclude columns with NAs above a certain threshold
na_excl <- schema$name[schema$na_percent > 70]

schema <- schema |>
  dplyr::filter(
    ! name %in% na_excl
  )

# Save schema

data.table::fwrite(schema, "data/schemas/SCHEMA-CORE-501CE-NONPROFIT-PZ-HRMN-V1.csv")

# Deduplicate and exclude columns

select_query <- paste0( "CREATE OR REPLACE TABLE ce_pc AS SELECT DISTINCT ",
                        paste(schema$name, collapse = ", "),
                        " FROM ce_pz")

dbExecute(con, select_query)


# write to a parquet file
dbExecute(con, "COPY ce_pz TO 'data/harmonized/core/501ce-pz/full/CORE-FULL-501CE-NONPROFIT-PZ-HRMN-V1.parquet' (FORMAT PARQUET)")

# Save individual data marts

for (year in tax_years$TAX_YEAR){
  message(paste("Processing", year))
  filename <- sprintf("CORE-%s-501CE-NONPROFIT-PZ-HRMN-V1.csv", year)
  filepath <- paste0("data/harmonized/core/501ce-pz/marts/", filename, collapse = "")
  pf <- dbGetQuery(con,
                   sprintf("SELECT DISTINCT * FROM ce_pz 
                            WHERE TAX_YEAR = '%s'", year))
  data.table::fwrite(pf, filepath)
}

# Create data dictionary
concordance <- rio::import("data/concordance/CONCORDANCE-990_990EZ-V0.csv")
concordance <- concordance |>
  dplyr::select(
    variable_name,
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

data.table::fwrite(dd, "data/harmonized/core/501ce-pz/dd/DD-CORE-501CE-NONPROFIT-PZ-HRMN-V1.csv")
