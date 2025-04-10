# Script to update the core files


# Packages
library(log4r)
library(paws)
library(purrr)
library(stringr)
library(rio)

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

download_raw_data(url_ls = soi_updt_url_ls,
                  destfolder = "data/raw/soi/",
                  logger = my_logger)

# Step 3: Begin harmonization

# 3.1 read in crosswalk\

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


soi_hrmn_ls <- purrr::map(
  soi_ls,
  rename_soi_990ez,
  xwalk_990ez,
  my_logger,
  .progress = TRUE
)


# C3 - PC Scope

c3_pc <- purrr::map(
  list.files("data/harmonized/core/501c3-pc/", full.names = TRUE),
  data.table::fread
)

c3_pc_new <- data.table::rbindlist(
  list(
    soi_hrmn_ls[[1]],
    soi_hrmn_ls[[3]]
  )
) |>
  dplyr::filter(
    F9_00_SUBSECTION_CODE == 3
  ) |>
  dplyr::distinct()

c3_pc_full <- data.table::rbindlist(
  c(c3_pc, c3_pc_new),
  fill = TRUE
)


