# Script Header
# Description: This script contains the code for validating CORE files after harmonization, processing, and merging with SOI data. 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25
# Details:
# (1) - Get file paths
# (2) - Generate validation reports

# Load Packages
library(purrr)
library(data.validator)
library(assertr)
library(tidyverse)
library(log4r)

# Helper Scripts
source("R/data_validate_helpers.R")
source("R/utils.R")

# Set up Logging
my_logger <- create_logger("data/logs/data_validate_log.txt")

# (1) Get Paths to Processed CORE Files

pz_501c3_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501c3-pz/", scope = "501C3-CHARITIES-PZ")
pz_501ce_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501ce-pz/", scope = "501CE-NONPROFIT-PZ")
pc_501c3_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501c3-pc/", scope = "501C3-CHARITIES-PC")
pc_501ce_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501ce-pc/", scope = "501CE-NONPROFIT-PC")

# (2) Validate Processed CORE Files

scope_ls <- list( "PC-501C3" = pc_501c3_proc_filepaths_ls,
                  "PZ-501C3" = pz_501c3_proc_filepaths_ls,
                  "PC-501CE" = pc_501ce_proc_filepaths_ls,
                  "PZ-501CE" = pz_501ce_proc_filepaths_ls)

purrr::imap(
  .x = scope_ls,
  .f = function( file_scope, filepath_ls ){
    purrr::map(
      filepath_ls,
      validate_processed_data,
      file_scope = file_scope,
      destfile = "core-validate.csv",
      save_results = TRUE,
      verbose = TRUE
    )
  }
)