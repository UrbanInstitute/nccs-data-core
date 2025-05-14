# Script Header
# Description: This script contains the post-processing code for CORE Harmonization, for after files have been harmonized.
# It adds extra columns.
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25
# Details:
# (1) - Load Crosswalks
# (2) - Run Harmonization

# Load Packages
library(purrr)
library(data.table)
library(log4r)

# Helper Scripts
source("R/utils.R")
source("R/data_post-process_helpers.R")

# Set up Logging
my_logger <- create_logger("data/logs/data_post-process_log.txt")

# (1) Get Paths to Harmonized Files

# (1.1) SOI

soi_pc_hrmn_filepaths_ls <- get_files(folder_name = "data/harmonized/soi/pc/", scope = "PC")
soi_ez_hrmn_filepaths_ls <- get_files(folder_name = "data/harmonized/soi/ez/", scope = "EZ")

# (1.2) CORE

core_pz_501c3_hrmn_filepaths_ls <- get_files(folder_name = "data/harmonized/core/501c3-pz/", scope = "501C3-CHARITIES-PZ")
core_pz_501ce_hrmn_filepaths_ls <- get_files(folder_name = "data/harmonized/core/501ce-pz/", scope = "501CE-NONPROFIT-PZ")
core_pc_501c3_hrmn_filepaths_ls <- get_files(folder_name = "data/harmonized/core/501c3-pc/", scope = "501C3-CHARITIES-PC")
core_pc_501ce_hrmn_filepaths_ls <- get_files(folder_name = "data/harmonized/core/501ce-pc/", scope = "501CE-NONPROFIT-PC")

# (2) Process Harmonized Files

# (2.1) SOI

soi_post_process_ls <- list(
  "data/processed/soi/pc/" = soi_pc_hrmn_filepaths_ls,
  "data/processed/soi/ez/" = soi_ez_hrmn_filepaths_ls
)

purrr::imap(
  .x = soi_post_process_ls,
  .f = function( filepaths, dest_folder ){
    purrr::map(
      filepaths,
      process_harmonized_data,
      dest_folder = dest_folder,
      .progress = "SOI Post Processing Progress"
    )
  }
)

# (2.2) CORE

core_post_process_ls <- list(
  "data/processed/core/501c3-pz/" = core_pz_501c3_hrmn_filepaths_ls,
  "data/processed/core/501ce-pz/" = core_pz_501ce_hrmn_filepaths_ls,
  "data/processed/core/501c3-pc/" = core_pc_501c3_hrmn_filepaths_ls,
  "data/processed/core/501ce-pc/" = core_pc_501ce_hrmn_filepaths_ls
)

purrr::imap(
  .x = core_post_process_ls,
  .f = function( filepaths, dest_folder ){
    purrr::map(
      filepaths,
      process_harmonized_data,
      dest_folder = dest_folder,
      .progress = "CORE Post Processing Progress"
    )
  }
)
