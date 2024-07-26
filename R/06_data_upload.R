# Script Header
# Description: This script contains code to upload new CORE releases to S3 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25
# Details:
# (1) - Get paths to processed core files
# (2) - Upload to S3

# Load Packages
library(paws)
library(purrr)

# Load Helper Scripts
source("R/utils.R")

# (1): Get path to processed core files

pz_501c3_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501c3-pz/", scope = "501C3-CHARITIES-PZ")
pz_501ce_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501ce-pz/", scope = "501CE-NONPROFIT-PZ")
pc_501c3_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501c3-pc/", scope = "501C3-CHARITIES-PC")
pc_501ce_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501ce-pc/", scope = "501CE-NONPROFIT-PC")


# (2) Upload Files to S3

purrr::imap(
  pz_501c3_proc_filepaths_ls,
  .f = function(x, idx) {
    upload_to_s3(file_path = x,
                 file_name = idx,
                 s3_folder = "harmonized/core/501c3-pz/")
  }
)

purrr::imap(
  pz_501ce_proc_filepaths_ls,
  .f = function(x, idx) {
    upload_to_s3(file_path = x,
                 file_name = idx,
                 s3_folder = "harmonized/core/501ce-pz/")
  }
)

purrr::imap(
  pc_501c3_proc_filepaths_ls,
  .f = function(x, idx) {
    upload_to_s3(file_path = x,
                 file_name = idx,
                 s3_folder = "harmonized/core/501c3-pc/")
  }
)

purrr::imap(
  pc_501ce_proc_filepaths_ls,
  .f = function(x, idx) {
    upload_to_s3(file_path = x,
                 file_name = idx,
                 s3_folder = "harmonized/core/501ce-pc")
  }
)
