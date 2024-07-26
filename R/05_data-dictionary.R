# Script Header
# Description: This script contains code for creating a data dictionary for the core files after validation is complete 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25
# Details:
# (1) - Get paths to files
# (2) - Create data dictionary

# Load Packages
library(purrr)
library(tidyverse)

# (1): Get path to processed core files

pz_501c3_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501c3-pz/", scope = "501C3-CHARITIES-PZ")
pz_501ce_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501ce-pz/", scope = "501CE-NONPROFIT-PZ")
pc_501c3_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501c3-pc/", scope = "501C3-CHARITIES-PC")
pc_501ce_proc_filepaths_ls <- get_files(folder_name = "data/processed/core/501ce-pc/", scope = "501CE-NONPROFIT-PC")

scope_ls <- list(
  "PC-501C3" = unlist(pc_501c3_proc_filepaths_ls),
  "PZ-501C3" = unlist(pz_501c3_proc_filepaths_ls),
  "PC-501CE" = unlist(pc_501ce_proc_filepaths_ls),
  "PZ-501CE" = unlist(pz_501ce_proc_filepaths_ls)
)

# (2) Create Data Dictionary

data_dictionary <- create_master_data_dictionary(
  proc_files_scope_ls = scope_ls,
  save_results = TRUE,
  destfile = "data/dictionary/core/CORE-HRMN_dd.csv"
)
