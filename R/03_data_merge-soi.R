# Script Header
# Description: This script contains the code to update harmonized CORE files with SOI Files 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25
# Details:
# (1) - Load Unified BMF
# (2) - Update CORE with SOI

# Load Packages
library(data.table)
library(log4r)
library(purrr)

# Helper Scripts
source("R/utils.R")
source("R/data_merge-soi_helpers.R")

# Logging
my_logger <- create_logger("data/logs/data_merge_soi_core_log.txt")

# (1) Load in Unified BMF

subsection.code_bmf <- data.table::fread(
  "https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/BMF_UNIFIED_V1.1.csv",
  select = c(
    "EIN2",
    "BMF_SUBSECTION_CODE"
  )
)

# (2) Merge SOI Data for specified tax years

years <- 2012:2022

purrr::map(
  .x = years,
  .f = subseccd_merge,
  subseccd = subseccd,
  .progress = "Merging SOI with Core Data"
)
