# Script Header
# Description: This script contains the data download code for CORE Harmonization 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25
# Details:
# (1) - Download IRS SOI Data
# (2) - Download Legacy CORE Data

# Packages
library(log4r)
library(paws)
library(purrr)
library(stringr)
library(rio)

# Helper Scripts
source("R/utils.R")

# Logging
my_logger <- create_logger("data/logs/data_download_log.txt")

# (1) Download SOI Data from Giving Tuesday Data Lake

soi_url_ls <- get_s3_bucket_contents(
  bucket_name = "gt990datalake-rawdata",
  bucket_folder = "EfileData/Extracts/Data/",
  bucket_url = "https://gt990datalake-rawdata.s3.amazonaws.com/"
)

download_raw_data(url_ls = soi_url_ls,
                  destfolder = "data/raw/soi/",
                  logger = my_logger)

# (2) Download CORE Data from Legacy NCCS Site

core_url_ls <- get_s3_bucket_contents(
  bucket_name = "nccsdata",
  bucket_folder = "legacy/core/",
  bucket_url = "https://nccsdata.s3.amazonaws.com/"
)

core_url_ls <- core_url_ls[!grepl("html|COMPARISON", core_url_ls)]

download_raw_data(url_ls = core_url_ls,
                  destfolder = "data/raw/core/",
                  logger = my_logger)