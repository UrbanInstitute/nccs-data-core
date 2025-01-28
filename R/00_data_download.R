# Script Header
# Description: This script contains the data download code for CORE Harmonization 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2025-01-28
# Details:
# (1) - Download IRS SOI Data (non PF)
# (2) - Download Legacy CORE Data (non PF)
# (3) - Download IRS SOI Data (PF)
# (4) - Download Legacy CORE Data (PF)

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

# (3) Download SOI Data from Giving Tuesday Data Lake (PF)

soi_pf_url_ls <- soi_url_ls[grepl("pf", soi_url_ls)]

# for christina
soi_pf_url_ls <- list(
  `12eofinextract990pf.dat` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/12eofinextract990pf.dat",
  `13eofinextract990pf.dat` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/13eofinextract990pf.dat",
  `14eofinextract990pf.dat` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/14eofinextract990pf.dat",
  `15eofinextract990pf.dat` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/15eofinextract990pf.dat",
  `16eofinextract990pf.dat` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/16eofinextract990pf.dat",
  `20eoextract990pf.xlsx` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/20eoextract990pf.xlsx",
  `21eoextract990pf.xlsx` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/21eoextract990pf.xlsx",
  `22eoextract990pf.xlsx` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/22eoextract990pf.xlsx",
  `23eoextract990pf.xlsx` = "https://gt990datalake-rawdata.s3.amazonaws.com/EfileData/Extracts/Data/23eoextract990pf.xlsx"
)

download_raw_data(url_ls = soi_pf_url_ls[3:4],
                  destfolder = "data/raw/soi_pf/",
                  logger = my_logger)

# (4) Download CORE Data from Legacy NCCS Site (PF)

core_pf_url_ls <- core_url_ls[grepl("-PF", core_url_ls)]

download_raw_data(url_ls = core_pf_url_ls,
                  destfolder = "data/raw/core_pf/",
                  logger = my_logger)
