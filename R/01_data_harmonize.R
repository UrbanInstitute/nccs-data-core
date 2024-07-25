# Script Header
# Description: This script contains the harmonization code for CORE Harmonization 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25
# Details:
# (1) - Load Crosswalks
# (2) - Run Harmonization

# Load Packages
library(data.table)
library(purrr)
library(tidyverse)
library(log4r)

# Load Helper Scripts
source("R/data_harmonize_helpers.R")
source("R/utils.R")

# Set up Logging
my_logger <- create_logger("data/logs/data_harmonize_log.txt")

# (1) Load Crosswalks

# (1.1) SOI
XWALK_SOI <- readxl::read_xlsx("data/crosswalks/VARIABLE-NAME-CROSSWALK-V1.xlsx")

harmonize_cols <-
  c(
    "EFILE.VAR",
    "SOI.VAR",
    "YEAR.2021.ez",
    "YEAR.2021.pc",
    "YEAR.2020.ez",
    "YEAR.2020.pc",
    "YEAR.2019.ez",
    "YEAR.2019.pc",
    "YEAR.2018.ez",
    "YEAR.2018.pc",
    "YEAR.2017.ez",
    "YEAR.2017.pc",
    "YEAR.2016.ez",
    "YEAR.2016.pc"
  )

XWALK_SOI <- XWALK_SOI %>% 
  dplyr::select(tidyselect::all_of(harmonize_cols)) %>% 
  tidyr::pivot_longer(! EFILE.VAR,
                      names_to = "Series",
                      values_to = "var_old",
                      values_drop_na = TRUE) %>% 
  dplyr::rename("var_new" = "EFILE.VAR") %>% 
  dplyr::select(! Series) %>% 
  dplyr::mutate(var_old = toupper(var_old)) %>% 
  dplyr::distinct()

# (1.2) CORE
XWALK_CORE <- readr::read_csv( "data/crosswalks/VAR-CROSSWALK-CORE-COMBINED.csv" )

# (2) Run Harmonization

# Core

# (2.1) Get list of raw files

raw_core_501c3_pc_files_ls <- get_files( folder_name = "data/raw/core/", scope = "501C3-CHARITIES-PC" )
raw_core_501c3_pz_files_ls <- get_files( folder_name = "data/raw/core/", scope = "501C3-CHARITIES-PZ" )
raw_core_501ce_pc_files_ls <- get_files( folder_name = "data/raw/core/", scope = "501CE-NONPROFIT-PC" )
raw_core_501ce_pz_files_ls <- get_files( folder_name = "data/raw/core/", scope = "501CE-NONPROFIT-PZ" )

# (2.2) Harmonize

run_harmonization(
  raw_paths = raw_core_501c3_pc_files_ls,
  logger = my_logger,
  xwalk_df = XWALK_CORE,
  ds = "CORE",
  scope = "501C3-CHARITIES-PC",
  destfolder = "data/harmonized/core/501c3-pc/",
  tax_year_column = "F9_00_TAX_PERIOD_BEGIN_DATE",
  tax_years = as.character(2012:2019)
)

run_harmonization(
  raw_paths = raw_core_501ce_pc_files_ls,
  logger = my_logger,
  xwalk_df = XWALK_CORE,
  ds = "CORE",
  scope = "501CE-NONPROFIT-PC",
  destfolder = "data/harmonized/core/501ce-pc/",
  tax_year_column = "F9_00_TAX_PERIOD_BEGIN_DATE",
  tax_years = as.character(2012:2019)
)

run_harmonization(
  raw_paths = raw_core_501c3_pz_files_ls,
  logger = my_logger,
  xwalk_df = XWALK_CORE,
  ds = "CORE",
  scope = "501C3-CHARITIES-PZ",
  destfolder = "data/harmonized/core/501c3-pz/",
  tax_year_column = "F9_00_TAX_PERIOD_BEGIN_DATE",
  tax_years = as.character(1989:2019)
)

run_harmonization(
  raw_paths = raw_core_501ce_pz_files_ls,
  logger = my_logger,
  xwalk_df = XWALK_CORE,
  ds = "CORE",
  scope = "501CE-NONPROFIT-PZ",
  destfolder = "data/harmonized/core/501ce-pz/",
  tax_year_column = "F9_00_TAX_PERIOD_BEGIN_DATE",
  tax_years = as.character(1989:2019)
)
# SOI

# (2.3) Get list of raw files

raw_files <- list.files("data/raw/soi/")

pc_files <- raw_files[ grepl( "990\\.", raw_files ) ]
ez_files <- raw_files[ grepl( "ez", tolower( raw_files ) ) ]

# (2.4) Harmonize only PC and EZ files

scope_raw_ls <- list(
  "PC" = paste0("data/raw/soi/", pc_files),
  "EZ" = paste0("data/raw/soi/", ez_files)
)

run_harmonization(
  raw_paths = scope_raw_ls$PC,
  logger = my_logger,
  xwalk_df = XWALK_SOI,
  ds = "SOI-EXTRACT",
  scope = "PC",
  destfolder = "data/harmonized/soi/pc/",
  tax_year_column = "F9_00_TAX_PERIOD_END_DATE",
  tax_years = as.character(2012:2022)
)

run_harmonization(
  raw_paths = scope_raw_ls$EZ,
  logger = my_logger,
  xwalk_df = XWALK_SOI,
  ds = "SOI-EXTRACT",
  scope = "EZ",
  destfolder = "data/harmonized/soi/ez/",
  tax_year_column = "F9_00_TAX_PERIOD_END_DATE",
  tax_years = as.character(2012:2022)
)
