# Script Header
# Description: This script standardizes the variable names across both the Legacy CORE files and the SOI files.
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2025-05-01
# Details:
# (1) - Load Crosswalks
# (2) - Run Harmonization

# Set up Logging
my_logger <- create_logger("data/logs/data_harmonize_log.txt")

# (1) Load Crosswalks

# (1.1) 990 and 990EZ crosswalk
xwalk_990 <- data.table::fread("data/crosswalks/XWALK-990-V2.csv") |>
  dplyr::filter(
    VAR_NAME_NEW != "NOT IN CONCORDANCE",
    VAR_NAME_NEW != "NOT IN DD"
  ) |>
  dplyr::distinct()

# (1.2) 990PF crosswalk
xwalk_990PF <- rio::import("data/crosswalks/XWALK-990PF-V0.xlsx") |>
  dplyr::filter(is.na(FOUND_IN_BMF)) |>
  dplyr::select(VAR_NAME_OLD,
                VAR_NAME_NEW)

# (2) Harmonize datasets - Standardize variable names across datasets

# (2.1) 501C3-CHARITIES-PC
legacy_501c3pc_file_ls <- get_files(folder_name = "data/raw/core/", 
                                    scope = "501C3-CHARITIES-PC")
soi_990ez_file_ls <- get_files(folder_name = "data/raw/soi/", 
                               scope = "ez\\.xlsx")
pc_501c3_file_ls <- c(legacy_501c3pc_file_ls, soi_990ez_file_ls)

# Extract
extract_filings <- function(file_path){
  dat <- data.table::fread(file_path)
}
# Transform
harmonize_filings <- function(dat, xwalk_df, logger, file_path){
  # Get necessary metadata
  legacy_colnames <- names(dat)
  file_name <- tools::file_path_sans_ext(file_path)
  # Harmonize columns
  xwalk_df <- xwalk_df |> dplyr::filter(VAR_NAME_OLD %in% legacy_colnames)
  harmonizable_cols <- xwalk_df$VAR_NAME_OLD
  dat_hrmn <- dat[, ..harmonizable_cols]
  data.table::setnames(dat_hrmn, 
                       old = harmonizable_cols, 
                       new = xwalk_df$VAR_NAME_NEW)
  # Log unharmonized columns
  unharmonized_cols <- setdiff(legacy_colnames, harmonizable_cols)
  log4r::info(logger,
              message = paste(
                "File:",
                file_name,
                "-",
                "Unharmonized cols:",
                paste0(unharmonized_cols, collapse = ", ")
              ))
  
  return(dat_hrmn)
}
create_ein2 <- function(dat){
  dat[, EIN2 := format_ein(EIN, to = "n")]
  return(dat)
}
create_taxyear <- function(dat, tax_year_column) {
  dat[, TAX_YEAR := as.character(substr(get(tax_year_column), 1, 4))]
  return(dat)
}
# Load
write_to_database <- function(dat, path, table_name){
  db_name <- paste0(path, table_name, ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_name)
  DBI::dbWriteTable(con, table_name, dat, append = TRUE)
  DBI::dbDisconnect(con)
}

# Create Schema

# After SOI do partitioning and then create new schema

# (2.1) Get list of raw files

# PC and PZ

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
pf_files_ls <- get_files(folder_name = "data/raw/soi_pf/", scope = "pf")

# (2.4) Harmonize 

# PC and EZ files
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

run_harmonization(
  raw_paths = pf_files_ls[1:2],
  logger = my_logger,
  xwalk_df = pf_xwalk,
  ds = "SOI-EXTRACT",
  scope = "PF",
  tax_year_column = "F9_00_TAX_PERIOD_END_DATE",
  tax_years = as.character(2012:2022)
)

test_pf_path <- pf_files_ls[[1]]
pf <- data.table::fread(test_pf_path)

pf_hrmn <- harmonize_data(pf, pf_xwalk)

unique




library(dtplyr)

pf_lazy <- dtplyr::lazy_dt(pf)

pf_lazy <- pf_lazy |>
  dplyr::mutate(newcol = VALASSETSCOLB * 25)

pf_processed <- data.frame(pf_lazy)

# rename columns
# save everything by year


# TODO:
# Validation check: Unharmonized columns
# Consistent Tax Year Column