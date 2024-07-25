# Script Header
# Description: This script contains helper functions for 03_data_merge-soi_helpers.R 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25

#' @title This function splits SOI data by subsection code before combining it with core data
#' @param year tax year of processed SOI data
#' @param subseccd data.table object containing EIN2 and BMD_SUBSECTION_CODE from unified bmf
subseccd_merge <- function(year, subseccd) {
  pc_path <- sprintf("harmonize/data/processed/soi/pc/SOI-EXTRACT-%s-PC-HRMN.csv", year)
  pc <- data.table::fread(pc_path, key = "EIN2")
  ez_path <- sprintf("harmonize/data/processed/soi/ez/SOI-EXTRACT-%s-EZ-HRMN.csv", year)
  ez <- data.table::fread(ez_path, key = "EIN2")
  pz <- data.table::rbindlist(list(pc, ez), fill = TRUE)
  
  pc <- subseccd[pc, on = "EIN2"]
  pz <- subseccd[pz, on = "EIN2"]
  
  pc_501c3 <- pc[BMF_SUBSECTION_CODE == 3, ]
  path <- sprintf("harmonize/data/processed/core/501c3-pc/CORE-%s-501C3-CHARITIES-PC-HRMN.csv", year)
  combine_files(path, pc_501c3)
  
  
  pz_501c3 <- pz[BMF_SUBSECTION_CODE == 3, ]
  path <- sprintf("harmonize/data/processed/core/501c3-pz/CORE-%s-501C3-CHARITIES-PZ-HRMN.csv", year)
  combine_files(path, pz_501c3)
  
  pc_501ce <- pc[BMF_SUBSECTION_CODE != 3, ]
  path <- sprintf("harmonize/data/processed/core/501ce-pc/CORE-%s-501CE-NONPROFIT-PC-HRMN.csv", year)
  combine_files(path, pc_501ce)
  
  pz_501ce <- pz[BMF_SUBSECTION_CODE != 3, ]
  path <- sprintf("harmonize/data/processed/core/501ce-pz/CORE-%s-501CE-NONPROFIT-PZ-HRMN.csv", year)
  combine_files(path, pz_501ce)
}

#' @title This function appends SOI records to a core file
#' @param path character scalar. Path to core file
#' @param soi_dat data.table. SOI data to append
#' @return Message indicating that SOI data has been appended to the core file
combine_files <- function(path, soi_dat){
  if (file.exists(path)) {
    core_dat <- data.table::fread(path)
    core_eins <- unique(core_dat$EIN2)
    soi_dat <- soi_dat[! (EIN2 %in% core_eins), ]
    soi_dat <- data.table::rbindlist(list(soi_dat, core_dat), fill = TRUE)
    soi_dat <- unique(soi_dat)
    data.table::fwrite(soi_dat, path)
  } else {
    soi_dat <- unique(soi_dat)
    data.table::fwrite(soi_dat, path)
  }
  return(message("SOI data appended to ", path))
}