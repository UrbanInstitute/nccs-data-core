# Script Header
# Description: Helper Functions for 02_data_post-process.R
# It adds extra columns.
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25

#' @title Function for postprocessing data harmonized by tax year
#'
#' @description This function creates 3 new columns:
#'  1. RTRN_ID: unique return IDs "EIN_{EIN}_TAXYEAR"
#'  2. DUP_RTRN_X: Boolean column indicating if the the return shares the same EIN
#'  as another return. This means that an organization refiled their tex return.
#'  The function also deletes redundant columns: (V1)
#'  3. TAX_YEAR. Column indicating Tax Year.
#'
#' @param inpath character scalar. Path to harmonized file.
#' @param dest_folder output folder
#'
#' @return data.table object with post processed data for reupload to S3.

process_harmonized_data <- function(inpath, dest_folder) {
  if (!file.exists(dest_folder)) {
    dir.create(dest_folder , recursive = TRUE)
  }
  
  dat <- data.table::fread(inpath, colClasses = list(character = c("F9_00_ORG_EIN")))
  
  tax_year <- dat[["TAX_YEAR"]]
  
  #' @title This function formats the EIN column to make it 9 digits by adding 0s
  #' at the start
  #' @param ein character scalar. Organization EIN
  #' @return character scalar. Reformatted EIN
  
  format_ein <- function(ein) {
    if (is.na(ein)) {
      ein <- "000000000"
      return(ein)
    } else {
      ein_len <- nchar(ein)
      if (ein_len == 9) {
        return(ein)
      } else {
        diff = 9 - ein_len
        diff = rep("0", diff)
        diff = paste0(diff, collapse = "")
        ein <- paste0(diff, ein, collapse = "")
        return(ein)
      }
    }
  }
  
  #' @title This function derives EIN2 from EIN column. EIN-XX-XXXXXXX
  #' @param character scalar. Formatted 9-digit EIN
  #' @returns character scalar. EIN2.
  derive_ein2 <- function(ein) {
    ein2 <- format_ein(ein)
    ein2 <- paste0("EIN-", substr(ein2, 1, 2), "-", substr(ein2, 3, 9))
    return(ein2)
  }
  
  # Format EINs and create EIN2
  dat[, F9_00_ORG_EIN := format_ein(F9_00_ORG_EIN), by = 1:nrow(dat)]
  dat[, EIN2 := derive_ein2(F9_00_ORG_EIN), by = 1:nrow(dat)]
  # Identify duplicate returns
  dat[, DUP_RTRN_X := ifelse(.N > 1, 1, 0), by = EIN2]
  # Remove columns which consist of only NA values
  dat[, .SD, .SDcols = colSums(!is.na(dat)) > 0]
  
  outpath <- gsub("harmonized", "processed", inpath)
  
  data.table::fwrite(dat, outpath)
  
  return(sprintf("Finished processing:\n %s", outpath))
  
}