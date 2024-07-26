# Script Header
# Description: This script contains helper functions for validation in 04_data_validate.R 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25

#' @title Function to perform data validation checks
#' @description This function performs specified validation checks to processed files and saves the outputs
#' @param proc_file_paths_ls named list. Named list mapping processed file name to path
#' @param file_scope chatacter scalar. Form scope of files to validate
#' @param destfile character scalar. Destination file for validation results
#' @save_results boolean. TRUE == save results to destfile
#' @verbose boolean. TRUE == print out validation results
#' @return data.frame object. Validation results

validate_processed_data <- function(proc_filepaths_ls,
                                    file_scope,
                                    destfile,
                                    save_results,
                                    verbose) {
  validate_rs <- purrr::imap(
    .x = proc_filepaths_ls,
    .f = function(x, idx) {
      yr <- stringr::str_extract(idx, "[0-9]{4}")
      yr_check <- function(year) {
        function(x) {
          x == year
        }
      }
      
      df <- readr::read_csv(x)
      
      report <- data.validator::data_validation_report()
      
      data.validator::validate(df, name = idx) %>%
        data.validator::validate_cols(yr_check(yr), TAX_YEAR, description = "Accurate Tax Year") %>%
        data.validator::validate_if(assertr::is_uniq(RTRN_ID), description = "Unique IDs") %>%
        data.validator::add_results(report)
      
      result <- report %>% data.validator::get_results(unnest = TRUE)
      
      if (verbose == TRUE) {
        print(report)
      }
      
      return(result)
      
    }
  )
  
  validate_rs <- data.table::rbindlist(validate_rs)
  
  if (save_results == TRUE) {
    destpath <- sprintf("data/validation_outputs/%s", destfile)
    data.table::fwrite(validate_rs, destpath)
    
  }
  
  return(validate_rs)
}