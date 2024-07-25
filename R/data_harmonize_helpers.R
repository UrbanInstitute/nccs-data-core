# Script Header
# Description: This script contains helper functions for 01_data_harmonize.R 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25

#' @title Run harmonization workflow for Files
#' 
#' @description This function takes URLs to unharmonized files organized by calendar year 
#' and performs variable harmonization based on new variable names in the crosswalk file. 
#' It either harmonizes all files or only harmonizes those that are not present in the 
#' destination bucket. It then saves files according to tax year as defined by 
#' the user, taking the first four characters as the YYYY tax year. 
#' 
#' @param raw_paths paths to raw data.
#' @param logger log4r object. Logs error messages for debugging
#' @param xwalk_df data.frame. Variable crosswalk
#' @param ds character scalar. Data series ("CORE", "SOI" or "BMF")
#' @param scope character scalar. Form scope to harmonize
#' @param tax_year_column character scalar. Name of column containing Tax Year
#' @param destfolder character scalar. Destination folder for harmonized files
#' @param tax_years character vector. Tax years for coverage. Default = NULL uses
#' tax years found in data set.
#' 
#' @returns message indicating harmonization is complete

run_harmonization <- function(raw_paths,
                              logger,
                              xwalk_df,
                              ds,
                              scope,
                              tax_year_column,
                              destfolder,
                              tax_years = NULL) {
  dat_hrmn <- purrr::map(raw_paths, data.table::fread, .progress = "Loading Raw Data")
  
  dat_hrmn <- purrr::map(dat_hrmn,
                         harmonize_data,
                         crosswalk_df = xwalk_df,
                         .progress = "Variable Harmonization")
  
  dat_hrmn <- data.table::rbindlist(dat_hrmn, fill = TRUE)
  
  dat_lazy <- dtplyr::lazy_dt(dat_hrmn)
  tax_period <- dat_hrmn[[tax_year_column]]
  dat_hrmn[, TAX_YEAR := substr(tax_period, 1, 4)]
  tax_period_years <- unique(dat_hrmn[["TAX_YEAR"]])
  
  if (!is.null(tax_years)) {
    tax_period_years <- intersect(tax_years, tax_period_years)
  }
  
  message("Filtering by Tax Year")
  
  for (year in tax_period_years) {
    dat_yr <- dat_lazy %>%
      dplyr::filter(.data[["TAX_YEAR"]] == year) %>%
      dplyr::mutate(F9_00_ORG_EIN = as.character(F9_00_ORG_EIN)) %>%
      select_if(~ !all(is.na(.)))
    
    df_yr <- as.data.frame(dat_yr)
    
    file_name <- sprintf("%s-%s-%s-HRMN.csv", ds, year, scope)
    destfile <- paste0(destfolder, file_name)
    
    result = tryCatch({
      message(sprintf("Saving Tax Year %s for Scope %s", year, scope))
      
      rio::export(df_yr, destfile)
      
      message("Finished Processing File")
      
    }, warning = function(w) {
      log4r::warn(logger, message = w)
      
    }, error = function(e) {
      log4r::error(logger,
                   message = sprintf("Failed to process tax year %s for scope % s", year, scope))
      log4r::error(logger, message = e)
      
    }, finally = {
      message("Moving to Next File")
      
    })
    
  }
  
  return(message("Harmonization Complete"))
}

#' @title Use xwalk to retrieve harmonized variables
#'
#' @description This function identifies legacy variables in the crosswalk that are
#' both present in the input legacy data set and have been assigned new names. It then
#' subsets the crosswalk accordingly.
#'
#' @param dat data.table object. Data.table of legacy core file.
#' @param xwalk_df data.frame object. data.frame of crosswalk file
#'
#' @returns subsetted crosswalk data.frame

get_harmonizable_vars <- function(dat, xwalk_df) {
  harmonized_vars_df <- xwalk_df %>%
    dplyr::filter(
      !is.na(var_new),!var_new %in% c("NOT IN DD", "NOT IN CONCORDANCE"),
      var_old %in% names(dat)
    ) %>%
    dplyr::select(var_old, var_new) %>%
    dplyr::distinct()
  
  return(harmonized_vars_df)
  
}

#' @title Use xwalk to harmonize a legacy core data set
#' 
#' @description This function renames variables in a legacy data set using a 
#' crosswalk file and filters out unmapped columns from the legacy data set.
#' 
#' @param dat data.table object. Data.table of legacy core file.
#' @param crosswalk_df data.frame object. data.frame of crosswalk file
#' 
#' @returns harmonized legacy data.table

harmonize_data <- function(dat, crosswalk_df) {
  data.table::setnames(dat, toupper(names(dat)))
  
  harmonizable_vars_df <- get_harmonizable_vars(dat, crosswalk_df)
  harmonizable_cols <- unique(harmonizable_vars_df$var_old)
  dat <- dat[, ..harmonizable_cols]
  
  data.table::setnames(
    dat,
    harmonizable_vars_df$var_old,
    make_unique_names(harmonizable_vars_df$var_new)
  )
  
  return(dat)
  
}

#' @title Append suffixes to repeat column names
#' 
#' @description This function takes checks for duplicate elements in the 
#' character vector and appends a suffix to make them unique
#' 
#' @param input_vector character vector.
#' 
#' @returns same vector with suffixes appended to previously duplicated elements

make_unique_names <- function(input_vector) {
  counts <- table(input_vector)
  duplicate_indices <- which(counts[input_vector] > 1)
  
  for (index in duplicate_indices) {
    duplicate_elements <- input_vector == input_vector[index]
    if (sum(duplicate_elements) > 1) {
      indices_to_rename <- which(duplicate_elements)
      
      for (i in 2:length(indices_to_rename)) {
        input_vector[indices_to_rename[i]] <- paste(input_vector[index], i, sep = "_")
      }
    }
  }
  
  return(input_vector)
}
