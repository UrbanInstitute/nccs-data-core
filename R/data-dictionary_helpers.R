# Script Header
# Description: This script contains helper functions for creating the data dictionary in 05_data-dictionary.R 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25

# Global Variables
CONCORDANCE_URL <- "https://raw.githubusercontent.com/Nonprofit-Open-Data-Collective/irs-efile-master-concordance-file/master/concordance.csv"
CONCORDANCE_DF <- readr::read_csv(CONCORDANCE_URL)

NCCS_VARS_DF <- data.frame(
  variable_name = c("TAX_YEAR", "EIN2", "DUP_RTRN_X", "MISSION_NTEE", "GEO_ZIP5"),
  description = c(
    "Tax Year",
    "Reformatted EIN",
    "Indicates duplicate return",
    "NTEE Codes",
    "5-digit Zip Code"
  ),
  location_code = rep(NA, 5),
  variable_scope = rep(NA, 5)
)


#' @title Function to create data dictionary for multiple form scopes belonging to single data series
#' @description maps single create_data_dictionary() call to a named list of processed files
#' @param proc_files_scope_ls named list mapping form scopes (scalar) to paths to processed files belonging to that form scope (vector)
#' @param save_results boolean. TRUE == save results to folder in destfile
#' @param destfile character scalar. Path to save data dictionary to. Default == NULL
#' @return data.frame object containing data dictionay

create_master_data_dictionary <- function(proc_files_scope_ls,
                                          save_results,
                                          destfile = NULL) {
  dds <- purrr::imap(proc_files_scope_ls, create_data_dictionary, .progress = "Data Dictionary Progress")
  
  master_dd <- data.table::rbindlist(dds)
  
  if (save_results == TRUE) {
    data.table::fwrite(master_dd, destfile)
  }
  
  return(master_dd)
  
}

#' @title Function to create a data dictionary for a single form scope
#' 
#' @description This function takes in URLs to .csv files for a set of harmonized
#' files belonging to a single form scope and creates a data dictionary based
#' on the variables in the master concordance file.
#' 
#' @param paths character vector. Path to .csv files in harmonize/data/processed/*
#' belonging to a specific form scope
#' @param scope character scalar. Form scope the URLs are pointing to.
#' @param concordance_df data.frame object. Concordance file
#' 
#' @return data.frame containing the data dictionary

create_data_dictionary <- function(paths, scope, concordance_df = CONCORDANCE_DF) {
  variable_header_ls <- purrr::map(
    paths,
    .f = function(x) {
      df <- data.table::fread(x, colClasses = list(character = c("F9_00_ORG_EIN")))
      datatypes <- sapply(df, class)
      return(datatypes)
    }
  )
  
  names(variable_header_ls) <- stringr::str_extract(paths, "[0-9]{4}")
  variable_names <- unique(unlist(sapply(variable_header_ls, names)))
  
  dd <- concordance_df %>%
    dplyr::filter(variable_name %in% variable_names) %>%
    dplyr::select("variable_name",
                  "description",
                  "location_code",
                  "variable_scope")
  
  dd <- dplyr::bind_rows(dd, NCCS_VARS_DF) %>%
    dplyr::distinct() %>%
    dplyr::group_by(variable_name, ) %>%
    dplyr::summarise(
      variable_description = unique(description)[1],
      variable_source = paste(unique(variable_scope), collapse = ";\n"),
      form_location = paste(unique(location_code), collapse = ";\n")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      variable_coverage = get_coverage(var_name = variable_name, variable_names_dic = variable_header_ls),
      form_scope = scope,
      variable_datatype = get_datatype (var_name = variable_name, variable_names_dic = variable_header_ls)
    )
  
  data.table::fwrite(dd, paste0(scope, "_dd.csv"))
  
  return(dd)
  
}

#' @title Function to get the coverage of tax years for which a variable
#' in the data dictionary has coverage for.
#' 
#' @param var_name character scalar. Name of variable to find coverage for
#' @param variable_names_dic list. Named list mapping data set names to the variables
#' present in each data set
#' 
#' @return character scalar of tax years seperated by ;

get_coverage <- function(var_name, variable_names_dic) {
  years <- names(variable_names_dic)[sapply(variable_names_dic, function(x)
    var_name %in% names(x))]
  
  years <- ifelse(length(years) > 1,
                  format_years_range(years),
                  paste(years, collapse = ";"))
  
  return(years)
  
}

#' @title Function to get data type belonging to a single variable
#' @param var_name character scalar. Name of variable to find coverage for
#' @param variable_names_dic list. Named list mapping data set names to the variables
#' present in each data set
#' @return unique datatypes of variable in each data set

get_datatype <- function(var_name, variable_names_dic) {
  datatypes <- sapply(variable_names_dic, function(x)
    x[names(x) == var_name])
  datatypes <- datatypes[!grepl("\\(0\\)", datatypes)]
  datatypes <- paste(unique(datatypes), collapse = ";")
  
  return(datatypes)
  
}

#' @title Function to format date range
#' 
#' @description This function takes in a character vector of years and
#' formats it to group sequential years together for readability.
#' 
#' @param years character vector of years
#' 
#' @return formatted date range

format_years_range <- function(years) {
  # Convert the character vector of years to numeric
  years_numeric <- as.numeric(years)
  
  # Sort the years
  years_sorted <- sort(years_numeric)
  
  # Initialize variables to store sequential and non-sequential years
  sequential_years <- NULL
  non_sequential_years <- NULL
  
  # Loop through the sorted years to identify sequential and non-sequential
  current_seq_start <- years_sorted[1]
  
  for (i in 1:(length(years_sorted) - 1)) {
    if (years_sorted[i] + 1 == years_sorted[i + 1]) {
      current_seq_end <- years_sorted[i + 1]
    } else {
      if (current_seq_start == years_sorted[i]) {
        non_sequential_years <- c(non_sequential_years, current_seq_start)
      } else {
        sequential_years <- c(sequential_years,
                              paste0(current_seq_start, "-", current_seq_end))
      }
      
      current_seq_start <- years_sorted[i + 1]
    }
  }
  
  # Handle the last element
  if (current_seq_start == years_sorted[length(years_sorted)]) {
    non_sequential_years <- c(non_sequential_years, current_seq_start)
  } else {
    sequential_years <- c(sequential_years,
                          paste0(current_seq_start, "-", years_sorted[length(years_sorted)]))
  }
  
  # Convert non-sequential years to character
  non_sequential_years <- as.character(non_sequential_years)
  
  # Combine sequential and non-sequential years
  result <- c(sequential_years, non_sequential_years)
  
  # Join them with a ";" separator
  formatted_result <- paste(result, collapse = "; ")
  
  return(formatted_result)
}
