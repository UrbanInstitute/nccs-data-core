# Install roxygen2 from CRAN
install.packages("roxygen2")
library(roxygen2)
#' Harmonize Legacy IRS PF Data Using a Crosswalk
#' 
#' This functions reads a legacy IRS Private Foundations (PF) dataset, 
#' and standardizes column names
#'  using a provided crosswalk. 
#'  It also provides keys steps for completing the harmonization process
#'  including steps for logging duplicate columns, creating a tax year,
#'  and creating a EIN2 column that ensures digits are not dropped from the EINs
#' @param path. File path to the legacy PF dataset
#' @param xwalk_pf A cross walk table created with old column names ('VAR_NAME_OLD),
#' and the names they will be changed to ('VAR_NAME_NEW')
#' @param logger A log4r object used to log messages during the harmonization process
#' 
#' @return A data table containing the darmonized dataset with standardized column names,
#' it will also include two additional columns (Tax Year and EIN2).
#' Tax year column is found using the tax period end dates from the PF dataset
#' EIN2 column is created using the format_ein function
#' 
#' @details
#' The function performs the following steps:
#' \ itemize{
#'    \item Extracts the calendar year for the legacy core/soi dataset.
#'    \item Reads the file into data.table object and coverts column names to uppercase
#'    \item Filters the crosswalk to only include names found in the legacy_colnames
#'    \item Logs the unharmonized and harmonized columns
#'    \item Performs harmonization using the crosswalk
#'    \item Logs duplicate columns (if any)
#'    \item Creates Tax Year and EIN2 columns
#'    \item Logs outputs}
#' 
harmonize_pf <- function(path, xwalk_pf, logger){
  
  # Extract the calendar year for the legacy core/soi dataset
  year <- stringr::str_extract(path, "(19|20)\\d{2}")
  if (is.na(year)){year <- paste0("20", stringr::str_extract(path, "\\d{2}"),
                                  collapse = "")}
  
  # Read file into data.table object
  pf_legacy_dt <- data.table::fread(path)
  setNames(pf_legacy_dt, toupper(names(pf_legacy_dt)))
  
  # Get column names
  legacy_colnames <- names(pf_legacy_dt)
  
  # Create xwalk sample
  xwalk_sample <- xwalk_pf |>
    dplyr::filter(VAR_NAME_OLD %in% legacy_colnames,
                  ! is.na(VAR_NAME_NEW),
                  VAR_NAME_NEW != "")
  
  # Get columnn names that are absent
  unharmonized_cols <- setdiff(legacy_colnames, xwalk_sample$VAR_NAME_OLD)
  
  # Data Quality Check
  #Check if all necessary columns are present
  required_columns <- c("F9_00_FISCAL_YEAR_END", "F9_00_TAX_PERIOD_END_DATE",
                        "F9_00_ORG_EIN")
  missing_requirecols <- setdiff(required_columns, legacy_colnames)
  
  if(length(missing_requirecols) > 0) {
    log4r::info(logger,message = paste("Missing essential columns in dataset:",
                                       paste(missing_requirecols, collapse = ", ")))
  }
  
  #Check for missing EINs
  if("F9_00_ORG_EIN" %in% legacy_colnames) {
    missing_ein_count <- sum(is.na(pf_legacy_dt$F9_00_ORG_EIN) |
                               pf_legacy_dt$F9_00_ORG_EIN == "")
    if(missing_ein_count > 0) {
      log4r::info(logger,message = paste("There are",
                                         paste(missing_ein_cols, "missing EINs in the dataset.")))
    }
  }
  
  # Perform harmonization
  harmonizable_cols <- unique(xwalk_sample$VAR_NAME_OLD)
  pf_legacy_sample <- pf_legacy_dt[, ..harmonizable_cols]
  data.table::setnames(pf_legacy_sample,
                       xwalk_sample$VAR_NAME_OLD,
                       xwalk_sample$VAR_NAME_NEW
  )
  
  # Duplicated column names - need to log
  dup_columns <- names(pf_legacy_sample)[duplicated(names(pf_legacy_sample))]
  
  # Create column containing tax year
  if ("F9_00_FISCAL_YEAR_END" %in% harmonizable_cols){
    pf_legacy_sample[, TAX_YEAR := substr(F9_00_FISCAL_YEAR_END, 1, 4)]
  } else {
    pf_legacy_sample[, TAX_YEAR := substr(F9_00_TAX_PERIOD_END_DATE, 1, 4)]
  }
  
  # Create EIN2 Column with check
  pf_legacy_sample[, EIN2 := format_ein(F9_00_ORG_EIN, to="id")]
  
  # Log outputs
  log4r::info(logger, message = paste("Year", year))
  log4r::info(logger, message = paste("Unharmonized cols:", paste0(unharmonized_cols, collapse = ", ")))
  log4r::info(logger, message = paste("Harmonized cols:", paste0(harmonizable_cols, collapse = ", ")))
  log4r::info(logger, message = paste("Duplicated cols:", paste0(dup_columns, collapse = ", ")))
  
  return(pf_legacy_sample)
}