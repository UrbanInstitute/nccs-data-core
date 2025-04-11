# Script Header
# Description: This script contains utility functions used in CORE Harmonization 
# Programmer: Thiyaghessan Poongundranar - tpoongundranar@urban.org
# Date Created: 2024-07-25
# Date Last Edited: 2024-07-25

#' @title Function to get the contents of an S3 Bucket
#' @param bucket_name character scalar. Name of S3 Bucket
#' @param bucket_folder character scalar. Folder to get contents from
#' @param bucket_url character scalar. Base url of S3 Bucket

get_s3_bucket_contents <- function( bucket_name, bucket_folder, bucket_url ){
  
  s3 <- paws::s3()
  obj <- s3$list_objects( Bucket = bucket_name )
  keys <- unlist( purrr::map( obj$Contents, purrr::pluck, "Key" ) )
  keys <- keys[ grepl( bucket_folder, keys ) ]
  
  expr <- sprintf( "(?<=%s).*", bucket_folder )
  filenames <- stringr::str_extract( keys,  expr )
  filenames <- filenames[ nchar( filenames ) > 1 ]
  
  s3_urls <- paste0( bucket_url,
                     bucket_folder,
                     filenames )
  
  s3_ls <- as.list( s3_urls )
  names( s3_ls ) <- filenames
  
  return( s3_ls )
  
}

#' @title Function to download raw data to a destination folder
#' @description This function downloads .dat, .csv or .xlsx data to a destination folder and logs errors
#' @param url_ls list. List of URLs (character)
#' @param destfolder character scalar. String indicating destination folder.
#' @param logger logging object. Logger to log failed downloads
#' @return message indicating that download is complete.

download_raw_data <- function(url_ls, destfolder, logger) {
  purrr::map2(
    .x = unlist(url_ls),
    .y = names(url_ls),
    .f = function(x, y) {
      tryCatch({
        message(sprintf("Downloading file: %s", y))
        if (grepl("dat$", y)) {
          df <- readr::read_delim(x)
        }
        else if (grepl("csv$", y)) {
          df <- data.table::fread(x)
        }
        else if (grepl("xlsx$", y)) {
          df <- rio::import(x)
        }
        
      }, warning = function(w) {
        log4r::warn(logger, message = w)
      }, error = function(e) {
        log4r::error(logger, message = sprintf("Failed to download file: %s from %s", y, x))
        log4r::error(logger, message = e)
        
      }, finally = {
        message("Moving to Next File")
        
      })
      
      file_root <- gsub("\\..*", "", y)
      destfile <- paste0(destfolder, file_root, ".csv")
      print(destfile)
      print(head(df))
      rio::export(df, destfile)
      
    },
    .progress = "Download Progress"
  )
  
  return(message("Download Complete"))
  
}

#' @title Function to create a logger
#' @param logfile_path character scalar. Path to logfile.
#' @return log4r logger object
create_logger <- function(logfile_path) {
  my_console_appender = log4r::console_appender(layout = default_log_layout())
  my_file_appender = log4r::file_appender(logfile_path, append = TRUE, layout = default_log_layout())
  
  my_logger <- log4r::logger(
    threshold = "INFO",
    appenders = list(my_console_appender, my_file_appender)
  )
  return(my_logger)
}

#' @title Function to get contents of a local folder containing raw NCCS data.
#' @param folder_name character scalar. Name of folder
#' @param scope character scalar. Form scope.
get_files <- function( folder_name, scope ){
  
  files_hrmn <- list.files( folder_name )[ grepl( scope, list.files( folder_name ) ) ]
  filepaths_ls <- as.list( paste0( folder_name, files_hrmn ) )
  names( filepaths_ls ) <- files_hrmn
  
  return( filepaths_ls )
  
}

#' @title Upload a harmonized data set to aws S3
#' 
#' @description This function takes the url link to a legacy data set hosted on
#' aws s3 and uploads its harmonized counterpart to a new folder in the same bucket
#' 
#' @param file_path character scalar. Path to processed file.
#' @param file_name character scalar. Name of harmonized file
#' @param s3_folder Name of s3 folder to upload data set to
#' 
#' @returns Message indicating that upload is complete
upload_to_s3 <- function(file_path, file_name, s3_folder) {
  s3 <- paws::s3()
  
  bucket_name = "nccsdata"
  key_name = paste0(s3_folder, file_name)
  
  message("Uploading to S3: ", file_name)
  
  s3$put_object(Body = file_path,
                Bucket = bucket_name,
                Key = key_name)
  
  return(message("S3 Upload Complete"))
  
}