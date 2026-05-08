#' @title This function is a wrapper for the s3_sync function
#' 
#' @description
#' The function is used to copy data from the Giving Tuesday and NCCS data lake hosted on AWS S3 to the data/raw folder
#' @param src S3 uri for source folder in S3 bucket
#' @param dest Local path to the destination folder
#' 
#' @return NULL
#' 
#' @export
aws_s3_sync <- function(src, dest) {
  status <- system(paste("aws s3 sync", src, dest))
  if (status != 0){
    stop(sprintf("AWS S3 sync failed with exit status %s", status))
  } else {
    message("Download complete")
    return(NULL)
  }
}