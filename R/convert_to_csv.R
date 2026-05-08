#' @title This function converts .xlsx and .dat files to .csv
#' 
#' 
xlsx_to_csv <- function(path){
  files <- list.files(path, full.names = TRUE)
  xlsx_paths <- files[grepl("\\.xlsx$", files)]
  for (path in xlsx_paths){
    # Read the xlsx file
    message("Reading xlsx file: ", path)
    df <- readxl::read_xlsx(path)
    
    # Create a new file name with .csv extension
    csv_path <- paste0(tools::file_path_sans_ext(path), ".csv")
    
    # Write the data to a CSV file
    write.csv(data, csv_path)
    message("Saved as .csv to: ", csv_path)
    # Optionally, remove the original xlsx file
    file.remove(path)
  }
}