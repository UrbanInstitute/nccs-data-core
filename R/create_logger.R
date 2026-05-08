#' @title Creates a logger object for writing to a file and the console.
#' @description
#' This function initializes a log4r logger with a console appender and a file appender. The logger is set to the 'INFO' threshold, meaning it will record messages with priority levels of INFO, WARN, ERROR, and FATAL.
#' @param logfile_path Character scalar. The full path to the log file where log
#'   messages will be written.
#' @return An object of class \code{logger} from the \code{log4r} package. This
#'   logger is configured to write to both the console and the specified file.
#'
#' @details
#' The logger uses the default log layout provided by \code{log4r::default_log_layout()}.
#' Log messages are appended to the specified file.
#'
#' @examples
#' \dontrun{
#' my_log <- create_logger("my_application.log")
#' log4r::info(my_log, "Application started successfully.")
#' log4r::error(my_log, "Something went wrong!")
#' }
#'
#' @export
create_logger <- function(logfile_path) {
  my_console_appender = log4r::console_appender(layout = default_log_layout())
  my_file_appender = log4r::file_appender(logfile_path, 
                                          append = TRUE, 
                                          layout = default_log_layout())
  my_logger <- log4r::logger(
    threshold = "INFO",
    appenders = list(my_console_appender, my_file_appender)
  )
  return(my_logger)
}