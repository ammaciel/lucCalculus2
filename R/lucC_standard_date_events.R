
#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to define a stardard date to extract events      ##
##                                                             ##  
##                                             2017-04-18      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title lucC Apply TWDTW from SITS Format
#' @name lucC_standard_date_events
#' @aliases lucC_standard_date_events
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Create two new columns in tibble with standard date in 
#' order to extract events
#' 
#' @usage lucC_standard_date_events(data_tb = NULL, 
#' month_year = "09", day_month = "01")
#' 
#' @param data_tb      Tibble. A tibble with values in sits format
#' @param month_year   Character. A month of the year in two digits (01 to 12)
#' @param day_month    Character. A day of the month in two digits (01 to 31). 
#' Depends the character calendar month. 
#' 
#' @keywords datasets
#' @return Tibble containing two new columns with start_date and end_date in a 
#' predefined standard range ensurer
#' @importFrom ensurer ensure_that
#' @importFrom lubridate year
#' @export
#'
#' @examples \dontrun{
#' 
#' library(lucC)
#' 
#' data("example_TWDTW")
#' example_TWDTW
#' 
#' # alter start_date and end_date to a especific range in 
#' # order to extract events
#' example_1.tb <- example_TWDTW %>% 
#'   lucC_standard_date_events(data_tb = ., 
#'   month_year = "09", day_month = "01")
#' 
#' example_1.tb
#' 
#'}
#'

lucC_standard_date_events <- function(data_tb = NULL, month_year = "09", day_month = "01"){
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, file must be defined!")
  ensurer::ensure_that(month_year, !is.null(month_year), 
                       err_desc = "month_year must be defined! Default is '08', August.")
  ensurer::ensure_that(day_month, !is.null(day_month), 
                       err_desc = "day_month date must be defined! Default is day '15'.")
  
  input_data <- data_tb 
  
  # create new columns with an uniform date
  input_data$"start_date2" <- as.Date(input_data$start_date, format = '%Y-%m-%d')   
  input_data$start_date <- as.Date(paste0(lubridate::year(input_data$start_date),"-", month_year,"-", day_month, sep = ""), format = '%Y-%m-%d') 
  
  input_data$"end_date2" <- as.Date(input_data$end_date, format = '%Y-%m-%d')
  input_data$end_date <- as.Date(paste0(lubridate::year(input_data$end_date),"-", month_year,"-", day_month, sep = ""), format = '%Y-%m-%d') 
  
  return(input_data)
  
}

