#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with standard format to interval                 ##
##                                                             ##
##                                             2017-02-26      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title lucC Intervals
#' @name lucC_interval
#' @aliases lucC_interval
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a lucC_interval, an input pattern format used by Allen's relations. And return a value of interval
#'
#' @usage lucC_interval(first_date , second_date)
#' 
#' @param first_date      Date. An date value like '2012-03-02'.
#' @param second_date     Date. An date value like '2013-03-02'.
#' 
#' @keywords datasets
#' @return Interval value with two dates 
#' @importFrom ensurer ensure_that 
#' @importFrom lubridate interval ymd
#' @export
#'
#' @examples \dontrun{
#' 
#' library(lucC)
#' 
#' lucC_starting_point()
#' 
#' # create some examples of intervals
#' time1 <- lucC_interval("2011-09-01","2011-10-01")
#' time2 <- lucC_interval("2011-09-15","2011-11-01")
#'
#'
#'}
#'
#'

# Transform two dates in an interval
lucC_interval <- function (first_date, second_date) {
  
  # Ensure if parameters exists
  ensurer::ensure_that(first_date, !is.null(first_date), 
                       err_desc = "Date must be defined!")
  ensurer::ensure_that(second_date, !is.null(second_date), 
                       err_desc = "Date must be defined!")

  # test if a valid date
  pattern = c('[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]')
  
  if (!is.null(grepl(pattern, first_date) == TRUE) & !is.null(grepl(pattern, second_date) == TRUE)){
     
    lubridate::interval(lubridate::ymd(first_date), lubridate::ymd(second_date))
    
  } else {
    cat("\nEnter with a date in format 'year-month-day' = '2010-01-02'\n")
  }
    
}


