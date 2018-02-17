#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with predicates holds(o,p,t) and occur(o,p,Te)   ##
##                                                             ##  
##                                             2017-10-04      ##
##                                                             ##
##  J. F. Allen.  end_datewards a general theory of action and ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


#' @title Predicate Allen Holds
#' @name lucC_predicate_holds
#' @aliases lucC_predicate_holds
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a predicate of Allen's which asserts that a property holds 
#' during a time interval. Return a tibble with value within defined interval 
#' 
#' @usage lucC_predicate_holds (locations = NULL, location_properties = NULL, 
#' time_intervals = lucC_interval("2000-01-01", "2004-01-01"))
#' 
#' @param locations           Tibble. A tibble with values longitude and latitude and other values
#' @param location_properties Character. Name of value present in a row of the tibble, such as 'Forest' or other value
#' @param time_intervals      Interval. A interval of time to verify if location_properties is over or 
#' not in lucC_interval format. Given a tibble with values, will be asserts if that location_properties 
#' of locations holds during a time interval. 
#' 
#' @keywords datasets
#' @return Tibble with all events hold during a time interval
#' @importFrom lubridate int_standardize int_start int_end
#' @export
#'
#' @examples \dontrun{
#' 
#' library(lucC)
#' 
#' lucC_starting_point()
#' 
#' data("example_TWDTW")
#' 
#' input_tb_json <- example_TWDTW %>%
#'   lucC_standard_date_events(month_year = "09", day_month = "01")
#' input_tb_json
#' 
#' # example of application
#' time_ex1 <- lucC_interval("2000-09-01", "2003-09-01")
#' time_ex1
#' time_ex2 <- lucC_interval("2003-09-01", "2010-09-01")
#' time_ex2
#' 
#' # location_properties
#' properties <- "Forest"
#' 
#' # example predicate holds
#' lucC_predicate_holds(locations = input_tb_json, 
#' location_properties = "Forest", time_intervals = time_ex1)
#' 
#' lucC_predicate_holds(locations = input_tb_json, 
#' location_properties = properties, time_intervals = time_ex2)
#' 
#'}
#'
#'

# HOLDS(property, time) 
# Asserts that a property holds during a time interval
# version: 2 
# format: holds(o,p,t)
# parameters: o = locations, p = properties of locations and t = time intervals

lucC_predicate_holds <- function(locations = NULL, location_properties = NULL, time_intervals = lucC_interval("2000-01-01", "2004-01-01")){

  if (!is.null(locations) & !is.null(location_properties) & !is.null(time_intervals)) {
    o <- locations
    p <- location_properties
    t <- lubridate::int_standardize(time_intervals)
  } else {
    stop("\nParameters:\n locations (data_df),\n 
         location_properties ('Forest') and\n 
         time_intervals (lucC_interval('2000-01-01', '2004-01-01')),\n 
         must be defined!\n")
  }
  
  intStart <- format(lubridate::int_start(t), format = '%Y-%m-%d')
  intEnd <- format(lubridate::int_end(t), format = '%Y-%m-%d')
  
  df <- o
  df <- df[order(df$end_date),] # order by end_date
  p <- as.character(p)
  
  # verify if properties of location holds
  holds <- function(x){
    row_holds = NULL
    
    if (isTRUE((x[["label"]] == p) & ((x[["start_date"]] >= intStart) & (x[["end_date"]] <= intEnd))))
      row_holds <- x
    
    row_holds
  }
  
  #out.df = data.frame(do.call("rbind", apply(df, 1, holds)))
  allvalues <- apply(df, 1, holds)
  
  if(!is.null(allvalues)){
    #sapply(allvalues,nrow)
    out.df = data.frame(do.call("rbind", allvalues), stringsAsFactors = FALSE)
    
    # only columns importants
    aux.df <- data.frame(longitude  = as.double(out.df$longitude), 
                         latitude   = as.double(out.df$latitude), 
                         start_date = as.Date(out.df$start_date, format = "%Y-%m-%d"), 
                         end_date   = as.Date(out.df$end_date, format = "%Y-%m-%d"), 
                         label      = as.character(out.df$label),
                         id         = as.integer(out.df$id),
                         index      = as.integer(out.df$index), 
                         stringsAsFactors=FALSE)
  } else {
    aux.df <- data.frame()
  }
  
  return(aux.df)
}

# ----------
# old version using foo loop - very slow
#
# lucC_predicate_holds <- function(locations = NULL, location_properties = NULL, time_intervals = lucC_interval("2000-01-01", "2004-01-01")){
#   
#   if (!is.null(locations) & !is.null(location_properties) & !is.null(time_intervals)) {
#     o <- locations
#     p <- location_properties
#     t <- lubridate::int_standardize(time_intervals)
#   } else {
#     stop("\nParameters:\n locations (data_df),\n 
#          location_properties ('Forest') and\n 
#          time_intervals (lucC_interval('2000-01-01', '2004-01-01')),\n 
#          must be defined!\n")
#   }
#   
#   intStart <- format(lubridate::int_start(t), format = '%Y-%m-%d')
#   intEnd <- format(lubridate::int_end(t), format = '%Y-%m-%d')
#   
#   df <- o
#   df <- df[order(df$end_date),] # order by end_date
#   p <- as.character(p)
#   aux.df = df[FALSE,]
#   
#   for (i in 1:nrow(df)) {
#     
#     if ((df$label[i] == p) & ((df$start_date[i] >= intStart) & (df$end_date[i] <= intEnd))) {
#       aux.df <- dplyr::bind_rows(aux.df,df[i,])
#       #cat(sprintf("time: %d in interval: %s -- %s = TRUE \n", i, df$start_date[i], df$end_date[i]))
#     } else {
#       # cat(sprintf("time: %d in interval: %s -- %s = FALSE \n", i, df$start_date[i], df$start_date[i]))
#     }
#     
#   }
#   
#   # if(nrow(aux.df) > 0){
#   #   cat("\nHave been found ", nrow(aux.df)," properties which holds during a time interval.\n")
#   # } else {
#   #   cat("\nAny property have been founded. Alter your location_properties or time_intervals parameters.\n")
#   # }
#   # 
#   return(aux.df)
# }




