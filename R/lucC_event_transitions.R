#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##       R script to discover event transitions                ##
##                                                             ##
##                                             2017-04-18      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title lucC Event Transitions
#' @name lucC_event_transitions
#' @aliases lucC_event_transitions
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a lucC_event_transitions, a set of event transitions. 
#' And return a tibble with events discovered
#'
#' @usage lucC_event_transitions(data_tb = NULL, properties = NULL, 
#' time_intervals = lucC_interval("2000-01-01", "2016-12-31"))
#' 
#' @param data_tb          Tibble. A data frame with input values.
#' @param properties       Character. A vector with at least 2 strings,
#'                         and maximum 10 combination of transitions, 
#'                         for example, c("Forest","Cropping").
#' @param time_intervals   Interval. A interval of time to verify if 
#'                         properties is into of lucC_interval format. 
#' 
#' @keywords datasets
#' @return Tibble with event transition 
#' @importFrom ensurer ensure_that 
#' @importFrom utils head tail 
#' @importFrom lubridate int_standardize int_end int_start
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples \dontrun{
#' 
#' library(lucC)
#' 
#' lucC_starting_point()
#' 
#' data("example_TWDTW")
#' example_TWDTW
#' 
#' example_3.tb <- example_TWDTW %>% 
#'   lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01")
#' 
#' example_3.tb
#' 
#' # p = properties of objects :
#' p1 <- c("Forest", "Pasture", "Single_cropping", "Double_cropping")
#' 
#' # t = interval:
#' t1 <- lucC_interval("2000-08-01","2017-03-01")
#' 
#' tb <- example_3.tb
#' output.tb3 <- tb[FALSE,]
#' coord <- unique(tb$index)
#' 
#' # Apply for each time series based on index
#' for(x in 1:length(coord)){
#'   temp.tb <- tb[which(as.character(tb$index) == coord[x]),]
#'   temp_final.tb <- lucC_event_transitions(temp.tb, properties = p1, 
#'   time_intervals = t1)
#'   output.tb3 <- dplyr::bind_rows(output.tb3, temp_final.tb)
#' }
#' output.tb3
#' 
#' # plots
#' lucC_plot_maps_input(example_3.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, 
#' RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))
#' 
#' lucC_plot_maps_events(output.tb3, EPSG_WGS84 = TRUE, custom_palette = TRUE, 
#' RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, 
#' colour_point = "blue", size_point = 2.3) 
#' 
#' lucC_plot_barplot_events(output.tb3, custom_palette = TRUE, 
#' RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), 
#' pixel_resolution = 250) 
#' 
#' lucC_plot_sequence_events(output.tb3, show_y_index = TRUE, 
#' end_date = "2017-03-01", custom_palette = TRUE, 
#' RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 
#' 
#'
#' }

# function to apply event transtion
lucC_event_transitions <- function(data_tb = NULL, properties = NULL, time_intervals = lucC_interval("2000-01-01","2016-12-31")){
  
  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb), 
                       err_desc = "data_tb tibble, must be defined!\n")
  ensurer::ensure_that(properties, !is.null(properties) & (length(properties)>=2 & length(properties)<=10) & 
                         is.character(properties), 
                       err_desc = "Properties must be character type and have at least 2 strings and maximum 10.")
  
  if (!is.null(time_intervals)) {
    t <- lubridate::int_standardize(time_intervals)
  } else {
    cat("time_intervals (lucC_interval(\"2000-01-01\", \"2004-01-01\")),\n 
         must be defined!\n")
  }
  
  interval_start <- format(lubridate::int_start(t), format = '%Y-%m-%d')
  interval_end <- format(lubridate::int_end(t), format = '%Y-%m-%d')
  
  # limit time to tibble data_tb
  data_tb <- data_tb[which(data_tb$start_date >= interval_start & data_tb$end_date <= interval_end),]
  
  
  # create a set of variables with each string from vector stored 
  for(i in 1:length(properties)){
    assign(paste0("properties_",i), as.character(properties[i]))
  }
  
  # start transition
  start_transition <- function(count, interval.tb, aux.tb){
    count <- 1
    aux.tb <- state_0(count, interval.tb, aux.tb)
    aux.tb
  }
  
  # state 0
  state_0 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(all(interval.tb$label[count] != properties) && count != nrow(interval.tb)){
        count <- count+1
        aux.tb <- state_0(count, interval.tb, aux.tb)
      } else if(interval.tb$label[count] == properties_1 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_1(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 1, to first transition (or string)
  state_1 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_1 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_1(count, interval.tb, aux.tb)
      } else if(interval.tb$label[count] == properties_2 && count <= nrow(interval.tb)){
        aux.tb <- state_2(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 2, to second transition (or string)
  state_2 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_2 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_2(count, interval.tb, aux.tb)
      } else if (all(exists("properties_3") == TRUE && interval.tb$label[count] == properties_3 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_3(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 3, is applied if the third string exists
  state_3 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_3 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_3(count, interval.tb, aux.tb)
      } else if (all(exists("properties_4") == TRUE && interval.tb$label[count] == properties_4 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_4(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 4, is applied if the fourth string exist
  state_4 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_4 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_4(count, interval.tb, aux.tb)
      } else if (all(exists("properties_5") == TRUE && interval.tb$label[count] == properties_5 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_5(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 5, is applied if the fifth string exist
  state_5 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_5 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_5(count, interval.tb, aux.tb)
      } else if (all(exists("properties_6") == TRUE && interval.tb$label[count] == properties_6 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_6(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 6, is applied if the sixth string exist
  state_6 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_6 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_6(count, interval.tb, aux.tb)
      } else if (all(exists("properties_7") == TRUE && interval.tb$label[count] == properties_7 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_7(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 7, is applied if the seventh string exist
  state_7 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_7 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_7(count, interval.tb, aux.tb)
      } else if (all(exists("properties_8") == TRUE && interval.tb$label[count] == properties_8 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_8(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 8, is applied if the eighth string exist
  state_8 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_8 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_8(count, interval.tb, aux.tb)
      } else if (all(exists("properties_9") == TRUE && interval.tb$label[count] == properties_9 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_9(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 9, is applied if the ninth string exist
  state_9 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_9 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_9(count, interval.tb, aux.tb)
      } else if (all(exists("properties_10") == TRUE && interval.tb$label[count] == properties_10 && count <= nrow(interval.tb)) == TRUE){
        aux.tb <- state_10(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
      
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # stage 10, is applied if the tenth string exist
  state_10 <- function(count, interval.tb, aux.tb){
    if(count <= nrow(interval.tb)){
      if(interval.tb$label[count] == properties_10 && count <= nrow(interval.tb)){
        aux.tb <- rbind(aux.tb, interval.tb[count,])
        count <- count+1
        aux.tb <- state_10(count, interval.tb, aux.tb)
      } else
        aux.tb <- state_error(aux.tb)
    } else 
      aux.tb <- state_error(aux.tb)
    
    aux.tb
  }
  
  # case transition doesn't exists, compute the tibble auxiliar
  state_error <- function(aux.tb){
    aux.tb
  }
  
  # create a new tibble with the same column names from input tibble
  aux.tb <- output.tb <- data_tb[FALSE,]
  interval.tb <- data_tb
  count <- 0
  
  # start the event transition
  aux.tb <- start_transition(count, interval.tb, aux.tb)
  
  # create a lucC interval with information from start and end date from input tibble
  interval <- lucC_interval(utils::head(data_tb$start_date,1),utils::tail(data_tb$end_date,1))
  
  logical <- NULL
  # verify if all properties are TRUE
  if(nrow(aux.tb)>= 1) { # if tibble have more than one row
    
    # convert column with label to a unique string
    str <- paste(aux.tb$label, sep = "", collapse = " ")
    
    # verify if exist label in the same order of input: "Pasture -> Cropping -> Pasture -> Cropping"
    if(length(properties) == 2 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, ""), str)))!= 0){
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 3 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                ".+", properties_3, ""), str)))!= 0){
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 4 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                ".+", properties_3, ".+", properties_4, ""), str)))!= 0){
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 5 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                ".+", properties_3, ".+", properties_4, 
                                                                                ".+", properties_5, ""), str)))!= 0){  
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 6 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                ".+", properties_3, ".+", properties_4, 
                                                                                ".+", properties_5, ".+", properties_6, ""), str)))!= 0){  
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 7 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                ".+", properties_3, ".+", properties_4, 
                                                                                ".+", properties_5, ".+", properties_6, 
                                                                                ".+", properties_7, ""), str)))!= 0){  
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 8 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                ".+", properties_3, ".+", properties_4, 
                                                                                ".+", properties_5, ".+", properties_6, 
                                                                                ".+", properties_7, ".+", properties_8, ""), str)))!= 0){  
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 9 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                ".+", properties_3, ".+", properties_4, 
                                                                                ".+", properties_5, ".+", properties_6, 
                                                                                ".+", properties_7, ".+", properties_8, 
                                                                                ".+", properties_9, ""), str)))!= 0){  
      logical <- cbind(logical,"TRUE")
    } else if (length(properties) == 10 && length(regmatches(str, regexpr(paste0("+", properties_1, ".+", properties_2, 
                                                                                 ".+", properties_3, ".+", properties_4, 
                                                                                 ".+", properties_5, ".+", properties_6, 
                                                                                 ".+", properties_7, ".+", properties_8, 
                                                                                 ".+", properties_9, ".+", properties_10, ""), str)))!= 0){  
      logical <- cbind(logical,"TRUE")
    } else{
      logical <- cbind(logical,"FALSE")
    }

    if(all(logical == TRUE)) 
      output.tb <- dplyr::bind_rows(output.tb, aux.tb)
    else {
      aux.tb <- NULL
      output.tb <- dplyr::bind_rows(output.tb, aux.tb)
    }
  } else {
    aux.tb <- NULL
    output.tb <- dplyr::bind_rows(output.tb, aux.tb)
  }
  
  output.tb <- output.tb[order(output.tb$index),] # order by index
  
  output.tb
  
}      

