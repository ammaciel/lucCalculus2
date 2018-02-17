#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with predicates holds(o,p,t) with raster         ##
##                                                             ##
##                                             2018-02-16      ##
##                                                             ##
##  J. F. Allen.  end_datewards a general theory of action and ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


#' @title Predicate Allen Holds
#' @name lucC_pred_holds
#' @aliases lucC_pred_holds
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a predicate of Allen's which asserts that a class holds
#' during a time interval. Return a tibble with value within defined interval
#'
#' @usage lucC_pred_holds (raster_obj = NULL, raster_class = NULL,
#' time_interval = c("2000-01-01", "2004-01-01"),
#' relation_interval = "contains", label = NULL, timeline = NULL)
#'
#' @param raster_obj        Raster. A raster stack with classified images
#' @param raster_class      Character. Name of class present in a row of the tibble, such as 'Forest' or other value
#' @param time_interval     Interval. A interval of time to verify if class is over or
#'                          not in character format. Given a tibble with values, will be asserts if that
#'                          class of locations holds during a time interval
#' @param relation_interval Character. If a location holds during all time interval 'equals' or can be appear in any
#'                          times 'contains'. Default is 'contains'
#' @param label             Character. All labels of each value of pixel of classified raster
#' @param timeline          Date. A list with all end_dates of classified raster
#'
#' @keywords datasets
#' @return Tibble with all events hold during a time interval
#' @import magrittr
#' @importFrom lubridate int_standardize int_start int_end as_date ymd years
#' @importFrom tibble as_tibble
#' @importFrom raster subset rasterToPoints
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples \dontrun{
#'
#' library(lucC)
#'
#'}
#'

# HOLDS(property, time)
# Asserts that a class holds during a time interval
# version: 3
# format: holds(o,c,t)
# parameters: o = locations, c = class of locations and t = time intervals
lucC_pred_holds <- function(raster_obj = NULL, raster_class = NULL, time_interval = c("2000-01-01", "2004-01-01"), relation_interval = "contains", label = NULL, timeline = NULL){

  if (!is.null(raster_obj) & !is.null(raster_class) & !is.null(label) & !is.null(timeline)) {
    rasterStack_obj <- raster_obj
    class_name <- as.character(raster_class)
    label <- label
    timeline <- timeline
  } else {
    stop("\nParameters:\n raster_obj (raster.stack),\n
         raster_class ('Forest') and must be defined!\n
         final_result = TRUE or FALSE\n")
  }

  if (!is.null(time_interval)) {

    # checking if first or second interval values are correct
    t <- lucC_interval(time_interval[1],time_interval[2]) %>%
      lubridate::int_standardize()

    # define time interval initial
    date_start <- match(lubridate::as_date(format(lubridate::int_start(t), format = '%Y-%m-%d')), timeline)

    # define time interval final
    date_end <- match(lubridate::as_date(format(lubridate::int_end(t), format = '%Y-%m-%d')), timeline)

  } else {
    stop("\nParameters:\n
         time_interval = c('2000-01-01', '2004-01-01') must be defined!\n")
  }

  # relation Allen CONTAINS or EQUALS
  if (!is.null(relation_interval) & (relation_interval == "equals" | relation_interval == "contains")) {
    relation_allen <- relation_interval
  }else{
    stop("\nInvalide option: 'equals' or 'contains' must be defined!\n")
  }

  # define values to query
  class <- match(class_name, label)

  # subset with all lcoations from raster holds during a time interval
  .holds_raster <- function(ras.obj, class.ras, start_date.ras, end_date.ras) {
    temp <- raster::subset(ras.obj, start_date.ras:end_date.ras)
    output <- temp == class.ras
    return(output)
  }

  # apply holds_raster to obtain results
  output_holds <- .holds_raster(rasterStack_obj, class, date_start, date_end) # rt
  #raster::plot(output_holds)

  longLatFromRaster <- NULL
  # extract x, y, and values from raster output_holds
  longLatFromRaster <- raster::rasterToPoints(output_holds)
  #longLatFromRaster <- raster::as.data.frame(output_holds, xy = TRUE)

  if (relation_allen == "equals" & ncol(longLatFromRaster) > 3) {
    longLatFromRaster.df <- longLatFromRaster[
      rowSums(longLatFromRaster[,c(3:ncol(longLatFromRaster))] &
                !is.na(longLatFromRaster[,c(3:ncol(longLatFromRaster))])) == length(3:ncol(longLatFromRaster)),]
  } else if (relation_allen == "contains" & ncol(longLatFromRaster) > 3)  {
    longLatFromRaster.df <- longLatFromRaster[
      rowSums(longLatFromRaster[,c(3:ncol(longLatFromRaster))] &
                !is.na(longLatFromRaster[,c(3:ncol(longLatFromRaster))])) > 0,]
  } else if ((relation_allen == "equals" | relation_allen == "contains") & ncol(longLatFromRaster) == 3){
    longLatFromRaster.df <- longLatFromRaster[!(is.na(longLatFromRaster[, 3]) | longLatFromRaster[, 3] == 0), ]
  }

  # longLatFromRaster.df
  # nrow(longLatFromRaster.df)

  ## test rows entire FALSE values
  # dplyr::anti_join(longLatFromRaster,longLatFromRaster.df)

  # define timeline from raster output_holds
  timeline_holds = timeline[ timeline >= timeline[date_start] & timeline <= timeline[date_end]]

  # alter column names of data.frame
  colnames(longLatFromRaster.df)[c(3:ncol(longLatFromRaster.df))] <- as.character(timeline_holds)
  longLatFromRaster.df

  # alter label for original value in character
  longLatFromRaster.df[,c(3:ncol(longLatFromRaster.df))] <-
    as.character(ifelse(longLatFromRaster.df[,c(3:ncol(longLatFromRaster.df))] == 1, class_name, NA))

  longLatFromRaster.df

  return(longLatFromRaster.df)
}



#' @title Predicate Allen Holds
#' @name lucC_pred_recur
#' @aliases lucC_pred_recur
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a predicate of Allen's which asserts that a class holds
#' during a time interval. Return a tibble with value within defined interval
#'
#' @usage lucC_pred_recur (raster_obj = NULL, raster_class = NULL,
#' time_interval1 = c("2001-01-01", "2001-01-01"),
#' time_interval2 = c("2003-01-01", "2004-01-01"),
#' label = NULL, timeline = NULL)
#'
#' @param raster_obj        Raster. A raster stack with classified images
#' @param raster_class      Character. Name of class present in a row of the tibble, such as 'Forest' or other value
#' @param time_interval1    Interval. A interval of time to verify if class is over or
#'                          not in character format. Given a tibble with values, will be asserts if that
#'                          class of locations holds during a time interval
#' @param time_interval2    Interval. A interval of time to verify if class is over or
#'                          not in character format. Given a tibble with values, will be asserts if that
#'                          class of locations holds during a time interval
#' @param label             Character. All labels of each value of pixel of classified raster
#' @param timeline          Date. A list with all end_dates of classified raster
#'
#' @keywords datasets
#' @return Tibble with all events hold during a time interval
#' @import magrittr
#' @importFrom lubridate int_standardize int_start int_end as_date ymd years
#' @importFrom tibble as_tibble
#' @importFrom raster subset rasterToPoints
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples \dontrun{
#'
#' library(lucC)
#'
#'}
#'

# HOLDS(property, time)
# Asserts that a class holds during a time interval
# version: 3
# format: holds(o,c,t)
# parameters: o = locations, c = class of locations and t = time intervals

lucC_pred_recur <- function(raster_obj = NULL, raster_class = NULL, time_interval1 = c("2001-01-01", "2001-01-01"), time_interval2 = c("2003-01-01", "2004-01-01"), label = NULL, timeline = NULL){

  if (!is.null(raster_obj) & !is.null(raster_class) & !is.null(label) & !is.null(timeline)) {
    rasterStack_obj <- raster_obj
    class_name <- as.character(raster_class)
    label <- label
    timeline <- timeline
  } else {
    stop("\nParameters:\n raster_obj (raster.stack),\n
         raster_class ('Forest') and must be defined!\n
         final_result = TRUE or FALSE\n")
  }

  # first time interval
  if (!is.null(time_interval1)) {
    # checking if first or second interval values are correct
    time_interval1 <- time_interval1
   } else {
    stop("\nParameters:\n
         time_interval1 = c('2000-01-01', '2004-01-01') must be defined!\n")
  }

  # second time interval
  if (!is.null(time_interval2)) {
    # checking if first or second interval values are correct
    time_interval2 <- time_interval2
   } else {
    stop("\nParameters:\n
         time_interval2 = c('2000-01-01', '2004-01-01') must be defined!\n")
  }

  res1 <- lucC_pred_holds(raster_obj = rasterStack_obj, raster_class = class_name,
                          time_interval = c(time_interval1[1],time_interval1[2]), relation_interval = "contains",
                          label = label, timeline = timeline)
  res2 <- lucC_pred_holds(raster_obj = rasterStack_obj, raster_class = class_name,
                          time_interval = c(time_interval2[1],time_interval2[2]), relation_interval = "contains",
                          label = label, timeline = timeline)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if( nrow(res1) > 0  & nrow(res2) > 0  & nrow(result <- lucC_relation_before(res1, res2)) > 0 ) {
    result <- result[!duplicated(result), ]
    return(result)
  } else {
    stop("\nRelation RECUR cannot be applied!\n")
  }

}




#' @title Predicate Allen Holds
#' @name lucC_result_format
#' @aliases lucC_result_format
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a predicate of Allen's which asserts that a class holds
#' during a time interval. Return a tibble with value within defined interval
#'
#' @usage lucC_result_format (raster_df = NULL)
#'
#' @param raster_df  Matrix or Dataframe. A data set from predicate operation
#'
#' @keywords datasets
#' @return Tibble with all states
#' @import magrittr
#' @importFrom lubridate as_date ymd years
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples \dontrun{
#'
#' library(lucC)
#'
#'}
#'

lucC_result_format <- function(raster_df = NULL){

  # check if data is empty
  if (!is.null(raster_df)) {
    longLatFromRaster.df <- raster_df
  } else {
    stop("\nData set cannot be empty!\n")
  }

  # extract timeline from colnames
  timeline_holds <- colnames(longLatFromRaster.df)[c(3:ncol(longLatFromRaster.df))]

  # information about number of lines
  longLatFromRaster.df <- cbind(longLatFromRaster.df[,1:2],
                                id = 1:nrow(longLatFromRaster.df),
                                longLatFromRaster.df[,3:ncol(longLatFromRaster.df)])
    # empty data.frame
  holds_raster.df <- NULL

  # extract values from longLatFromRaster.df to a data.frame format lucC
  .data.df <- function(x, nreps = length(timeline_holds)){
    label_idx <- 4:NROW(x)
    longitude <- x[1]
    latitude <- x[2]
    start_date <- as.character(lubridate::ymd(timeline_holds) - lubridate::years(1))
    end_date <- as.character(lubridate::ymd(timeline_holds))
    label <- as.character(x[label_idx])
    id <- 0
    index <- as.numeric(x[3])

    out <- data.frame(longitude = rep(longitude, times = nreps),
                      latitude = rep(latitude, times = nreps),
                      start_date = start_date,
                      end_date = end_date,
                      label = label,
                      id = rep(id, times = nreps),
                      index = index,
                      stringsAsFactors = FALSE)
    return(out)

  }

  holds_raster.df <- do.call("rbind", apply(longLatFromRaster.df, 1, .data.df))
  holds_raster.df

  # create data.frame with results
  output_holds_raster.df <- data.frame(longitude  = as.numeric(as.character(holds_raster.df$longitude)),
                                       latitude   = as.numeric(as.character(holds_raster.df$latitude)),
                                       start_date = lubridate::as_date(as.character(holds_raster.df$start_date)),
                                       end_date   = lubridate::as_date(as.character(holds_raster.df$end_date)),
                                       label      = as.character(holds_raster.df$label),
                                       id         = as.numeric(1:nrow(holds_raster.df)),
                                       index      = as.numeric(as.character(holds_raster.df$index)),
                                       stringsAsFactors = FALSE)

  ## remove factor
  # str(output_holds_raster.df)

  # create tibble as result
  output_holds_raster.tb <- tibble::as_tibble(output_holds_raster.df)
  #head(output_holds_raster.tb)

  # remove location without values == NA
  output_holds_raster.tb <- output_holds_raster.tb %>%
    tidyr::drop_na()

  # remove duplicated value rows
  output_holds_raster.tb <- output_holds_raster.tb[!duplicated(output_holds_raster.tb), ]

  # put an id as a number line
  output_holds_raster.tb$id <- as.numeric(1:nrow(output_holds_raster.tb))

  output_holds_raster.tb

  return(output_holds_raster.tb)

}



