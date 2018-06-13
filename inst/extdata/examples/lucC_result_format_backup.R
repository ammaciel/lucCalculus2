#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with to result format to create bar plots        ##
##                                                             ##
##                                             2018-02-27      ##
##                                                             ##
##                                                             ##
#################################################################


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
  #longLatFromRaster.df

  #return(longLatFromRaster.df)

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
  #holds_raster.df

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


