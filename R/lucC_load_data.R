#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script extra input data                                 ##
##                                                             ##
##                                             2018-02-26      ##
##                                                             ##
##                                                             ##
#################################################################


#' @importFrom utils globalVariables
utils::globalVariables(c("points_input_map.list", "map_input_df", "points_events_map.list"))


#' @title Example CSV with a timeline
#' @name timeline_SVM_mt
#'
#' @description CSV file format of example of a timelines already classified using classification method.
NULL


#' @title timeline with all dates of the classified image
#' @name timeline_SVM_mt
#'
#' @description Dataset of example of a timelines already classified using classification method.
NULL



#' @title Format data as tibble format
#' @name lucC_starting_point
#' @aliases lucC_starting_point
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Change the global default setting to not auto-convert to factors
#'
#' @usage lucC_starting_point()
#'
#' @keywords datasets
#' @return Change the global default setting
#' @export
#'
#' @examples \dontrun{
#'
#' library(lucC)
#'
#' lucC_starting_point()
#'
#'}
#'
#'
#'
lucC_starting_point <- function(){

  # change the global default setting to not auto-convert to factors
  options(stringsAsFactors = FALSE)

  # digits: controls the number of digits to print when printing numeric values.
  options(digits=15)

  #  cat("\nChange the global default setting successfully. Now, will not auto-convert to factors!\n")

  #   cat("\n  #########################################################################\n
  #    A input data must to have seven columns names called:\n
  #    'longitude','latitude','start_date','end_date','label','id','index'.\n
  #    The 'index' is an unique value for each set of longitude and latitude equals,\n
  #    with start_date and end_date different, i.e, one pixel over time.\n
  #    The 'id' is a continuos number, like a row line.\n
  #   #########################################################################\n\n")
}



#' @title Format data as tibble format
#' @name lucC_data_preparation
#' @aliases lucC_data_preparation
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a format pattern to input data and make operations with Allen's relations and predicates. And return a tibble with values input
#' @usage lucC_data_preparation(data_tb = NULL)
#'
#' @param data_tb  Dataframe. A time series file.
#'
#' @keywords datasets
#' @return Tibble with columns 'longitude', 'latitude', 'start_date', 'end_date', 'label', 'id', 'index'
#' @import magrittr tibble
#' @importFrom ensurer ensure_that
#' @export
#'
#' @examples \dontrun{
#'
#' library(lucC)
#'
#' lucC_starting_point()
#'
#' # open a CSV example
#' file = "./inst/example_csv_file.csv"
#'
#' # read a raw csv and turn into format lucC with
#' input_tb_raw_csv <- file %>%
#'   read.csv(sep = ",", header = TRUE) %>%
#'   lucC_data_preparation()
#' input_tb_raw_csv
#'
#' new_tb <- lucC_data_preparation(input_tb)
#'
#'}
#'
#'


lucC_data_preparation <- function(data_tb = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb),
                       err_desc = "data_tb tibble, file must be defined!")

  input_data <- data_tb

  column_names <- c("longitude", "latitude", "start_date", "end_date", "label", "id", "index")

  # verifiy if columns names are equal to format columns names need
  if(all(colnames(input_data) %in% column_names) == TRUE){
    cat("\nColumns names from input data are OK!\n")

    # change data format to tibble
    input_tb <- tibble(longitude  = double(),
                       latitude   = double(),
                       start_date = character(),
                       end_date   = character(),
                       label      = character(),
                       id         = numeric(),
                       index      = numeric())

    input_tb <- tibble::add_row(input_tb, longitude  = input_data$longitude,
                                latitude   = input_data$latitude,
                                start_date = input_data$start_date,
                                end_date   = input_data$end_date,
                                label      = input_data$label,
                                id         = input_data$id,
                                index      = input_data$index)

    input_tb %>%
      tibble::as_tibble()

    return(input_tb)

    # if the input file have different name columns, a message will be showed to change the file
  } else {
    stop("Please! Your input data column names need to have seven columns names called:\n
         'longitude','latitude','start_date','end_date','label','id','index'\n")
  }

}


