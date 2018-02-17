#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with format as tibble the input data             ##
##                                                             ##
##                                             2017-04-18      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Save result as JSON format
#' @name lucC_toJSON
#' @aliases lucC_toJSON
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Save the output data in JSON format in an user's directory
#' @usage lucC_toJSON (data_tb, path_json_file = NULL)
#'
#' @param data_tb        Tibble. A data frame with input values
#' @param path_json_file Character. Name path and file to save JSON file data

#' @keywords datasets
#' @return JSON format to file stored
#' @import magrittr
#' @importFrom ensurer ensure_that
#' @importFrom jsonlite toJSON
#' @importFrom readr write_lines
#' @export
#'
#' @examples \dontrun{
#'
#' library(lucC)
#'
#' lucC_starting_point()
#'
#' file_csv = "./data/example_TWDTW.csv"
#'
#' input_tb_csv <- file_csv %>%
#'   lucC_fromCSV(separator = ",", header_file = TRUE) #%>%
#' input_tb_csv
#'
#' # save the input as json format
#' output_file = "~/Desktop/example_json.json"
#' lucC_toJSON(input_tb_csv, output_file)
#'
#'}
#'

lucC_toJSON <- function (data_tb, path_json_file = NULL) {

  # Ensure if parameters exists
  ensurer::ensure_that(data_tb, !is.null(data_tb),
                       err_desc = "data_tb tibble, must be defined!\n")
  ensurer::ensure_that(path_json_file, !is.null(path_json_file),
                       err_desc = "Enter a path to SAVE your data!")

  input_data <- as.data.frame(data_tb)

  input_data %>%
    jsonlite::toJSON (pretty = TRUE, digits=15) %>%
    readr::write_lines (path_json_file)

  cat("\nFile CSV format saved successfully!\n")

  return (path_json_file)

}


#' @title Open JSON file
#' @name lucC_fromJSON
#' @aliases lucC_fromJSON
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Open the input data in JSON format in an user's directory
#' @usage lucC_fromJSON (path_json_file = NULL)
#'
#' @param path_json_file Character. Name path and file to open JSON file data

#' @keywords datasets
#' @return Open JSON format to file
#' @import magrittr tibble
#' @importFrom ensurer ensure_that
#' @importFrom jsonlite fromJSON
#' @export
#'
#'
#' @examples \dontrun{
#' # Open a data example
#' library(lucC)
#'
#' lucC_starting_point()
#'
#' json_file = "./inst/extdata/patterns/example_TWDTW.json"
#'
#' input_tb_json <- json_file %>%
#'   lucC_fromJSON()
#' input_tb_json
#'
#'}
#'
#'

lucC_fromJSON <- function (path_json_file = NULL) {

  # Ensure if parameters exists
  ensurer::ensure_that(path_json_file, !is.null(path_json_file),
                       err_desc = "Enter with a path to OPEN your data!")

  data_tb <- path_json_file %>%
    jsonlite::fromJSON () %>%
    tibble::as_tibble()

  data_tb

  cat("\nFile JSON format opened successfully!\n")

  return(data_tb)

}


#' @title Open CSV file
#' @name lucC_fromCSV
#' @aliases lucC_fromCSV
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Open the input data in CSV format in an user's directory
#' @usage lucC_fromCSV (path_csv_file = NULL, separator = ",", header_file = TRUE)
#'
#' @param path_csv_file Character. Name path and file to open CSV file data
#' @param separator     Character. Separator to csv, i.e. ',' or ';' ...
#' @param header_file   Character. Header file csv, if TRUE or FALSE, but in this application is necessary have

#' @keywords datasets
#' @return Open a CSV format to file
#' @import magrittr tibble
#' @importFrom ensurer ensure_that
#' @importFrom utils read.csv
#' @export
#'
#' @examples \dontrun{
#' # Open a data example
#' library(lucC)
#'
#' lucC_starting_point()
#'
#' file = "./data/example_TWDTW.csv"
#'
#' input_tb_raw_csv <- file %>%
#'   read.csv(sep = ",", header = TRUE) %>%
#'   lucC_data_preparation()
#' input_tb_raw_csv
#'
#'}
#'
#'

lucC_fromCSV <- function (path_csv_file = NULL, separator = ",", header_file = TRUE) {

  # Ensure if parameters exists
  ensurer::ensure_that(path_csv_file, !is.null(path_csv_file),
                       err_desc = "Enter with a path to OPEN your data!")
  ensurer::ensure_that(separator, !is.null(separator),
                       err_desc = "Define type of separator from CSV file! Default id ','")
  ensurer::ensure_that(header_file, !is.null(header_file),
                       err_desc = "Define if file has or not a header! Default is TRUE")

  data_tb <- path_csv_file %>%
    utils::read.csv (sep = separator, header = header_file, stringsAsFactors = FALSE) %>%
    tibble::as_tibble()

  data_tb

  cat("\nFile CSV format opened successfully!\n")

  return(data_tb)

}

#' @title Example CSV with a time series set
#' @name example_points_ts
#'
#' @description Dataset of points for using with sits_getdata and acquire time series from WTSS. With data from a particular region from Santa Carmem municipality, Mato Grosso, Brazil.
NULL

#' @title Example CSV File
#' @name example_TWDTW
#'
#' @description Dataset of example of a time series set already classified using TWDTW in CSV format to run some functions this package. With data from a particular region from Santa Carmem municipality, Mato Grosso, Brazil.
NULL

#' @title example file of time series set from sits
#' @name example_ts
#'
#' @description Dataset of example of time series set from sits in format RData to run some functions this package. With data from a particular region from Santa Carmem municipality, Mato Grosso, Brazil.
NULL

#' @title example file of a time series set already classified using TWDTW
#' @name example_TWDTW
#'
#' @description Dataset of example of a time series set already classified using TWDTW in RData to run some functions this package. With data from a particular region from Santa Carmem municipality, Mato Grosso, Brazil.
NULL

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



