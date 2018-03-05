#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##  R script to compute Frequqncy, Area, CumSum,               ##
##  Freqquency relative and Frequency Relative CumSum          ##
##                                                             ##
##                                             2018-03-04      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Plot Sequence Maps with lucC Events
#' @name lucC_result_measures
#' @aliases lucC_result_measures
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a data.frame with main measures to resultd from LUC Calculus application such as: Area (km2),
#' Cumulative Sum (CumSum), Relative Frequency and Cumulative Relative Frequency
#'
#' @usage lucC_result_measures (data_mtx = NULL, pixel_resolution = 250)
#'
#' @param data_mtx         Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param pixel_resolution Numeric. Is a spatial resolution of the pixel. Default is 250 meters considering MODIS 250 m. See more at \url{https://modis.gsfc.nasa.gov/about/specifications.php}.
#'
#' @keywords datasets
#' @return Data frame with measures computed
#' @import ggplot2
#' @importFrom ensurer ensure_that
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate bind_cols bind_cols
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_result_measures(data_mtx = NULL, pixel_resolution = NULL)
#'
#'}
#'

lucC_result_measures <- function(data_mtx = NULL, pixel_resolution = 250){

  # Ensure if parameters exists
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(pixel_resolution, !is.null(pixel_resolution),
                       err_desc = "pixel_resolution must be defined! Default is 250 meters on basis of MODIS image")

  # to data frame
  input_data <- reshape2::melt(as.data.frame(data_mtx), id = c("x","y"))

  # count number of values
  dataMeasures.df <- data.frame(table(lubridate::year(input_data$variable), input_data$value)) %>%
    dplyr::mutate(Area_km2 = (.$Freq*(pixel_resolution*pixel_resolution))/(1000*1000)) # Area

  colnames(dataMeasures.df)[c(1:3)] <- c("Years", "Classes", "Pixel_number")

 # compute absolute frequency
  dataMeasures.df <- dataMeasures.df %>%
    dplyr::group_by(Classes) %>%
    dplyr::mutate(CumSum = cumsum(Area_km2)) %>% # rev_area
    as.data.frame() %>%
    dplyr::bind_cols()

  # compute relative frquency for each label
  dataMeasures.df <- dataMeasures.df %>%
    dplyr::group_by(Classes) %>%
    dplyr::mutate(Relative_Frequency = (Area_km2/max(CumSum))*100) %>%
    as.data.frame() %>%
    dplyr::bind_cols()

  # compute relative frequency cumsum for each label
  dataMeasures.df <- dataMeasures.df %>%
    dplyr::group_by(Classes) %>%
    dplyr::mutate(Cumulative_Relative_Frequency = cumsum(Relative_Frequency)) %>%
    as.data.frame() %>%
    dplyr::bind_cols()

  dataMeasures.df

  return(as.data.frame(dataMeasures.df))

}

