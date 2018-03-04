#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to remove columns of matrix                      ##
##                                                             ##
##                                             2018-03-04      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Save tibble as Raster in Folder
#' @name lucC_remove_columns
#' @aliases lucC_remove_columns
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Remove columns from a data_mtx
#'
#' @usage lucC_remove_columns(data_mtx = NULL, name_columns = NULL)
#'
#' @param data_mtx      Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param name_columns  Character. Name of columns to remove from data set
#'
#' @keywords datasets
#' @return Matrix with without selected columns
#' @importFrom ensurer ensure_that
#' @export
#'
#' @examples \dontrun{
#'
#' # save RasterBrick in path
#' lucC_create_RasterBrick (path_open_GeoTIFFs = "~/Desktop/raster", path_save_RasterBrick = "~/Desktop")
#'
#'}
#'

# plot maps with events
lucC_remove_columns <- function(data_mtx = NULL, name_columns = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(name_columns, !is.null(name_columns),
                       err_desc = "name_columns must be defined! Enter names of columns to remove!")
  #
  message(paste(c("Columns to remove: \n", name_columns, "\n"), collapse="\n"))

  # columns to delete
  drops <- name_columns
  data_mtx_new <- data_mtx[ , !(names(data_mtx) %in% drops)]

  return(data_mtx_new)

}
