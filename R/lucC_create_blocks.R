#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##  R script to split rasterBrick in blocks in accordance      ##
##  with the number of cells                                   ##
##                                                             ##
##                                             2018-03-06      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Create blocks from RasterBrick in accordance with number of cells
#' @name lucC_create_blocks
#' @aliases lucC_create_blocks
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide a data.frame with main measures to resultd from LUC Calculus application such as: Area (km2),
#' Cumulative Sum (CumSum), Relative Frequency and Cumulative Relative Frequency
#'
#' @usage lucC_create_blocks (raster_obj = NULL, number_cells = 250)
#'
#' @param raster_obj     Raster. A raster stack with classified images
#' @param number_cells   Numeric. A number of cells to split the raster in small parts. Default is 250.
#'
#' @keywords datasets
#' @return List of raster splitted by blocks
#' @importFrom ensurer ensure_that
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_create_blocks(raster_obj = rb_sits2, number_cells = 250)
#'
#'}
#'

lucC_create_blocks <- function(raster_obj = NULL, number_cells = 250){

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj), err_desc = "raster_obj rasterBrick must be defined!\n")
  ensurer::ensure_that(number_cells, !is.null(number_cells),
                       err_desc = "number_cells must be defined! Default is 250 cells\n")

  original_raster <- raster_obj
  # by row
  ii <- seq(1, nrow(original_raster), number_cells)
  # by col
  jj <- seq(1, ncol(original_raster), number_cells)
  # list to store raster
  raster_blocks.list <- list()
  # start first block
  block_id <- 1
  for (i in ii) {
    for (j in jj) {
      raster_blocks.list[[block_id]] <- original_raster[i:(i+(number_cells-1)), j:(j+(number_cells-1)), drop=FALSE]
      block_id <- block_id + 1
    }
  }

  raster_blocks.list

  return(raster_blocks.list)

}
