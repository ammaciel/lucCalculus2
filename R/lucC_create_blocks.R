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
#' @description Provide a data.frame with spatially aggregats the original raster it turns each aggregated cell into a polygon then the extent of each polygon is used to crop the original raster. \url{https://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples}
#' @usage lucC_create_blocks (raster_obj = NULL, number_blocks_xy = 6, save_images = TRUE)
#'
#' @param raster_obj        Raster. A raster stack with classified images
#' @param number_blocks_xy  Numeric. A number of peaces to split raster, consider 2 peaces the raster will be cropped into 4 blocks. Default is 6
#' @param save_images       Boolean. Write raster in file. Default is TRUE
#'
#' @keywords datasets
#' @return List with all the pieces in case you want to keep them in the memory.
#' @importFrom ensurer ensure_that
#' @importFrom raster aggregate ncell rasterToPolygons extent crop writeRaster
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_create_blocks(raster_obj = rb_sits2, number_blocks_xy = 6, save_images = TRUE)
#'
#'}
#'

lucC_create_blocks <- function(raster_obj = NULL, number_blocks_xy = 6, save_images = TRUE){

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj), err_desc = "raster_obj rasterBrick must be defined!\n")
  ensurer::ensure_that(number_blocks_xy, !is.null(number_blocks_xy),
                       err_desc = "number_blocks_xy must be defined! Default is 6 by x and y, or 36 blocks\n")

  h <- ceiling(ncol(raster_obj)/number_blocks_xy)
  v <- ceiling(nrow(raster_obj)/number_blocks_xy)
  agg <- raster::aggregate(raster_obj, fact = c(h,v))
  agg[] <- 1:raster::ncell(agg)
  agg_poly <- raster::rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()

  # crop orgiinal raster by extent from poligon
  for(i in 1:raster::ncell(agg)){
    e <- raster::extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- raster::crop(raster_obj,e)
  }

  # save images in directory
  if(save_images==TRUE){
    for(i in 1:length(r_list)){
      raster::writeRaster(r_list[[i]], filename=paste("Raster_Splitted_", i, sep=""),
                              format="GTiff", datatype="INT1U", overwrite=TRUE)
    }
  }

  path <- getwd()
  message("\nRaster splitted saved in path ", path, "\n")

}

# lucC_create_blocks <- function(raster_obj = NULL, number_cells = 250){
#   # Ensure if parameters exists
#   ensurer::ensure_that(raster_obj, !is.null(raster_obj), err_desc = "raster_obj rasterBrick must be defined!\n")
#   ensurer::ensure_that(number_cells, !is.null(number_cells),
#                        err_desc = "number_cells must be defined! Default is 250 cells\n")
#
# original_raster <- raster_obj
# # by row
# ii <- seq(1, nrow(original_raster), number_cells)
# # by col
# jj <- seq(1, ncol(original_raster), number_cells)
# # list to store raster
# raster_blocks.list <- list()
# # start first block
# block_id <- 1
# for (i in ii) {
#   for (j in jj) {
#     raster_blocks.list[[block_id]] <- original_raster[i:(i+(number_cells-1)), j:(j+(number_cells-1)), drop=FALSE]
#     block_id <- block_id + 1
#   }
# }
#
# raster_blocks.list
#
# return(raster_blocks.list)


#' @title Create blocks from RasterBrick in accordance with number of cells
#' @name lucC_merge_blocks
#' @aliases lucC_merge_blocks
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Merge GeoTIFF splitted into parts. \url{https://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples}
#' @usage lucC_merge_blocks (path_open_GeoTIFFs = NULL, number_raster = 4,
#' pattern_name = NULL, is.rasterBrick = FALSE)
#'
#' @param path_open_GeoTIFFs   Character. Name a path folder to OPEN raster images data.
#' @param number_raster        Integer. Number of GeoTIFF files.
#' @param pattern_name         Character. A pattern in name of GeoTIFF to mosaic them
#' @param is.rasterBrick       Boolean. If TRUE GeoTIFF is a RasterBrick, FALSE is a single layer. Default is FALSE
#'
#' @keywords datasets
#' @return RasterBrick Mosaic.
#' @importFrom ensurer ensure_that
#' @importFrom raster brick mosaic writeRaster
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_merge_blocks(path_open_GeoTIFFs = NULL, number_raster = 4,
#'                   pattern_name = "MT_year_", is.rasterBrick = FALSE)
#'
#'}
#'

lucC_merge_blocks <- function(path_open_GeoTIFFs = NULL, number_raster = 4, pattern_name = NULL, is.rasterBrick = FALSE){

 # Ensure if parameters exists
  ensurer::ensure_that(path_open_GeoTIFFs, !is.null(path_open_GeoTIFFs),
                       err_desc = "path_open_GeoTIFFs file, must be defined, without last /! Enter a path to OPEN your GeoTIFF images.")

  message("Verifying if GeoTIFF image exist ...")
  # all files in folder
  all.files <- list.files(path_open_GeoTIFFs, full.names = TRUE, pattern = paste0(pattern_name, "[0-9]\\.tif$", sep = ""))
  if( length(all.files) > 0){
    cat(all.files, sep = "\n")
  } else
    stop("There is no path or pattern_name file!\n")

  # read each piece back in R
  list <- list()
  if(isTRUE(is.rasterBrick)){
    for(i in 1:number_raster){ # change this 9 depending on your number of pieces
      rx <- raster::brick(paste0(path_open_GeoTIFFs,"/", pattern_name, i,".tif", sep=""))
      list[[i]] <- rx
    }
  } else if(is.rasterBrick == FALSE){
    for(i in 1:number_raster){ # change this 9 depending on your number of pieces
      rx <- raster::raster(paste0(path_open_GeoTIFFs,"/", pattern_name, i,".tif", sep=""))
      list[[i]] <- rx
    }
  }

  message("\nFiles will be merged:\n")
  # mosaic them and save output
  list$fun <- max
  rast.mosaic <- do.call(raster::mosaic, list)

  message("Start merge files:\n")
  raster::writeRaster(rast.mosaic, filename = paste0(path_open_GeoTIFFs,"/Mosaic_", pattern_name, sep=""),
              format="GTiff", datatype="INT1U", overwrite=TRUE)

}





