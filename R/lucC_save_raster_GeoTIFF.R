#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to save raster in GeoTIFF format                 ##
##                                                             ##
##                                             2018-03-01      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Save a RasterBrick reclassified in Folder
#' @name lucC_save_GeoTIFF
#' @aliases lucC_save_GeoTIFF
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Save new_raster reclassified in a diretory defined by user
#'
#' @usage lucC_save_GeoTIFF(raster_obj = NULL, data_mtx = NULL, path_raster_folder = NULL)
#'
#' @param raster_obj          Raster. A raster stack with classified images
#' @param data_mtx            Matrix. A matrix with pixel replaced, can be obtined using lucC_update_raster()
#' @param path_raster_folder  Character. Name a path folder to save raster images data. If  doesn't exist, a new directory is created
#'
#' @keywords datasets
#' @return Raster in geotiff format to open using SIG
#' @importFrom ensurer ensure_that
#' @importFrom raster rasterFromXYZ nlayers writeRaster
#' @importFrom utils txtProgressBar
#' @export
#'
#' @examples \dontrun{
#'
#' # save rasters in folder
#' lucC_save_GeoTIFF (raster_obj = rb_sits, data_mtx = new_raster, path_raster_folder = "~/Desktop/raster")
#'
#'}
#'

# plot maps with events
lucC_save_GeoTIFF <- function(raster_obj = NULL, data_mtx = NULL, path_raster_folder = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj),
                       err_desc = "raster_obj tibble, file must be defined!\nThis data can be obtained using lucC predicates holds or occurs.")
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT and lucC_update_raster().")
  ensurer::ensure_that(path_raster_folder, !is.null(path_raster_folder),
                       err_desc = "path_raster_folder must be defined! Enter a path to SAVE your GeoTIFF images!")

  # Create directory if doesn't exist
  output_dir <- file.path(path_raster_folder)

  if (!dir.exists(output_dir)){
    message("\nCreated new directory because the path provided doesn't exist! ...\n")
    dir.create(output_dir)
  } else {
    path_raster_folder <- path_raster_folder
  }

  # matrix reclassified to raster format
  new_raster <- raster::rasterFromXYZ(data_mtx)
  names(new_raster) <- raster_obj@data@names

  # crs from original raster
  raster::crs(new_raster) <- raster_obj@crs

  message("Saving... \n")

  # write it as a geoTIFF file using the raster package
  raster::writeRaster(new_raster, filename= paste0(path_raster_folder,"/New", sep = ""), bylayer=TRUE, suffix = names(new_raster), format="GTiff", overwrite=TRUE)

  cat("\nGeoTIFF images saved successfully in directory: '", path_raster_folder, "'\n")

}



