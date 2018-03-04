#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to load GeoTIFF and saved as RasterBrick         ##
##                                                             ##
##                                             2018-03-04      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Save tibble as Raster in Folder
#' @name lucC_create_RasterBrick
#' @aliases lucC_create_RasterBrick
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Create a RasterBrick from list of GeoTIFF images stored in a folder
#'
#' @usage lucC_create_RasterBrick(path_open_GeoTIFFs = NULL, path_save_RasterBrick = NULL)
#'
#' @param path_open_GeoTIFFs     Character. Name a path folder to OPEN raster images data.
#' @param path_save_RasterBrick  Character. Name a path folder to SAVE RasterBrick from GeoTIFF images. If  doesn't exist, a new directory is created
#'
#' @keywords datasets
#' @return RasterBrick file
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
lucC_create_RasterBrick <- function(path_open_GeoTIFFs = NULL, path_save_RasterBrick = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(path_open_GeoTIFFs, !is.null(path_open_GeoTIFFs),
                       err_desc = "path_open_GeoTIFFs tibble, must be defined! Enter a path to OPEN your GeoTIFF images.")
  ensurer::ensure_that(path_save_RasterBrick, !is.null(path_save_RasterBrick),
                       err_desc = "path_save_RasterBrick must be defined! Enter a path to SAVE your GeoTIFF images!")

  # create a rasterBrick with data
  GeoTIFF_files <- list.files(path_open_GeoTIFFs,
                              full.names = TRUE,
                              pattern = ".tif$")
  # order files by digits
  numbers = as.numeric(regmatches(GeoTIFF_files, regexpr("[0-9]+", GeoTIFF_files)))
  GeoTIFF_files <- GeoTIFF_files[order(numbers)]

  message(paste(c("GeoTIFF images: \n", GeoTIFF_files, "\n"), collapse="\n"))

  name_RasterBrick <- basename(path_open_GeoTIFFs)

  # Create directory if doesn't exist
  output_dir <- file.path(path_save_RasterBrick)

  if (!dir.exists(output_dir)){
    message("\nCreated new directory because the path provided doesn't exist! ...\n")
    dir.create(output_dir)
  } else {
    path_save_RasterBrick <- path_save_RasterBrick
  }

  message("...")

  # save RasterBrick
  build_RasterBrick <- GeoTIFF_files %>%
    raster::stack(.) %>%
    raster::brick(.) %>%
    raster::writeRaster(., paste0(path_save_RasterBrick,"/",name_RasterBrick,".tif", sep = ""), overwrite=TRUE)

  message("RasterBrick saved in path: ", paste0(path_save_RasterBrick,"/",name_RasterBrick,".tif", sep = ""),"\n")

}

