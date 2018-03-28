#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to replace pixels in a ReasterBrick              ##
##                                                             ##
##                                             2018-03-02      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Update a RasterBrick with pixel replaced
#' @name lucC_raster_update
#' @aliases lucC_raster_update
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Update a RasterBrick with new values of pixel discovered from LUC Calculus formalism
#'
#' @usage lucC_raster_update(raster_obj = NULL, data_mtx = NULL,
#' timeline = NULL, class_to_replace = NULL, new_pixel_value = 20)
#'
#' @param raster_obj       Raster. A raster stack with classified images
#' @param data_mtx         Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param timeline         Character. A list of all dates of classified raster, timeline
#' @param class_to_replace Character. All labels of each value of pixel from classified raster
#' @param new_pixel_value  Integer. New pixel value to raster. Default is 20
#'
#' @keywords datasets
#' @return Matrix with raster and new pixel to create a RasterBrick reclassified
#' @importFrom ensurer ensure_that
#' @importFrom lubridate year
#' @importFrom dplyr mutate  select left_join
#' @importFrom tidyr gather spread
#' @importFrom stats na.omit
#' @export
#'
#' @examples \dontrun{
#'
#' rb_new <- lucC_raster_update(raster_obj = rb_sits, data_mtx = third_raster.df,
#' timeline = timeline, class_to_replace = "Forest", new_pixel_value = 6)
#' rb_new
#'
#'}
#'

# plot maps for input data
lucC_raster_update <- function(raster_obj = NULL, data_mtx = NULL, timeline = NULL, class_to_replace = NULL, new_pixel_value = 20) {

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj),
                       err_desc = "raster_obj tibble, file must be defined!\nThis data can be obtained using lucC predicates holds or occurs.")
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(timeline, !is.null(timeline),
                       err_desc = "timeline must be defined!")
  ensurer::ensure_that(class_to_replace, !is.null(class_to_replace),
                       err_desc = "class_to_replace must be defined!")

  #-------------------- prepare rasterBrick --------------------------------
  # original raster
  df <- raster::rasterToPoints(raster_obj) %>%
    data.frame()

  rm(raster_obj)
  gc()

  # replace colnames to timeline
  colnames(df)[c(3:ncol(df))] <- as.character(lubridate::year(timeline))
  #raster_df <- reshape2::melt(df, id.vars = c("x","y"))
  raster_df <- df %>%
    tidyr::gather(variable, value, -x, -y)

  rm(df)
  gc()

  # remove factor
  # raster_df$variable = as.character(levels(raster_df$variable))[raster_df$variable]

  #-------------------- prepare matrix with events --------------------------------
  # data matrix to new raster
  new_df <- as.data.frame(data_mtx)
  colnames(new_df)[c(3:ncol(new_df))] <- as.character(lubridate::year(colnames(new_df)[c(3:ncol(new_df))]))

  rm(data_mtx)
  gc()

  # replace new clase by new pixel value
  new_df[c(3:ncol(new_df))] <- ifelse(new_df[c(3:ncol(new_df))] == class_to_replace, new_pixel_value, "")

  # point_df <- reshape2::melt(new_df, id.vars = c("x","y")) %>%
  #   stats::na.omit()
  point_df <- new_df %>%
    tidyr::gather(variable, value, -x, -y) %>%
    stats::na.omit()

  # remove factors
  point_df$x = as.numeric(as.character(point_df$x)) # as.numeric(levels(point_df$x))[point_df$x]
  point_df$y = as.numeric(as.character(point_df$y))
  point_df$variable = as.character(as.character(point_df$variable))

  rm(new_df)
  gc()
  # ------------------ replace point_df in raster_df ---------------------

  # change original by new values - ok
  raster_df_temp0 <- base::merge(raster_df, point_df, by = c("x","y","variable")) %>%
    dplyr::mutate(value = .$value.y) %>%
    dplyr::select(-value.x, -value.y) %>%
    .[order(.$variable),]

  rm(point_df)
  gc()

  # replace in entire raster
  raster_df_temp <- dplyr::left_join(raster_df, raster_df_temp0, by = c("x" = "x", "y" = "y", "variable" = "variable")) %>%
    dplyr::mutate(value = ifelse(!is.na(.$value.y), .$value.y, .$value.x)) %>%
    dplyr::select(-value.x, -value.y) %>%
    .[order(.$variable),]

  rm(raster_df, raster_df_temp0)
  gc()

  # remove duplicated lines
  raster_df_temp <- raster_df_temp[!duplicated(raster_df_temp), ]

  #raster_df_update <- reshape2::dcast(raster_df_temp, x+y ~ variable, value.var= "value")
  raster_df_update <- raster_df_temp %>%
    tidyr::spread(variable, value)

  colnames(raster_df_update)[c(3:ncol(raster_df_update))] <- as.character(timeline)

  rm(raster_df_temp)
  gc()

  return(raster_df_update)

}





