#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to plot input data                               ##
##                                                             ##
##                                             2018-02-28      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Plot Input Maps
#' @name lucC_plot_maps_input
#' @aliases lucC_plot_maps_input
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot map ggplot2 for all input data
#'
#' @usage lucC_plot_maps_input (raster_obj = NULL, EPSG_WGS84 = TRUE,
#' custom_palette = FALSE, RGB_color = NULL, plot_ncol = NULL,
#' relabel = FALSE, original_labels = NULL, new_labels = NULL)
#'
#' @param raster_obj         Tibble. A tibble with values longitude and latitude and other values
#' @param EPSG_WGS84      Character. A reference coordinate system. If TRUE, the values of latitude and longitude alredy use this coordinate system, if FALSE, the data set need to be transformed
#' @param custom_palette  Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE
#' @param RGB_color       Character. A vector with color names to map legend, for example, c("Green","Blue"). Default is setting scale_colour_hue
#' @param plot_ncol       Numeric. A number of columns to show the maps
#' @param relabel         Boolean. A TRUE or FALSE value. If TRUE, user will provide its own legend text setting! Default is FALSE
#' @param original_labels Character. A vector with original labels from legend text, for example, c("Forest","Pasture").
#' @param new_labels      Character. A vector with new labels to legend text, for example, c("Mature_Forest","Pasture1").
#'
#' @keywords datasets
#' @return Plot with input data as colored map
#' @import ggplot2 magrittr
#' @importFrom ensurer ensure_that
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr filter
#' @importFrom lubridate year
#' @importFrom sp proj4string CRS spTransform coordinates
#' @export
#'
#' @examples \dontrun{
#'
#' library(lucC)
#'
#' lucC_starting_point()
#'
#' # open a JSON file example
#' file_json = "./inst/extdata/patterns/example_TWDTW.json"
#'
#' # open file JSON
#' input_tb_raw_json <- file_json %>%
#'   lucC_fromJSON()
#' input_tb_raw_json
#'
#' # plot maps input data
#' lucC_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE,
#' custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f",
#' "#929e6e", "#f5e7a1"))
#'
#'
#'}
#'

# plot maps for input data
lucC_plot_maps_input <- function(raster_obj = NULL, EPSG_WGS84 = TRUE, custom_palette = FALSE, RGB_color = NULL, plot_ncol = NULL, relabel = FALSE, original_labels = NULL, new_labels = NULL) {

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj),
                       err_desc = "raster_obj tibble, file must be defined!\nThis data can be obtained using lucC predicates holds or occurs.")
  ensurer::ensure_that(EPSG_WGS84, !is.null(EPSG_WGS84),
                       err_desc = "EPSG_WGS84 must be defined, if exists values of longitude and latitude (TRUE ou FALSE)! Default is TRUE")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(relabel, !is.null(relabel),
                       err_desc = "relabel must be defined, if wants use its own legend text setting! Default is FALSE")

  #ensurer::ensure_that(RGB_color, custom_palette == TRUE & is.character(RGB_color),
  #                    err_desc = "RGB_color must be defined, if custom_palette equals TRUE, then provide a list of colors with the same length its number of legend! Default is the color brewer 'Paired'")
                      # & (length(RGB_color) == length(unique(raster_obj$label)))

  input_data <- raster_obj

  # create points
  .createPoints(input_data, EPSG_WGS84)

  a <- data.frame(Reduce(rbind, points_input_map.list))

  rownames(a) <- NULL
  a <- data.frame(a) %>% dplyr::filter(a$w != "NA")
  a$x <- as.integer(a$x)
  a$y <- as.integer(a$y)
  a$w <- as.factor(a$w)
  a$z <- as.factor(a$z)
  map_input_df <- NULL
  map_input_df <- a

  map_input_df <- map_input_df[order(map_input_df$w),] # order by years
  rownames(map_input_df) <- seq(length=nrow(map_input_df)) # reset row numbers

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(raster_obj$label))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      my_palette = RGB_color
    }
  } else {
    # more colors
    colour_count = length(unique(map_input_df$z))
    my_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(name="Paired", n = 12))(colour_count)
  }

  original_leg_lab <- base::levels(droplevels(map_input_df$z))
  cat("Original legend labels: \n", original_leg_lab, "\n")

  # insert own legend text
  if(relabel == TRUE){
    if(is.null(original_labels) | length(new_labels) != length(unique(map_input_df$z)) |
       all(original_labels %in% original_leg_lab) == FALSE){
      cat("\nIf relabel = TRUE, a vector with original labels must be defined!")
      cat("\nProvide a list of original labels and new labels with the same length of the legend!\n")
    } else {
      my_original_label = original_labels
      my_new_labels = new_labels
    }
  } else {
    # my legend text
    my_original_label = unique(map_input_df$z)
    my_new_labels = unique(map_input_df$z)
  }

  # plot images all years
  g <- ggplot2::ggplot(map_input_df, aes(map_input_df$x, map_input_df$y)) +
        geom_raster(aes_string(fill=map_input_df$"z")) +
        scale_y_continuous(expand = c(0, 0), breaks = NULL) +
        scale_x_continuous(expand = c(0, 0), breaks = NULL) +
        facet_wrap("w", ncol = plot_ncol) +
        coord_fixed(ratio = 1) +
        # coord_fixed(ratio = 1/cos(mean(map_input_df$x)*pi/180)) +
        theme(legend.position = "bottom") +#, strip.text = element_text(size=10)) +
        xlab("") +
        ylab("") +
        scale_fill_manual(name="Legend:", values = my_palette, breaks = my_original_label, labels = my_new_labels)
        # scale_fill_brewer(name="Legend:", palette= "Paired")

  print(g)

  map_input_df <<- map_input_df

}


# create points
.createPoints <- function(input_data, EPSG_WGS84){

  map_tb <- input_data

  dates <- unique(lubridate::year(map_tb$end_date))
  indexLong <- which(colnames(map_tb) == "longitude")
  indexLat <- which(colnames(map_tb) == "latitude")
  indexLabel <- which(colnames(map_tb) == "label")

  # save points in environment
  points_input_map.list <- NULL
  points_input_map.list <<- list()

  for(x in 1:length(dates)){

    map <- dplyr::filter(map_tb, grepl(dates[x], as.character(map_tb$end_date), fixed = TRUE))
    pts <- map[c(indexLong:indexLat,indexLabel)] # long, lat and class
    colnames(pts) <- c('x', 'y', 'z')

    if (EPSG_WGS84 == TRUE) {
      # converte to sinusoidal projection in case values in Longitude and Latitude
      d <- data.frame("x" = pts$x, "y" = pts$y, "z" = pts$z, "w"= dates[x])
      sp::coordinates(d) <- cbind(pts$x, pts$y)
      sp::proj4string(d) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      CRS.new <- sp::CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
      d <- sp::spTransform(d, CRS.new)

    } else if (EPSG_WGS84 == FALSE) {
      # use in case data from SciDB col and row
      d <- data.frame(x=pts$x, y=pts$y, z=pts$z, w=dates[x])
      sp::coordinates(d) <- cbind(pts$x, pts$y)
    } else {
      stop("FALSE/TRUE")
    }

    pts1 <- as.data.frame(d)
    colnames(pts1) <- c('x1', 'y1', 'z', 'w', 'x', 'y')
    pts1 <- data.frame(pts1$x,pts1$y,pts1$z,pts1$w,pts1$x1,pts1$y1)
    names(pts1)[1:6] = c('x', 'y', 'z','w','x1', 'y1')
    points_input_map.list[[paste("pts_",dates[x], sep = "")]] <<- pts1

  }

}




