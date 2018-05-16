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
##                                             2018-03-01      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Plot RasterBrick
#' @name lucC_plot_raster
#' @aliases lucC_plot_raster
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot RasterBrick in ggplot2.
#'
#' @usage lucC_plot_raster(raster_obj = NULL, timeline = NULL,
#' label = NULL, custom_palette = FALSE, RGB_color = NULL,
#' plot_ncol = 5, relabel = FALSE, original_labels = NULL,
#' new_labels = NULL, legend_text = "Legend:", columns_legend = 4)
#'
#' @param raster_obj      Raster. A raster stack with classified images
#' @param timeline        Character. A list of all dates of classified raster, timeline
#' @param label           Character. All labels of each value of pixel from classified raster
#' @param custom_palette  Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE
#' @param RGB_color       Character. A vector with color names of original raster, same order. Default is setting scale_colour_hue
#' @param plot_ncol       Numeric. A number of columns to show the maps. Default is 5
#' @param relabel         Boolean. A TRUE or FALSE value. If TRUE, user will provide its own legend text setting! Default is FALSE
#' @param original_labels Character. A vector with original labels from legend text, for example, c("Forest","Pasture")
#' @param new_labels      Character. A vector with new labels to legend text, for example, c("Mature_Forest","Pasture1")
#' @param legend_text     Character. A text legend to show in plot. Default is "Legend:"
#' @param columns_legend   Integer. A number with the desired number of columns in legend. Default is 4
#'
#' @keywords datasets
#' @return Plot with input data as colored map
#' @import ggplot2
#' @importFrom ensurer ensure_that
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lubridate year
#' @importFrom dplyr mutate
#' @importFrom plyr mapvalues
#' @importFrom raster rasterToPoints
#' @importFrom tidyr gather
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_plot_raster(raster_obj = rb_sits, timeline = timeline, label = label,
#' custom_palette = TRUE, RGB_color = colors_1)
#'
#'}
#'

# plot maps for input data
lucC_plot_raster <- function(raster_obj = NULL, timeline = NULL, label = NULL, custom_palette = FALSE, RGB_color = NULL, plot_ncol = 5, relabel = FALSE, original_labels = NULL, new_labels = NULL, legend_text = "Legend:", columns_legend = 4) {

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj),
                       err_desc = "raster_obj, file must be defined!")
  ensurer::ensure_that(timeline, !is.null(timeline),
                       err_desc = "timeline must be defined!")
  ensurer::ensure_that(label, !is.null(label),
                       err_desc = "label must be defined!")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(relabel, !is.null(relabel),
                       err_desc = "relabel must be defined, if wants use its own legend text setting! Default is FALSE")

  #-------------------- start: rasterBrick --------------------------------
  # remove values with 0 as class
  raster_obj <- raster::reclassify(rb_sits, cbind(0, NA))

  # make the points a dataframe for ggplot
  df <- raster::rasterToPoints(raster_obj) %>%
    data.frame()

  # make column headings
  colnames(df) <- c("x", "y")
  # change other by year of timeline
  colnames(df)[c(3:ncol(df))] <- as.character(lubridate::year(timeline))
  # long format data
  #raster_df <- reshape2::melt(df, id = c("x","y"))
  raster_df <- tidyr::gather(df, variable, value, -x, -y) # other way

  classes_n <- sort(unique(raster_df$value))
  # replace legend's number by text
  from <- seq(1:length(label))
  to <- label
  # The map serves as a look-up, associating 'from' names with 'to' values.
  # The assignment preserves matrix dimensions and dimnames.
  raster_df$value <- plyr::mapvalues(raster_df$value, from, to)

  #-------------------- end: rasterBrick --------------------------------

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(label)){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      # select only colors that match with legend
      names(RGB_color) <- as.character(c(1:length(label)))
      my_palette <- RGB_color[names(RGB_color) %in% as.character(classes_n)]
    }
  } else {
    # more colors
    colour_count = length(unique(raster_df$value))
    my_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(name="Paired", n = 12))(colour_count)
  }

  label_raster_df <- unique(raster_df$value)
  # R to respect the order in data.frame. To change the order of factor levels by specifying the order explicitly.
  raster_df$value <- factor(raster_df$value, levels = label_raster_df[order(match(label_raster_df, label))])

  # show legend follow an order of labels
  original_leg_lab <- label_raster_df[order(match(label_raster_df, label))]
  cat("Original legend labels: \n", paste0(original_leg_lab, collapse = " ", sep = ","), "\n")

  # insert own legend text
  if(relabel == TRUE){
    if(is.null(original_labels) | length(new_labels) != length(unique(raster_df$value)) |
       all(original_labels %in% original_leg_lab) == FALSE){
      cat("\nIf relabel = TRUE, a vector with original labels must be defined!")
      cat("\nProvide a list of original labels and new labels with the same length of the legend!\n")
    } else {
      my_original_label = original_labels
      my_new_labels = new_labels
    }
  } else {
    # my legend text
    my_original_label = original_leg_lab
    my_new_labels = original_leg_lab
  }

  # plot images all years
  g <- ggplot(raster_df, aes(raster_df$x, raster_df$y)) +
    geom_raster(aes(fill=raster_df$value), stat = "identity") +
    scale_y_discrete(expand = c(0, 0), breaks = NULL) +
    scale_x_discrete(expand = c(0, 0), breaks = NULL) +
    theme_bw() +
    facet_wrap("variable", ncol = plot_ncol) +
    #coord_equal() +
    coord_fixed(ratio = 1) +
    #theme(legend.position = "bottom") +#, strip.text = element_text(size=10)) +
    xlab("") +
    ylab("") +
    scale_fill_manual(name = legend_text, values = my_palette, breaks = my_original_label, labels = my_new_labels) +
    guides(fill=guide_legend(ncol = columns_legend)) + # number of columns - legend plot
    theme(axis.title.x = element_blank(), # element_text(size=16),
          axis.title.y = element_blank(), #  element_text(size=16, angle=90),
          axis.text.x = element_blank(), #  element_text(size=14),
          axis.text.y = element_blank(), #  element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(colour=NA, fill="gray85"), #strip.background = element_blank(),
          legend.position = "bottom",
          legend.text=element_text(size=10),
          legend.key = element_blank()
    )

  print(g)

}



