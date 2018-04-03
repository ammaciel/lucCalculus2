#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to plot events as maps                           ##
##                                                             ##
##                                             2018-03-01      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Plot RasterBrick and Events over
#' @name lucC_plot_raster_result
#' @aliases lucC_plot_raster_result
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot map ggplot2 for all events discovered in input data
#'
#' @usage lucC_plot_raster_result (raster_obj = NULL, data_mtx = NULL,
#' timeline = NULL, label = NULL, custom_palette = FALSE, RGB_color = NULL,
#' plot_ncol = 5, relabel = FALSE, original_labels = NULL, new_labels = NULL,
#' legend_text = "Legend:", columns_legend = 4, shape_point = 0,
#' colour_point = "black", size_point= 1)
#'
#' @param raster_obj      Raster. A raster stack with classified images
#' @param data_mtx        Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
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
#' @param shape_point     Numeric or Character. Is a shape point for events highlighted over map. Default is 0.
#' This includes different points symbols commonly used in R as "pch", such as numeric values like 0 to square, 1 to circle
#' and 4 to cross shape. And also other characters can be used including ".", "+", "*", "-", "#".
#' @param colour_point    Numeric. Is a colour for the shape point for events highlighted over map. Default is black
#' @param size_point      Numeric. Is a size of the shape point around pixels in plot. Default is 1
#'
#' @keywords datasets
#' @return Plot with input data as colored map
#' @import ggplot2
#' @importFrom ensurer ensure_that
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lubridate year
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom stats na.omit setNames
#' @importFrom raster rasterToPoints
#' @export
#'
#' @examples \dontrun{
#'
#'
#' lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = b,
#' timeline = timeline, label = label, custom_palette = TRUE,
#' RGB_color = colors_1, relabel = FALSE)
#'
#'}
#'

# plot maps with events
lucC_plot_raster_result <- function(raster_obj = NULL, data_mtx = NULL, timeline = NULL, label = NULL, custom_palette = FALSE, RGB_color = NULL, plot_ncol = 5, relabel = FALSE, original_labels = NULL, new_labels = NULL, legend_text = "Legend:", columns_legend = 4, shape_point = 0, colour_point = "black", size_point= 1){

  # Ensure if parameters exists
  ensurer::ensure_that(raster_obj, !is.null(raster_obj),
                       err_desc = "raster_obj, file must be defined!")
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(timeline, !is.null(timeline),
                       err_desc = "timeline must be defined!")
  ensurer::ensure_that(label, !is.null(label),
                       err_desc = "label must be defined!")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(relabel, !is.null(relabel),
                       err_desc = "relabel must be defined, if wants use its own legend text setting! Default is FALSE")

  # --------- start: events result LUC Calculus -----------
  events_df <- as.data.frame(data_mtx)
  # change other by year
  colnames(events_df)[c(3:ncol(events_df))] <- as.character(lubridate::year(colnames(events_df)[c(3:ncol(events_df))]))
  # melt data
  #points_df <- reshape2::melt(events_df, id = c("x","y"))
  points_df <- events_df %>%
    tidyr::gather(variable, value, -x, -y) %>%
    stats::na.omit()

  points_df <- points_df[order(points_df$variable),] # order by years
  rownames(points_df) <- seq(length=nrow(points_df)) # reset row numbers

  # remove factor
  points_df$x = as.numeric(as.character(points_df$x)) #as.numeric(levels(points_df$x))[points_df$x]
  points_df$y = as.numeric(as.character(points_df$y))

  # --------- end: events result LUC Calculus -----------

  #-------------------- start: rasterBrick --------------------------------
  # make the points a dataframe for ggplot
  df <- raster::rasterToPoints(raster_obj) %>%
    data.frame()

  # make column headings
  colnames(df) <- c("x", "y")
  # change other by year of timeline
  colnames(df)[c(3:ncol(df))] <- as.character(lubridate::year(timeline))
  # melt data
  #raster_df <- reshape2::melt(df, id = c("x","y"))
  raster_df <- df %>%
    tidyr::gather(variable, value, -x, -y)

  # replace legend's number by text
  from <- as.character(sort(unique(raster_df$value)))
  to <- label
  # The map serves as a look-up, associating 'from' names with 'to' values.
  # The assignment preserves matrix dimensions and dimnames.
  map <- stats::setNames(to, from)
  raster_df$value <- map[raster_df$value]

  #-------------------- end: rasterBrick --------------------------------

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(label)){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      # select only colors that match with legend
      my_palette <- RGB_color %>%
        data.frame() %>%
        dplyr::mutate(id = c(1:length(.))) %>%
        .[.$id %in% from,] %>%
        .[,1] %>%
        as.character()
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
    geom_point(data=points_df, aes(x=points_df$x, y=points_df$y), shape = shape_point, colour = colour_point, size = size_point) + #  size=1.4
    scale_y_discrete(expand = c(0, 0), breaks = NULL) +
    scale_x_discrete(expand = c(0, 0), breaks = NULL) +
    theme_bw() +
    facet_wrap("variable", ncol = 6) +
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

