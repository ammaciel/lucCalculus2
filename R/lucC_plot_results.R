#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##  R script to plot events: sequence, frequency and bar       ##
##                                                             ##
##                                             2018-02-28      ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Plot Sequence Maps
#' @name lucC_plot_sequence_events
#' @aliases lucC_plot_sequence_events
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot locations as a sequence of lines over time
#'
#' @usage lucC_plot_sequence_events (data_mtx = NULL, custom_palette = FALSE,
#' RGB_color = NULL, show_y_index = TRUE, start_date = "2000-01-01",
#' end_date = "2016-12-31", relabel = FALSE, original_labels = NULL,
#' new_labels = NULL)
#'
#' @param data_mtx        Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param custom_palette  Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE
#' @param RGB_color       Character. A vector with color names to sequence legend, for example, c("Green","Blue"). Default is setting scale_colour_hue
#' @param show_y_index    Boolean. TRUE/FALSE to show the index values in the axis y of the graphic
#' @param start_date      Date. A start date to plot in sequence in format (ymd), '2011-01-01'
#' @param end_date        Date. A end date to plot in sequence in format (ymd), '2013-01-01'
#' @param relabel         Boolean. A TRUE or FALSE value. If TRUE, user will provide its own legend text setting! Default is FALSE
#' @param original_labels Character. A vector with original labels from legend text, for example, c("Forest","Pasture").
#' @param new_labels      Character. A vector with new labels to legend text, for example, c("Mature_Forest","Pasture1").
#'
#' @keywords datasets
#' @return Plot sequence time series as lines
#' @import ggplot2
#' @importFrom ensurer ensure_that
#' @importFrom scales hue_pal
#' @importFrom tidyr gather
#' @importFrom dplyr mutate group_indices_
#' @importFrom stats na.omit
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_plot_sequence_events(ts_occur1, show_y_index = FALSE,
#' end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e")
#'
#'}
#'

lucC_plot_sequence_events <- function(data_mtx = NULL, custom_palette = FALSE, RGB_color = NULL, show_y_index = TRUE, start_date = "2000-01-01", end_date = "2016-12-31", relabel = FALSE, original_labels = NULL, new_labels = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(show_y_index, !is.null(show_y_index),
                       err_desc = "Show y index label must be defined! Default is 'TRUE'")
  ensurer::ensure_that(start_date, !is.null(start_date),
                       err_desc = "Start date must be defined! Default is '2000-01-01'")
  ensurer::ensure_that(end_date, !is.null(end_date),
                       err_desc = "End date must be defined! Default is '2016-12-31'!")

  # to data.frame
  #mapSeq <- reshape2::melt(as.data.frame(data_mtx), id = c("x","y"), na.rm = TRUE)
  mapSeq <- as.data.frame(data_mtx) %>%
    tidyr::gather(variable, value, -x, -y) %>%
    stats::na.omit()

  mapSeq <- mapSeq[!duplicated(mapSeq), ]

  # create new columns to use in geom_segment
  data <- base::as.data.frame(mapSeq)
  data <- data %>%
    dplyr::mutate(start_date = as.Date((lubridate::ymd(.$variable) - lubridate::years(1)), format = '%Y-%m-%d')) %>%
    dplyr::mutate(end_date = as.Date((lubridate::ymd(.$variable)), format = '%Y-%m-%d')) %>%
    dplyr::mutate(Category = dplyr::group_indices_(data, .dots=c("x", "y"))) %>%
    dplyr::mutate(longLat = paste(.$x, .$y, .$Category, sep = ", ")) %>%
    stats::na.omit() %>%
    .[order(.$Category),] # order by index

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(data$value))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      my_palette = RGB_color
    }
  } else {
    # more colors
    colour_count = length(unique(data$value))
    my_palette = scales::hue_pal()(colour_count)
  }

  original_leg_lab <- unique(data$value)
  cat("Original legend labels: \n", original_leg_lab, "\n")

  # insert own legend text
  if(relabel == TRUE){
    if(is.null(original_labels) | length(new_labels) != length(unique(data$label)) |
       all(original_labels %in% original_leg_lab) == FALSE){
      cat("\nIf relabel = TRUE, a vector with original labels must be defined!")
      cat("\nProvide a list of original labels and new labels with the same length of the legend!\n")
    } else {
      my_original_label = original_labels
      my_new_labels = new_labels
    }
  } else {
    # my legend text
    my_original_label = unique(data$value)
    my_new_labels = unique(data$value)
  }

  g <- ggplot2::ggplot(data, aes(y = data$Category)) +
    labs(x = "Time", y = "Locations") +
    theme_bw()+
    geom_segment(aes(x = data$"start_date", y = data$Category,
                     xend = data$"end_date", yend = data$Category,
                     color = data$"value"), size = 1.25) +

    geom_point(aes(x = data$"start_date", color =  data$"value"), size = 3, shape = 19) +
    geom_point(aes(x = data$"end_date", color = data$"value"), size = 3, shape = 19) +

    # define time period
    scale_x_date(limits=as.Date(c(start_date, end_date))) +
    scale_y_continuous(breaks = data$"Category", labels = data$"longLat") +
    scale_color_manual(name="Legend:", values = my_palette, breaks = my_original_label, labels = my_new_labels)
  #scale_color_grey(name = "Legend:", start = 0, end = 0.8)

  # shows axis y label with index values from matrix
  if(show_y_index == TRUE){
    g <- g + theme(legend.position = "bottom",
                   legend.text = element_text(size=11), ###
                   legend.key = element_blank())
  } else {
    g <- g + theme(legend.position = "bottom",
                   legend.text = element_text(size=11), ###
                   axis.text.y = element_blank(),
                   legend.key = element_blank())
  }

  print(g)

}



#' @title Plot Barplot Maps
#' @name lucC_plot_bar_events
#' @aliases lucC_plot_bar_events
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot barplot over time
#'
#' @usage lucC_plot_bar_events (data_mtx = NULL, data_frequency = NULL,
#' custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250,
#' relabel = FALSE, original_labels = NULL, new_labels = NULL,
#' legend_text = "Legend:", column_legend = 2,
#' side_by_side = FALSE)
#'
#' @param data_mtx         Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param data_frequency   Dataframe. A frequency table of a categorical variable from a data set
#' @param custom_palette   Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE
#' @param RGB_color        Character. A vector with color names to map legend, for example, c("Green","Blue"). Default is setting scale_colour_hue
#' @param pixel_resolution Numeric. Is a spatial resolution of the pixel. Default is 250 meters considering MODIS 250 m. See more at \url{https://modis.gsfc.nasa.gov/about/specifications.php}.
#' @param relabel          Boolean. A TRUE or FALSE value. If TRUE, user will provide its own legend text setting! Default is FALSE
#' @param original_labels  Character. A vector with original labels from legend text, for example, c("Forest","Pasture").
#' @param new_labels       Character. A vector with new labels to legend text, for example, c("Mature_Forest","Pasture1").
#' @param legend_text      Character. A text legend to show in plot, such as "Land use transitions:". Default is "Legend:".
#' @param column_legend    Integer. A number with the desired number of columns in legend.
#' @param side_by_side     Boolean. Make bar of a barplot a side-by-side plot. Default is FALSE.
#'
#' @keywords datasets
#' @return Plot a barplot in Y axis in square kilometers (Area km^2) = (Number of pixel *(pixel_resolution*pixel_resolution))/(1000*1000)
#' @import ggplot2
#' @importFrom ensurer ensure_that
#' @importFrom lubridate year
#' @importFrom scales hue_pal
#' @importFrom tidyr gather
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_plot_bar_events(data_mtx = raster_mtx, custom_palette = TRUE,
#' RGB_color = c("green", "orange"), pixel_resolution = 232,
#' relabel = TRUE, original_labels = c("Forest", "Pasture1"),
#' new_labels = c("Forest", "Pasture"), legend_text = "Legend: ",
#' column_legend = 1, side_by_side = TRUE)
#'
#'}
#'

lucC_plot_bar_events <- function(data_mtx = NULL, data_frequency = NULL, custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250, relabel = FALSE, original_labels = NULL, new_labels = NULL, legend_text = "Legend:", column_legend = 2, side_by_side = FALSE){

  # Ensure if parameters exists
  #ensurer::ensure_that(data_mtx, !is.null(data_mtx),
  #                     err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(pixel_resolution, !is.null(pixel_resolution),
                       err_desc = "pixel_resolution must be defined! Default is 250 meters on basis of MODIS image")

  # input data matrix or a frequency table
  if (!is.null(data_mtx)){
    # to data frame
    #input_data <- reshape2::melt(as.data.frame(data_mtx), id = c("x","y"), na.rm = TRUE)
    input_data <- as.data.frame(data_mtx) %>%
      tidyr::gather(variable, value, -x, -y) %>%
      stats::na.omit()

    input_data <- input_data[!duplicated(input_data), ]
    # count number of values
    mapBar <- data.frame(table(lubridate::year(input_data$variable), input_data$value))
  } else if (!is.null(data_frequency)){
    # already
    mapBar <- data_frequency
    colnames(mapBar) <- c("Var1", "Var2", "Freq")
  } else {
    stop("\nProvide at least a 'data_mtx' or a 'data_frequency' to plot graphics!\n")
  }

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(mapBar$Var2))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      my_palette = RGB_color
    }
  } else {
    # more colors
    colour_count = length(unique(mapBar$Var2))
    my_palette = scales::hue_pal()(colour_count)
  }

  original_leg_lab <- unique(as.character(mapBar$Var2)) # levels(droplevels(mapBar$Var2))
  cat("Original legend labels: \n", original_leg_lab, "\n")

  # insert own legend text
  if(relabel == TRUE){
    if(is.null(original_labels) | length(new_labels) != length(unique(mapBar$Var2)) |
       all(original_labels %in% original_leg_lab) == FALSE){
      cat("\nIf relabel = TRUE, a vector with original labels must be defined!")
      cat("\nProvide a list of original labels and new labels with the same length of the legend!\n")
    } else {
      my_original_label = original_labels
      my_new_labels = new_labels
    }
  } else {
    # my legend text
    my_original_label = unique(mapBar$Var2)
    my_new_labels = unique(mapBar$Var2)
  }

  # make side-by-side bar plot
  if (side_by_side == TRUE){
    bar_position = "dodge"
  } else {
    bar_position = "stack"
  }

  g <- ggplot2::ggplot(mapBar,aes(x=mapBar$Var1, y=(mapBar$Freq*(pixel_resolution*pixel_resolution))/(1000*1000), fill=mapBar$Var2))+
    geom_bar(width = 0.7, stat="identity", position = bar_position)+
    theme_bw()+
    #ylab(expression(paste("Area ",km^{2}," = ((pixels number x pixel ", resolution^{2},")/",1000^{2},")"))) +
    ylab(expression(paste("Area (",km^{2},")")))+
    xlab("Time")+
    scale_fill_manual(name= legend_text, values = my_palette, breaks = my_original_label, labels = my_new_labels) + #Legend
    #scale_fill_grey(name = "Legend:", start = 0, end = 0.8) +
    # theme(legend.position = "bottom",
    #       legend.text=element_text(size=11),  ###
    #       legend.key = element_blank())

    guides(fill=guide_legend(ncol = column_legend)) + # number of columns - legend plot

    theme(legend.position = "bottom",
          legend.text=element_text(size=10), ### ### era 11
          axis.text.x=element_text(angle=45, hjust=1, size = 10),
          legend.key = element_blank())

  print(g)

}



#' @title Plot Frequency Polygon
#' @name lucC_plot_frequency_events
#' @aliases lucC_plot_frequency_events
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Plot frequency line over time
#'
#' @usage lucC_plot_frequency_events (data_mtx = NULL, data_frequency = NULL,
#' custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250,
#' relabel = FALSE, original_labels = NULL, new_labels = NULL,
#' legend_text = "Legend:", column_legend = 2)
#'
#' @param data_mtx         Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param data_frequency   Dataframe. A frequency table of a categorical variable from a data set
#' @param custom_palette   Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE
#' @param RGB_color        Character. A vector with color names to map legend, for example, c("Green","Blue"). Default is setting scale_colour_hue
#' @param pixel_resolution Numeric. Is a spatial resolution of the pixel. Default is 250 meters considering MODIS 250 m. See more at \url{https://modis.gsfc.nasa.gov/about/specifications.php}.
#' @param relabel          Boolean. A TRUE or FALSE value. If TRUE, user will provide its own legend text setting! Default is FALSE
#' @param original_labels  Character. A vector with original labels from legend text, for example, c("Forest","Pasture").
#' @param new_labels       Character. A vector with new labels to legend text, for example, c("Mature_Forest","Pasture1").
#' @param legend_text      Character. A text legend to show in plot. Default is "Legend:".
#' @param column_legend    Integer. A number with the desired number of columns in legend.
#'
#' @keywords datasets
#' @return Plot a frequency polygon in Y axis in square kilometers (Area km^2) = (Number of pixel *(pixel_resolution*pixel_resolution))/(1000*1000)
#' @import ggplot2
#' @importFrom ensurer ensure_that
#' @importFrom lubridate year
#' @importFrom scales hue_pal
#' @importFrom tidyr gather
#' @export
#'
#' @examples \dontrun{
#'
#' lucC_plot_frequency_events(data_mtx = raster_mtx, custom_palette = TRUE,
#' RGB_color = c("green", "orange"), pixel_resolution = 232,
#' relabel = TRUE, original_labels = c("Forest", "Pasture1"),
#' new_labels = c("Forest", "Pasture"), legend_text = "Legend: ",
#' column_legend = 1, side_by_side = TRUE)
#'
#'}
#'

lucC_plot_frequency_events <- function(data_mtx = NULL, data_frequency = NULL, custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250, relabel = FALSE, original_labels = NULL, new_labels = NULL, legend_text = "Legend:", column_legend = 2){

  # Ensure if parameters exists
  #ensurer::ensure_that(data_mtx, !is.null(data_mtx),
  #                     err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(pixel_resolution, !is.null(pixel_resolution),
                       err_desc = "pixel_resolution must be defined! Default is 250 meters on basis of MODIS image")

  # input data matrix or a frequency table
  if (!is.null(data_mtx)){
    # to data frame
    #input_data <- reshape2::melt(as.data.frame(data_mtx), id = c("x","y"), na.rm = TRUE)
    input_data <- as.data.frame(data_mtx) %>%
      tidyr::gather(variable, value, -x, -y) %>%
      stats::na.omit()

    input_data <- input_data[!duplicated(input_data), ]
    # count number of values
    mapFreq <- data.frame(table(lubridate::year(input_data$variable), input_data$value))
  } else if (!is.null(data_frequency)){
    # already
    mapFreq <- data_frequency
    colnames(mapFreq) <- c("Var1", "Var2", "Freq")
  } else {
    stop("\nProvide at least a 'data_mtx' or a 'data_frequency' to plot graphics!\n")
  }

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(mapFreq$Var2))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      my_palette = RGB_color
    }
  } else {
    # more colors
    colour_count = length(unique(mapFreq$Var2))
    my_palette = scales::hue_pal()(colour_count)
  }

  original_leg_lab <- unique(as.character(mapFreq$Var2)) # levels(droplevels(mapFreq$Var2))
  cat("Original legend labels: \n", original_leg_lab, "\n")

  # insert own legend text
  if(relabel == TRUE){
    if(is.null(original_labels) | length(new_labels) != length(unique(mapFreq$Var2)) |
       all(original_labels %in% original_leg_lab) == FALSE){
      cat("\nIf relabel = TRUE, a vector with original labels must be defined!")
      cat("\nProvide a list of original labels and new labels with the same length of the legend!\n")
    } else {
      my_original_label = original_labels
      my_new_labels = new_labels
    }
  } else {
    # my legend text
    my_original_label = unique(mapFreq$Var2)
    my_new_labels = unique(mapFreq$Var2)
  }

  g <- ggplot2::ggplot(mapFreq,aes(x=mapFreq$Var1, y=(mapFreq$Freq*(pixel_resolution*pixel_resolution))/(1000*1000), group = mapFreq$Var2, color = mapFreq$Var2))+
    geom_freqpoly(stat = "identity", size = 1)+
    geom_point( size = 2, shape = 16) +
    theme_bw()+
    #ylab(expression(paste("Area ",km^{2}," = ((pixels number x pixel ", resolution^{2},")/",1000^{2},")"))) +
    ylab(expression(paste("Area (",km^{2},")")))+
    xlab("Time")+
    scale_color_manual(name= legend_text, values = my_palette, breaks = my_original_label, labels = my_new_labels) + #Legend
    #scale_fill_grey(name = "Legend:", start = 0, end = 0.8) +
    # theme(legend.position = "bottom",
    #       legend.text=element_text(size=11),  ###
    #       legend.key = element_blank())

    guides(fill=guide_legend(ncol= column_legend)) + # number of columns - legend plot

    theme(legend.position = "bottom",
          legend.text=element_text(size=10), ### ### era 11
          axis.text.x=element_text(angle=45, hjust=1, size = 10),
          legend.key = element_blank())


  print(g)

}


