library(sits.LUC.Calculus)

# MT_samples <- "~/Desktop/rasterJuruena"
# #MT_samples <- "~/Desktop/raster"
#
# # create a rasterBrick with data
# MT_samples_brick <- list.files(MT_samples,
#                                full.names = TRUE,
#                                pattern = ".tif$") %>%
#   raster::stack(.) %>%
#   raster::brick(.) %>%
#   raster::writeRaster(., "~/Desktop/raster_Jur.tif", overwrite=TRUE)

# open files
file <- c("inst/extdata/raster/raster_Jur.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

# create label with classified data from SVM method
label <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
label

#library(sits)
# create a raster metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sample_region", timeline = timeline, bands = "ndvi")
raster.tb

raster.tb$r_objs[[1]][[1]]

class(raster.tb$r_objs[[1]][[1]])

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]

# resolution
raster::res(rb_sits)

#raster::plot(rb_sits)
names(rb_sits)

#-------------
# alter attributes using labels
rb_sits@data@attributes <- lapply(rb_sits@data@attributes, function(x)  {x <- data.frame(ID = c(1:length(label)), category = label)} )
#colors <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1" )
colors <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")

rasterVis::levelplot(rb_sits, col.regions=colors) # par.settings=rasterVis::RdBuTheme



#------------- tests - intervals before, meets and follows -- Allen's relations
a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
                     time_interval = c("2001-09-01","2003-09-01"),
                     relation_interval = "equals", label = label, timeline = timeline)
a


b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
                     time_interval = c("2001-09-01","2007-09-01"),
                     relation_interval = "contains", label = label, timeline = timeline)
b

# before
#c <- lucC_relation_before(first_raster = a, second_raster = b)
#c <- lucC_relation_after(first_raster = b, second_raster = a)
#c <- lucC_relation_meets(first_raster = a, second_raster = b)
#c <- lucC_relation_met_by(first_raster = a, second_raster = b) # C c("2008-09-01","2010-09-01"), F c("2002-09-01","2007-09-01")
c <- lucC_relation_starts(a,b)
#c <- lucC_relation_started_by(b,a)
#c <- lucC_relation_finishes(b,a)
#c <- lucC_relation_finished_by(a,b)
#c <- lucC_relation_during(a,b)
#c <- lucC_relation_equals(a,b)
c
lucC_plot_sequence_events(c, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(c, custom_palette = FALSE)


#------------- tests - recur
system.time(third_raster.df <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                   time_interval1 = c("2001-09-01","2001-09-01"),
                                   time_interval2 = c("2002-09-01","2016-09-01"),
                                   label = label, timeline = timeline))
third_raster.df

lucC_plot_sequence_events(third_raster.df, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(third_raster.df, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend")


#------------- tests - convert
sixth_raster.df <- lucC_pred_convert(raster_obj = rb_sits, raster_class1 = "Forest",
                                     time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                                     raster_class2 = "Pasture1",
                                     time_interval2 = c("2002-09-01","2002-09-01"), relation_interval2 = "equals",
                                     label = label, timeline = timeline)
sixth_raster.df

lucC_plot_sequence_events(sixth_raster.df, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(sixth_raster.df, custom_palette = FALSE)


#------------- tests - evolve
fifth_raster.df <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "contains",
                                    raster_class2 = "Pasture1",
                                    time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                                    label = label, timeline = timeline)
fifth_raster.df

lucC_plot_sequence_events(fifth_raster.df, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(data_mtx = fifth_raster.df, custom_palette = TRUE, RGB_color = c("green", "orange"), pixel_resolution = 232, relabel = TRUE, original_labels = c("Forest", "Pasture1"), new_labels = c("Forest", "Pasture"), legend_text = "Legend: ", column_legend = 1, side_by_side = TRUE)
lucC_plot_frequency_events(fifth_raster.df, custom_palette = FALSE, legend_text = "Legend")


#---------------
a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Pasture1",
                     time_interval = c("2001-09-01","2001-09-01"),   # c("2001-09-01","2003-09-01"),
                     relation_interval = "contains", label = label, timeline = timeline)
a

b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
                     time_interval = c("2002-09-01","2007-09-01"),  # c("2004-09-01","2007-09-01"),
                     relation_interval = "contains", label = label, timeline = timeline)
b

# before
c1 <- lucC_relation_before(first_raster = a, second_raster = b)
c1
lucC_plot_sequence_events(c1, custom_palette = FALSE, show_y_index = TRUE)

# meets
c2 <- lucC_relation_meets(first_raster = a, second_raster = b)
c2
lucC_plot_sequence_events(c2, custom_palette = FALSE, show_y_index = TRUE)

# follows
c3 <- lucC_relation_follows(first_raster = a, second_raster = b)
c3
lucC_plot_sequence_events(c3, custom_palette = FALSE, show_y_index = TRUE)
lucC_plot_bar_events(c3, custom_palette = FALSE, side_by_side = FALSE, legend_text = "New", column_legend = 2)
lucC_plot_frequency_events(c31, custom_palette = FALSE, legend_text = "New", column_legend = 1)

# in
c4 <- lucC_relation_in(first_raster = a, second_raster = b)
c4
lucC_plot_sequence_events(c4, custom_palette = FALSE, show_y_index = TRUE)
lucC_plot_bar_events(c4, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend")
lucC_plot_frequency_events(c4, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend")


.
.
.
.
.

raster_obj$crs

# plot maps for input data
lucC_plot_maps_input2 <- function(raster_obj = NULL, custom_palette = FALSE, RGB_color = NULL, plot_ncol = NULL, relabel = FALSE, original_labels = NULL, new_labels = NULL) {

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

  CRS_obj <- input_data@crs

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

