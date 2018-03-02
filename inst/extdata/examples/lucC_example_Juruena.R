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

#-----------
colors_1 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
lucC_plot_raster(raster_obj = rb_sits, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 6)

#-------------
# # alter attributes using labels
# rb_sits@data@attributes <- lapply(rb_sits@data@attributes, function(x)  {x <- data.frame(ID = c(1:length(label)), category = label)} )
# rasterVis::levelplot(rb_sits, col.regions=colors) # par.settings=rasterVis::RdBuTheme


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

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = third_raster.df, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE) #, shape_point = "#")

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

lucC_plot_raster(raster_obj = rb_sits, timeline = timeline, label = label, custom_palette = FALSE, RGB_color = colors_1)

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = b, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE, shape_point = ".")

.
.
.
