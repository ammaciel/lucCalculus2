library(lucCalculus)

# always
options(digits = 12)

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterItanhanga", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files
file <- c("inst/extdata/raster/rasterItanhanga.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Itanhanga", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]
rb_sits


# ------------- define variables to plot raster -------------
# original label - see QML file, same order
#label <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
label <- as.character(c("Cerrado", "Double_cropping", "Single_cropping", "Forest", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water"))
label

# colors
#colors_1 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1 <- c("#b3cc33", "#cd6155", "#e6b0aa", "#228b22", "#7ecfa4", "#afe3c8",  "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1

# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 5)

# change pixel of water by Cerrado, because this class doesn't exist in this municipality
rb_sits <- raster::reclassify(rb_sits, cbind(15, 1))
rb_sits

lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 5)

#----------------------------
# IJGIS
#----------------------------
# 1- Discover Forest Recur its natural vegetation - LUC Calculus
#----------------------------

system.time(
  forest_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline)
)

head(forest_recur)

lucC_plot_bar_events(forest_recur, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_re <- lucC_remove_columns(data_mtx = forest_recur, name_columns = c("2001-09-01"))
head(forest_sec)

lucC_plot_bar_events(forest_re, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_re,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")
# Save results
# create images output
lucC_save_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_re,       # without 2001
                        timeline = timeline, label = label, path_raster_folder = "~/Desktop/rasterItanhanga_RECUR", as_RasterBrick = FALSE)


#----------------------------
# 2. Verify if occur forest EVOLVE from a different class in 2001
#----------------------------

forest_evolve <- NULL
# classes without Forest
#classes <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
classes <- as.character(c("Cerrado", "Double_cropping", "Single_cropping", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water"))

# percor all classes
system.time(
  for(i in seq_along(classes)){
    print(classes[i])
    temp <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = classes[i],
                             time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                             raster_class2 = "Forest",
                             time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                             label = label, timeline = timeline)

    forest_evolve <- lucC_merge(forest_evolve, temp)
  }
)

head(forest_evolve)


lucC_plot_bar_events(forest_evolve, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_ev <- lucC_remove_columns(data_mtx = forest_recur, name_columns = c("2001-09-01"))
head(forest_ev)

lucC_plot_bar_events(forest_re, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_ev,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")
# Save results
# create images output
lucC_save_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_ev,       # without 2001
                        timeline = timeline, label = label, path_raster_folder = "~/Desktop/rasterItanhanga_EVOLVE", as_RasterBrick = FALSE)



