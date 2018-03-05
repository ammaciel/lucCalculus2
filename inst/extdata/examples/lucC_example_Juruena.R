library(sits.LUC.Calculus)


#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# Juruena data <- "~/Desktop/rasterJuruena"
# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/rasterJuruena", path_save_RasterBrick = "~/Desktop")

# ------------- define variables to use in sits -------------
# open files
file <- c("~/Desktop/rasterJuruena.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Juruena", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_Juruena <- raster.tb$r_objs[[1]][[1]]
rb_Juruena


# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
label

# colors
colors_1 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1

# plot raster brick
lucC_plot_raster(raster_obj = rb_Juruena,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 6)


#----------------------------
# 2- Discover Secondary Vegetation - LUC Calculus
#----------------------------

# 1. Verify if forest RECUR ins econd interval
system.time(
  forest_recur <- lucC_pred_recur(raster_obj = rb_Juruena, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline)
)

class(forest_recur)

# 2. Verify if occur forest FOLLOWS a different class in 2001
convert_class <- NULL
# classes without Forest
classes <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))

# percor all classes
system.time(
  for(i in seq_along(classes)){
    print(classes[i])
    temp <- lucC_pred_evolve(raster_obj = rb_Juruena, raster_class1 = classes[i],
                             time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                             raster_class2 = "Forest",
                             time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                             label = label, timeline = timeline)

    convert_class <- rbind(convert_class, temp)
  }
)

# 3. Merge both forest_recur and convert_class datas
forest_sec <- lucC_relation_equals(convert_class, forest_recur)
head(forest_sec)

lucC_plot_bar_events(forest_sec, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_sec2 <- lucC_remove_columns(data_mtx = forest_sec, name_columns = c("2001-09-01"))
head(forest_sec2)

lucC_plot_bar_events(forest_sec2, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_Juruena,
                        data_mtx = forest_sec2,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")



#----------------------------
# 3- Update original raster to add new pixel value
#----------------------------

# 1. update original RasterBrick with new class
rb_Juruena_new <- lucC_update_raster(raster_obj = rb_Juruena,
                                  data_mtx = forest_sec2,       # without 2001
                                  timeline = timeline,
                                  class_to_replace = "Forest",  # only class Forest
                                  new_pixel_value = 17)         # new pixel value

head(rb_Juruena_new)

lucC_plot_bar_events(data_mtx = rb_Juruena_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF images
lucC_save_GeoTIFF(raster_obj = rb_Juruena,
                  data_mtx = rb_Juruena_new,
                  path_raster_folder = "~/Desktop/rasterJuruenaSec")



#===================================================================================================
#----------------------------
# 4- Open idividual images reclassified and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/rasterJuruenaSec", path_save_RasterBrick = "~/Desktop")

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c("~/Desktop/rasterJuruenaSec.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "JuruenaSec", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_Juruena2 <- raster.tb$r_objs[[1]][[1]]
rb_Juruena2

# new class Seconary vegetation
label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))
label2


#----------------------------
# 5- Discover Forest and Secondary vegetation - LUC Calculus
#----------------------------

secondary.mtx <- lucC_pred_holds(raster_obj = rb_Juruena2, raster_class = "Secondary_vegetation",
                                 time_interval = c("2001-09-01","2016-09-01"),
                                 relation_interval = "contains", label = label2, timeline = timeline)
secondary.mtx

forest.mtx <- lucC_pred_holds(raster_obj = rb_Juruena2, raster_class = "Forest",
                             time_interval = c("2001-09-01","2016-09-01"),
                             relation_interval = "contains", label = label2, timeline = timeline)
forest.mtx

Forest_secondary.mtx <- lucC_relation_equals(secondary.mtx, forest.mtx)
head(Forest_secondary.mtx)

# plot results
lucC_plot_bar_events(data_mtx = Forest_secondary.mtx,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# Compute values
measures <- lucC_result_measures(data_mtx = Forest_secondary.mtx, pixel_resolution = 232)
measures

#-----------------
# define new color squeme - added Secondary Vegetation value
colors_2 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")

# plot
lucC_plot_raster(raster_obj = rb_Juruena2, timeline = timeline,
                 label = label2, custom_palette = TRUE,
                 RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)





