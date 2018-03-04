library(sits.LUC.Calculus)

# Juruena data <- "~/Desktop/rasterJuruena"

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/rasterJuruena", path_save_RasterBrick = "~/Desktop")

# open files
file <- c("~/Desktop/rasterJuruena.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a raster metadata file based on the information about the files
raster_sec.tb <- sits::sits_coverage(files = file, name = "Juruena_new", timeline = timeline, bands = "ndvi")
raster_sec.tb

# new variable
rb_sits_2 <- raster_sec.tb$r_objs[[1]][[1]]
rb_sits_2






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

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]


#----------------------------
# test secondary vegetation
#----------------------------
# classes without Forest
classes <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))

convert_class <- NULL

system.time(
  forest_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline)
)

class(forest_recur)

system.time(
  for(i in seq_along(classes)){
    print(classes[i])
    temp <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = classes[i],
                             time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                             raster_class2 = "Forest",
                             time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                             label = label, timeline = timeline)

    convert_class <- rbind(convert_class, temp)
  }
)

convert_class1 <- merge(convert_class, forest_recur, all = TRUE)
head(convert_class1)

drops <- c("2001-09-01")
convert_class1 <- convert_class1[ , !(names(convert_class1) %in% drops)]
head(convert_class1)


data_new <- lucC_remove_columns(data_mtx = convert_class1, name_columns = c("2001-09-01"))


#lucC_plot_sequence_events(convert_class, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(convert_class1, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend")

lucC_plot_bar_events(convert_class1, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend")


# new color
colors_1 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
lucC_plot_raster(raster_obj = rb_sits, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 6)

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = convert_class1, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE, shape_point = ".")


#-----------------------------------
# update original raster
new_class <- length(label) + 1

rb_sits_new <- lucC_update_raster(raster_obj = rb_sits,
                                  data_mtx = convert_class1,
                                  timeline = timeline,
                                  class_to_replace = "Forest",
                                  new_pixel_value = 17)

rb_sits_new

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

# save as raster file
lucC_save_GeoTIFF(raster_obj = rb_sits, data_mtx = rb_sits_new, path_raster_folder = "~/Desktop/rasterSec")



# ----------- open
library(sits.LUC.Calculus)

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/rasterSec", path_save_RasterBrick = "~/Desktop")

# open files
file <- c("~/Desktop/raster_Jur.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a raster metadata file based on the information about the files
raster_sec.tb <- sits::sits_coverage(files = file, name = "Juruena_new", timeline = timeline, bands = "ndvi")
raster_sec.tb

# new variable
rb_sits_2 <- raster_sec.tb$r_objs[[1]][[1]]
rb_sits_2

#--------------------
label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))
label2

secondary <- lucC_pred_holds(raster_obj = rb_sits_2, raster_class = "Secondary_vegetation",
                     time_interval = c("2001-09-01","2016-09-01"),
                     relation_interval = "contains", label = label2, timeline = timeline)
secondary

forest <- lucC_pred_holds(raster_obj = rb_sits_2, raster_class = "Forest",
                             time_interval = c("2001-09-01","2016-09-01"),
                             relation_interval = "contains", label = label2, timeline = timeline)
forest

ddad <- lucC_relation_equals(secondary, forest)
head(ddad)

lucC_plot_bar_events(data_mtx = ddad, pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

#-----------------
colors_2 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")

lucC_plot_raster(raster_obj = rb_sits_2, timeline = timeline, label = label2, custom_palette = TRUE, RGB_color = colors_2, relabel = FALSE)



lucC_plot_bar_events(data_mtx = df_new_sv[which(rb_sits_new[,c(3:ncol(rb_sits_new))] == "Forest" | rb_sits_new[,c(3:ncol(rb_sits_new))] ==  "Secondary_vegetation"),], pixel_resolution = 232, custom_palette = FALSE)











.
