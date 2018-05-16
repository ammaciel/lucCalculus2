library(lucCalculus)

options(digits = 12) # always put this element

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
# lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/MT_Chronos/Sinop_2005_2016", path_save_RasterBrick = "~/Desktop/MT_Chronos/Sinop_2005_2016")

# ------------- define variables to use in sits -------------
# open files
file <- c("~/Desktop/MT_Chronos/Sinop_2005_2016/Sinop_2005_2016.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sinop", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]
rb_sits


# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_area", "Water", "Secondary_vegetation"))

#label <- as.character(c("Cerrado", "Single_cropping", "Forest", "Pasture", "Double_cropping", "Double_cropping", "Single_cropping", "Double_cropping", "Double_cropping", "Sugarcane", "Urban_area", "Water", "Secondary_vegetation"))

# colors
colors_1 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red")

# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 4)


#----------------------------
# 2- Discover Single and Double cropping - LUC Calculus
#----------------------------

message("Start Single cropping holds ...\n")
# secondary and forest
single.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Single_cropping",
                                 time_interval = c("2005-09-01","2016-09-01"),
                                 relation_interval = "contains", label = label, timeline = timeline)
#head(secondary.mtx)

message("Start Double cropping holds ...\n")
double.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Double_cropping",
                              time_interval = c("2005-09-01","2016-09-01"),
                              relation_interval = "contains", label = label, timeline = timeline)
#head(forest.mtx)

message("Merge Forest and Secondary Vegetation ...\n")
holds.mtx <- lucC_merge(single.mtx, double.mtx)
#head(Forest_secondary.mtx)

message("Save image in provided path ...\n")
# save result of secondary vegetation

file_name <- basename(tools::file_path_sans_ext(file))

message("Prepare image 1 ...\n")
lucC_save_raster_result(raster_obj = rb_sits, data_mtx = holds.mtx, timeline = timeline, label = label, path_raster_folder = paste0("~/Desktop/MT_Chronos/Sinop_2005_2016/Holds_SC_DC/", file_name, sep = ""), as_RasterBrick = FALSE)

message("Prepare image 2 ...\n")
lucC_save_raster_result(raster_obj = rb_sits, data_mtx = holds.mtx, timeline = timeline, label = label, path_raster_folder = paste0("~/Desktop/MT_Chronos/Sinop_2005_2016/Holds_SC_DC/", file_name, sep = ""), as_RasterBrick = TRUE)


.

#-------------------------------
# open files
file <- c("~/Desktop/MT_Chronos/Sinop_2005_2016/Holds_SC_DC/Sinop_2005_2016/New_Sinop_2005_2016.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

# library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sinop", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]
rb_sits

# change pixel of water by Cerrado, because this class doesn't exist in this municipality
#rb_sits <- raster::reclassify(rb_sits, cbind(0, 0))
#rb_sits <- raster::reclassify(rb_sits, cbind(0, NA))
#rb_sits
#rb_sits[is.na(rb_sits)] <- 0

label <- as.character(c("NA", "Single_cropping", "NA", "NA","Double_cropping"))

# colors
colors_1 <- c("green","blue", "purple","green","green")


# raster::plot(rb_sits)
# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 4)

rb_sits$New_Sinop_2005_2016.11

raster::plot(rb_sits$New_Sinop_2005_2016.1)

.
.
.


class1 <- c("Forest")
classes <- c("Pasture", "Secondary_vegetation") #

direct_transi.df <- NULL

# along of all classes
system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ")
      temp <- lucC_pred_convert(raster_obj = rb_sits2, raster_class1 = class1,
                                time_interval1 = c(t_1,t_1), relation_interval1 = "equals",
                                raster_class2 = classes[i],
                                time_interval2 = c(t_2,t_2), relation_interval2 = "equals",
                                label = label2, timeline = timeline)

      if (!is.null(temp)) {
        temp <- lucC_remove_columns(data_mtx = temp, name_columns = as.character(t_1))
      } else{
        temp <- temp
      }

      direct_transi.df <- lucC_merge(direct_transi.df, temp)
    }
    cat("\n")
  }
)

Forest_Pasture <- direct_transi.df
head(Forest_Pasture)

Forest_Pasture[ Forest_Pasture == "Pasture" ] <- "Forest_Pasture"
head(Forest_Pasture)

# plot results
lucC_plot_bar_events(data_mtx = Forest_Pasture,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = FALSE)

# Compute values
measures_Forest_Pasture <- lucC_result_measures(data_mtx = Forest_Pasture, pixel_resolution = 232)
measures_Forest_Pasture



#---------------------------------
# 7- Soybean Moratotium - LUC Calculus
# - Pasture to soybean (deforested before 2006)
#---------------------------------
# 1. All locations (pixels) that are soybean in a year?
# 2. In the past this location (pixel) was pasture in any time?
# 3. This location (pixel) was deforested before 2006? Soy Moratorium.
#
# o = geo-objects, the own df_input data.frame
#---------------------------------

#label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))

label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture", "Pasture", "Pasture", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Water", "Water", "Secondary_vegetation"))
label2

# create timeline with classified data from SVM method
timeline2 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

# soy moratorium
timeline1 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01"))


# intereting classes
soybean_before.df <- NULL

raster.data <- rb_sits2

# along of all classes
system.time(
  for(x in 2:length(timeline2)){
    #x = 7
    t_1 <- timeline1[x-1]
    t_2 <- timeline2[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    soybean.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Soybean",
                                  time_interval = c(t_2,t_2),
                                  relation_interval = "equals", label = label2, timeline = timeline)

    pasture.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                                  time_interval = c(timeline1[1],t_1),
                                  relation_interval = "contains", label = label2, timeline = timeline)

    forest.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                                 time_interval = c(timeline1[1],t_1),
                                 relation_interval = "contains", label = label2, timeline = timeline)

    fores_past.temp <- lucC_relation_occurs(pasture.df, forest.df)

    temp <- lucC_relation_precedes(soybean.df, fores_past.temp)

    if (!is.null(temp)) {
      tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_2)
    } else {
      tempF <- NULL
    }
    soybean_before.df <- lucC_merge(soybean_before.df, tempF)
  }
)


#Soybean_Before_2006 <- soybean_before.df
#Soybean_Before_2006[ Soybean_Before_2006 == "Soybean" ] <- "Soybean_Before_2006"
#head(Soybean_Before_2006)

# remove(temp, soybean_before.df, forest.df, pasture.df, soybean.df, fores_past.temp, tempF, t_1, t_2, x)

# plot results
lucC_plot_bar_events(data_mtx = soybean_before.df, pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)

## Compute values
# Soybean_Before_2006.tb <- lucC_result_measures(data_mtx = Soybean_Before_2006, pixel_resolution = 231.656)
# Soybean_Before_2006.tb
#
# # plot
# colors_3 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#7ecfa4", "#b6a896", "#3a3aff", "red", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")
#
# lucC_plot_raster(raster_obj = raster.data, timeline = timeline,
#                  label = label2, custom_palette = TRUE,
#                  RGB_color = colors_3, relabel = FALSE, plot_ncol = 6)
#
# lucC_plot_raster_result(raster_obj = raster.data, data_mtx = Soybean_Before_2006, timeline = timeline,
#                  label = label2, custom_palette = TRUE,
#                  RGB_color = colors_3, relabel = FALSE, plot_ncol = 6, shape_point = ".")
#


#---------------------------------
# 8 - Soybean Moratotium - LUC Calculus
# - Pasture to soybean (deforested after 2006)
#---------------------------------
# 1. All locations (pixels) that are soybean in a year?
# 2. In the past this location (pixel) was pasture in any time?
# 3. This location (pixel) was deforested after 2006? Soy Moratorium.
#
# o = geo-objects, the own df_input data.frame
#---------------------------------

#label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))

label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture", "Pasture", "Pasture", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Water", "Water", "Secondary_vegetation"))
label2

# create timeline with classified data from SVM method
timeline2 <- lubridate::as_date(c("2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

# soy moratorium
timeline1 <- lubridate::as_date(c("2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))


# intereting classes
soybean_after.df <- NULL

raster.data <- rb_sits2

# along of all classes
system.time(
  for(x in 2:length(timeline2)){
    #    x = 3
    t_1 <- timeline1[x-1]
    t_2 <- timeline2[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    soybean.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Soybean",
                                  time_interval = c(t_2,t_2),
                                  relation_interval = "equals", label = label2, timeline = timeline)

    pasture.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                                  time_interval = c(timeline1[1],t_1),
                                  relation_interval = "contains", label = label2, timeline = timeline)

    forest.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                                 time_interval = c(timeline1[1],t_1),
                                 relation_interval = "contains", label = label2, timeline = timeline)

    fores_past.temp <- lucC_relation_occurs(pasture.df, forest.df)

    temp <- lucC_relation_precedes(soybean.df, fores_past.temp)

    if (!is.null(temp)) {
      tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_2)
    } else {
      tempF <- NULL
    }
    soybean_after.df <- lucC_merge(soybean_after.df, tempF)
  }
)

#Soybean_After_2006 <- soybean_after.df
#Soybean_After_2006[ Soybean_After_2006 == "Soybean" ] <- "Soybean_After_2006"
#head(Soybean_After_2006)

# remove(temp, soybean_before.df, forest.df, pasture.df, soybean.df, fores_past.temp, tempF, t_1, t_2, x)

# plot results
lucC_plot_bar_events(data_mtx = soybean_after.df, pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)

# # Compute values
# Soybean_After_2006.tb <- lucC_result_measures(data_mtx = Soybean_After_2006, pixel_resolution = 231.656)
# Soybean_After_2006.tb
#
# # plot
# colors_3 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#7ecfa4", "#b6a896", "#3a3aff", "red", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")
#
# lucC_plot_raster(raster_obj = raster.data, timeline = timeline,
#                  label = label2, custom_palette = TRUE,
#                  RGB_color = colors_3, relabel = FALSE, plot_ncol = 6)
#
# lucC_plot_raster_result(raster_obj = raster.data, data_mtx = Soybean_After_2006, timeline = timeline,
#                         label = label2, custom_palette = TRUE,
#                         RGB_color = colors_3, relabel = FALSE, plot_ncol = 6, shape_point = ".")

#--------------------------------------------------------------


Soy <- lucC_merge(Soybean_Before_2006, Soybean_After_2006)
head(Soy)

lucC_plot_bar_events(data_mtx = Soy, pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)


.



.

#------------------------------------
# explit a raster by blocks
#------------------------------------
blocks <- lucC_create_blocks(rb_sits2, number_cells = 400)
blocks

lucC_plot_raster(raster_obj = blocks[[1]], timeline = timeline,
                 label = label2, custom_palette = TRUE,
                 RGB_color = colors_3, relabel = FALSE, plot_ncol = 6)

lucC_plot_raster(raster_obj = blocks[[2]], timeline = timeline,
                 label = label2, custom_palette = TRUE,
                 RGB_color = colors_3, relabel = FALSE, plot_ncol = 6)

