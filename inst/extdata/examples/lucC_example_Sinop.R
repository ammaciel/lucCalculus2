library(lucCalculus)

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterSinop", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files
file <- c("inst/extdata/raster/rasterSinop.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
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
label <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
label

# colors
colors_1 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1

# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 6)


#----------------------------
# 2- Discover Secondary Vegetation - LUC Calculus
#----------------------------

# 1. Verify if forest RECUR ins econd interval
system.time(
  forest_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline)
)

head(forest_recur)

# 2. Verify if occur forest EVOLVE from a different class in 2001
forest_evolve <- NULL
# classes without Forest
classes <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))

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

# 3. Merge both forest_recur and forest_evolve datas
forest_secondary <- lucC_merge(forest_evolve, forest_recur)
head(forest_secondary)

lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
head(forest_sec)

lucC_plot_bar_events(forest_sec, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_sec,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")

# create images output
rb_sits_VS <- lucC_raster_result(raster_obj = rb_sits,
                                 data_mtx = forest_sec,       # without 2001
                                 timeline = timeline, label = label)         # new pixel value

lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_VS, path_raster_folder = "~/Desktop/N_Sinop", as_RasterBrick = FALSE)



#----------------------------
# 3- Update original raster to add new pixel value
#----------------------------

rm(forest_evolve, forest_recur, forest_secondary, raster.tb)
gc()

# 1. update original RasterBrick with new class
rb_sits_new <- lucC_update_raster(raster_obj = rb_sits,
                                     data_mtx = forest_sec,       # without 2001
                                     timeline = timeline,
                                     class_to_replace = "Forest",  # only class Forest
                                     new_pixel_value = 17)         # new pixel value

head(rb_sits_new)

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF images
lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_new,
                  path_raster_folder = "inst/extdata/raster/rasterSinopSecVeg", as_RasterBrick = FALSE)



#===================================================================================================
#----------------------------
# 4- Open idividual images reclassified and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterSinopSecVeg", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c("inst/extdata/raster/rasterSinopSecVeg.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "SinopVegSec", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits2 <- raster.tb$r_objs[[1]][[1]]
rb_sits2

# new class Seconary vegetation
label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))
label2

# colors
colors_2 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")


# plot raster brick
lucC_plot_raster(raster_obj = rb_sits2,
                 timeline = timeline, label = label2,
                 custom_palette = TRUE, RGB_color = colors_2, plot_ncol = 6)



#----------------------------
# 5- Discover Forest and Secondary vegetation - LUC Calculus
#----------------------------

secondary.mtx <- lucC_pred_holds(raster_obj = rb_sits2, raster_class = "Secondary_vegetation",
                                 time_interval = c("2001-09-01","2016-09-01"),
                                 relation_interval = "contains", label = label2, timeline = timeline)
head(secondary.mtx)

forest.mtx <- lucC_pred_holds(raster_obj = rb_sits2, raster_class = "Forest",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains", label = label2, timeline = timeline)
head(forest.mtx)

Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
head(Forest_secondary.mtx)

# plot results
lucC_plot_bar_events(data_mtx = Forest_secondary.mtx,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# Compute values
measuresFor_Sec <- lucC_result_measures(data_mtx = Forest_secondary.mtx, pixel_resolution = 232)
measuresFor_Sec

# save raster
new_raster <- lucC_update_raster_result(raster_obj = rb_sits2, data_mtx = Forest_secondary.mtx, timeline = timeline, label = label2)

# save the update matrix as GeoTIFF images
lucC_save_GeoTIFF(raster_obj = rb_sits2,
                  data_mtx = new_raster,
                  path_raster_folder = "inst/extdata/raster/rasterSinopResultForestVegSec", as_RasterBrick = FALSE)

# save the update matrix as GeoTIFF images
lucC_plot_raster(raster_obj = rb_sits2, timeline = timeline, label = label2,
                 plot_ncol = 6, custom_palette = TRUE, RGB_color = colors_2)



# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterSinopResultForestVegSec", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c("inst/extdata/raster/rasterSinopResultForestVegSec.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "SinopVegSec", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits3 <- raster.tb$r_objs[[1]][[1]]
rb_sits3

# new class Seconary vegetation
label3 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))

# colors
colors_3 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")


# plot raster brick
lucC_plot_raster(raster_obj = rb_sits3,
                 timeline = timeline, label = label3,
                 custom_palette = TRUE, RGB_color = colors_2, plot_ncol = 6)

.

plot(rb_sits3)










#----------------------------
# 6- Discover Land use transitions - LUC Calculus
#----------------------------
label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture", "Pasture", "Pasture", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))
label2

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

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


Soybean_Before_2006 <- soybean_before.df
Soybean_Before_2006[ Soybean_Before_2006 == "Soybean" ] <- "Soybean_Before_2006"
head(Soybean_Before_2006)

# remove(temp, soybean_before.df, forest.df, pasture.df, soybean.df, fores_past.temp, tempF, t_1, t_2, x)

# plot results
lucC_plot_bar_events(data_mtx = Soybean_Before_2006, pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)

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

Soybean_After_2006 <- soybean_after.df
Soybean_After_2006[ Soybean_After_2006 == "Soybean" ] <- "Soybean_After_2006"
head(Soybean_After_2006)

# remove(temp, soybean_before.df, forest.df, pasture.df, soybean.df, fores_past.temp, tempF, t_1, t_2, x)

# plot results
lucC_plot_bar_events(data_mtx = Soybean_After_2006, pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)

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

