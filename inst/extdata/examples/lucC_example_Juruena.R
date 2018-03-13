library(sits.LUC.Calculus)


#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterJuruena", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files
file <- c("inst/extdata/raster/rasterJuruena.tif")
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

head(forest_recur)

# 2. Verify if occur forest FOLLOWS a different class in 2001
forest_evolve <- NULL
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

    forest_evolve <- lucC_merge(forest_evolve, temp)
  }
)

# 3. Merge both forest_recur and forest_evolve datas
forest_secondary <- lucC_merge(forest_evolve, forest_recur)
head(forest_secondary)

lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232)

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
head(forest_sec)

lucC_plot_bar_events(forest_sec, custom_palette = FALSE, pixel_resolution = 232)

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_Juruena,
                        data_mtx = forest_sec,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")



#----------------------------
# 3- Update original raster to add new pixel value
#----------------------------
label_new <- length(label)+1
# 1. update original RasterBrick with new class
rb_Juruena_new <- lucC_update_raster(raster_obj = rb_Juruena,
                                  data_mtx = forest_sec,        # without 2001
                                  timeline = timeline,
                                  class_to_replace = "Forest",  # only class Forest
                                  new_pixel_value = label_new)  # new pixel value

head(rb_Juruena_new)

lucC_plot_bar_events(data_mtx = rb_Juruena_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF images
lucC_save_GeoTIFF(raster_obj = rb_Juruena,
                  data_mtx = rb_Juruena_new,
                  path_raster_folder = "inst/extdata/raster/rasterJuruenaSec")



#===================================================================================================
#----------------------------
# 4- Open idividual images reclassified and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterJuruenaSec", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c("inst/extdata/raster/rasterJuruenaSec.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "JuruenaSecVeg", timeline = timeline, bands = "ndvi")
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
head(secondary.mtx)

forest.mtx <- lucC_pred_holds(raster_obj = rb_Juruena2, raster_class = "Forest",
                             time_interval = c("2001-09-01","2016-09-01"),
                             relation_interval = "contains", label = label2, timeline = timeline)
head(forest.mtx)

Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
head(Forest_secondary.mtx)

# plot results
lucC_plot_bar_events(data_mtx = Forest_secondary.mtx,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# Compute values
measures_Jur <- lucC_result_measures(data_mtx = Forest_secondary.mtx, pixel_resolution = 232)
measures_Jur

#-----------------
# define new color squeme - added Secondary Vegetation value
colors_2 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")

# plot
lucC_plot_raster(raster_obj = rb_Juruena2, timeline = timeline,
                 label = label2, custom_palette = TRUE,
                 RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)



#----------------------------
# 6- Discover Land use transitions - LUC Calculus
#----------------------------
#label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))
label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture", "Pasture", "Pasture", "Soybean_Cotton", "Soybean_Crop", "Soybean_Crop", "Soybean_Crop", "Soybean_Crop", "Soybean_Fallow", "Soybean_Fallow", "Water", "Water", "Secondary_vegetation"))
label2

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

class1 <- c("Forest")
classes <- c("Pasture", "Secondary_vegetation")

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
      temp <- lucC_pred_convert(raster_obj = rb_Juruena2, raster_class1 = class1,
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
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# Compute values
measures_Forest_Pasture <- lucC_result_measures(data_mtx = Forest_Pasture, pixel_resolution = 232)
measures_Forest_Pasture








