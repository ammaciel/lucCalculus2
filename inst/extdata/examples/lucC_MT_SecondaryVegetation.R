library(sits.LUC.Calculus)

# start time
start.time <- Sys.time()

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
# lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/INPE_2018/Classi_MT_SVM/raster_mt_by_year_2004", path_save_RasterBrick = "~/Desktop")

# ------------- define variables to use in sits -------------
# open files
file <- c("~/Desktop/INPE_2018/Classi_MT_SVM/raster_mt_by_year_2004.tif")
# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

file_name <- basename(tools::file_path_sans_ext(file))

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(service = "RASTER", files = file, name = file_name, timeline = timeline, bands = "ndvi")

message(paste0("\nLoad RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]

# ------------- define variables to plot raster -------------
#label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_area", "Water"))
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_area", "Water"))


list_MT <- lucC_create_blocks(rb_sits, number_blocks_xy = 5, save_images = TRUE)

lucC_merge_blocks(path_open_GeoTIFFs = "/home/inpe/github_projects/sits.LUC.Calculus")


# all files in folder
#all.the.files <- list.files("~/Desktop/INPE_2018/Classi_MT_SVM/raster_1splitted", full=TRUE, pattern = ".tif")
#all.the.files

for (i in 1:length(all.the.files)) {

  # file
  #file <- all.the.files[15]
  file <- all.the.files[i]
  #file

  # create timeline with classified data from SVM method
  timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

  file_name <- basename(tools::file_path_sans_ext(file))

  #library(sits)
  # create a RasterBrick metadata file based on the information about the files
  raster.tb <- sits::sits_coverage(service = "RASTER", files = file, name = file_name, timeline = timeline, bands = "ndvi")

  message(paste0("\nLoad RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

  # new variable
  rb_sits <- raster.tb$r_objs[[1]][[1]]

  # ------------- define variables to plot raster -------------
  # original label - see QML file, same order
  #label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_area", "Water"))
  label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_area", "Water"))

  # colors
  colors_1 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1")

  # plot raster brick
  # band1 <- rb_sits$X0000_0000.1
  # lucC_plot_raster(raster_obj = band1,
  #                  timeline = timeline, label = label,
  #                  custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 6)

  #----------------------------
  # 2- Discover Secondary Vegetation - LUC Calculus
  #----------------------------
  # 1. RECUR predicate indicates a class that appear again
  #system.time(
    forest_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"),
                                    time_interval2 = c("2003-09-01","2016-09-01"),
                                    label = label, timeline = timeline)
  #)
  #head(forest_recur)

  message("RECUR ok! ...\n")

  # 2. EVOLVE to verify Forest class that occurs after a different class in 2001
  forest_evolve <- NULL

  # classes without Forest based on original label
  classes <- as.character(c("Cerrado", "Fallow_Cotton", "Pasture", "Soy", "Sugarcane", "Urban_area", "Water"))

  #system.time(
    # percor all classes
    for(i in seq_along(classes)){
      print(classes[i])
      temp <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = classes[i],
                               time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                               raster_class2 = "Forest",
                               time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                               label = label, timeline = timeline)

      forest_evolve <- lucC_merge(forest_evolve, temp)
    }
  #)
  message("EVOLVE ok! ...\n")

  # 3. Merge both forest_recur and forest_evolve datas
  forest_secondary <- lucC_merge(forest_evolve, forest_recur)

  # # plot
  # lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

  # 4. Remove column 2001 because it' is not used to replace pixels's only support column
  forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
  #head(forest_sec)

  ## plot
  # lucC_plot_bar_events(forest_sec, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

  # # 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
  # lucC_plot_raster_result(raster_obj = rb_sits,
  #                         data_mtx = forest_sec,
  #                         timeline = timeline,
  #                         label = label, custom_palette = TRUE,
  #                         RGB_color = colors_1, relabel = FALSE) #, shape_point = ".")

  #----------------------------
  # 3 - Update original raster to add new pixel value
  #----------------------------

  number_label <- length(label)+1
  # 1. update original RasterBrick with new class
  rb_sits_new <- lucC_update_raster(raster_obj = rb_sits,
                                    data_mtx = forest_sec,           # without 2001
                                    timeline = timeline,
                                    class_to_replace = "Forest",     # only class Forest
                                    new_pixel_value = number_label)  # new pixel value

  message("Updated pixel in RasterBrick ok! ...\n")
  #head(rb_sits_new)

  #lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

  # new name
  new_file_name <- paste0(dirname(file),"/", file_name, "_new", sep = "")

  # 2. save the update matrix as GeoTIFF RasterBrick
  lucC_save_GeoTIFF(raster_obj = rb_sits,
                    data_mtx = rb_sits_new,
                    path_raster_folder = new_file_name, as_RasterBrick = TRUE ) # FALSE before


  message("--------------------------------------------------\n")
  # clear environment, except these elements
  rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "end.time"))])
  gc(reset = TRUE)

}

# end time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

