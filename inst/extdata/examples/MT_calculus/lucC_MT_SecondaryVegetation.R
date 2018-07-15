# RasterBrick Images
#----------------------------------------------------------
# Divide image in blocks, then merge again
#----------------------------------------------------------

library(lucCalculus)

options(digits = 12)

# start time
start.time <- Sys.time()

# without secondary vegetation
file <- c("~/TESTE/raster_mt_by_year_2004.tif")

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

file_name <- basename(tools::file_path_sans_ext(file))

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(service = "RASTER", files = file, name = file_name, timeline = timeline, bands = "ndvi")

message(paste0("\nLoad RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]

#label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_area", "Water"))
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_area", "Water"))

lucC_create_blocks(rb_sits, number_blocks_xy = 2, save_images = TRUE) # 4 blocks will be created

#--------------------------------------------
# new set of raster with secondary vegetation
#--------------------------------------------
# all files in folder
all.the.files <- list.files("~/TESTE/MT/MT_SecVeg", full=TRUE, pattern = ".tif")
all.the.files

# merge blocks into a single image
# lucC_merge_rasters(path_open_GeoTIFFs = "~/TESTE/MT/MT_SecVeg", number_raster = 4, pattern_name = "New_Raster_Splitted_", is.rasterBrick = TRUE)

# save each layer of brick as images
# lucC_save_rasterBrick_layers(path_name_GeoTIFF_Brick = "~/TESTE/MT/MT_SecVeg/Mosaic_New_Raster_Splitted_.tif")

# end time
print(Sys.time() - start.time)


#----------------------------------------------------------
# Discover Secondary Vegetation
#----------------------------------------------------------

library(lucCalculus)

options(digits = 12)

# all files in folder
# all.the.files <- list.files("~/Desktop/INPE_2018/Classi_MT_SVM/raster_1splitted", full=TRUE, pattern = ".tif")
all.the.files <- list.files("~/TESTE/MT", full=TRUE, pattern = ".tif")
all.the.files

#-------------
#Carrega os pacotes necessários para realizar o paralelismo
#library(foreach)

# #Checa quantos núcleos existem
# ncl <- parallel::detectCores()-2
# ncl
# #Registra os clusters a serem utilizados
# cl <- parallel::makeCluster(ncl) #ncl
# doParallel::registerDoParallel(6)
# foreach::getDoParWorkers()
# #-------------

# start time
start.time <- Sys.time()

result.list <- list()
sec_veg.tb <- NULL

for (i in 1:length(all.the.files)) {
#sec_veg.tb <- foreach(i = 1:length(all.the.files), .combine=rbind, .packages= c("lucCalculus")) %dopar%  {

  # file
  file <- all.the.files[i]
  #file <- list_MT[i]

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
  message("Start RECUR ...\n")
  #system.time(
  forest_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline)
  #)
  #head(forest_recur)

  message("RECUR ok! ...\n")

  # 2. EVOLVE to verify Forest class that occurs after a different class in 2001
  forest_evolve <- NULL

  # classes without Forest based on original label
  classes <- as.character(c("Cerrado", "Fallow_Cotton", "Pasture", "Soy", "Sugarcane", "Urban_area", "Water"))

  message("Start EVOLVE ...\n")

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
  forest_secondary <- lucC_merge(forest_recur, forest_evolve)

  # # plot
  # lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

  # 4. Remove column 2001 because it' is not used to replace pixels's only support column
  forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
  #head(forest_sec)

  rm(forest_recur, forest_evolve, forest_secondary)
  gc()

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
  message("Start update pixel in RasterBrick ...\n")

  number_label <- length(label) + 1
  # 1. update original RasterBrick with new class
  rb_sits_new <- lucC_raster_update(raster_obj = rb_sits,
                                    data_mtx = forest_sec,           # without 2001
                                    timeline = timeline,
                                    class_to_replace = "Forest",     # only class Forest
                                    new_pixel_value = number_label)  # new pixel value

  message("Updated pixel in RasterBrick ok! ...\n")
  #head(rb_sits_new)

  #lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

  # new name
  new_file_name <- paste0(dirname(file),"/", file_name, "_new", sep = "")

  result.list[[i]] <- rb_sits_new

  # 2. save the update matrix as GeoTIFF RasterBrick
  lucC_save_GeoTIFF(raster_obj = rb_sits,
                    data_mtx = rb_sits_new,
                    path_raster_folder = new_file_name, as_RasterBrick = FALSE ) # FALSE before

  lucC_save_GeoTIFF(raster_obj = rb_sits,
                    data_mtx = rb_sits_new,
                    path_raster_folder = new_file_name, as_RasterBrick = TRUE ) # FALSE before


  message("--------------------------------------------------\n")
  # clear environment, except these elements
  rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "end.time", "result.list"))])
  gc()

}

#Stop clusters
#parallel::stopCluster(cl)

# end time
print(Sys.time() - start.time)



#----------------------------------------------------
# Merge all blocks and then generate image exit for each band
#----------------------------------------------------

library(lucCalculus)

options(digits = 12)

# start time
start.time <- Sys.time()

# merge blocks into a single image
lucC_merge_rasters(path_open_GeoTIFFs = "~/TESTE/MT/MT_SecVeg/All_blocks_SecVeg", number_raster = 4, pattern_name = "New_Raster_Splitted_", is.rasterBrick = TRUE)
# save each layer of brick as images
lucC_save_rasterBrick_layers(path_name_GeoTIFF_Brick = "~/TESTE/MT/MT_SecVeg/All_blocks_SecVeg/Mosaic_New_Raster_Splitted_.tif")


# end time
print(Sys.time() - start.time)

