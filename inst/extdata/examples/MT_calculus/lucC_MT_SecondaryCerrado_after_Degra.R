#----------------------------------------------------------
# Discover Cerrado Secondary
#----------------------------------------------------------

library(lucCalculus)

options(digits = 12)

# all files in folder
#all.the.files <- list.files("~/TESTE/MT/MT_SecVeg", full=TRUE, pattern = ".tif")
all.the.files <- list.files("~/TESTE/MT/MT_Degradation", full=TRUE, pattern = ".tif")
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
sec_cerrado.tb <- NULL

for (y in 1:length(all.the.files)) {
  #sec_cerrado.tb <- foreach(i = 1:length(all.the.files), .combine=rbind, .packages= c("lucCalculus")) %dopar%  {

  # file
  file <- all.the.files[y]
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
  #label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_area", "Water", "Secondary_Vegetation","Degradation"))
  label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation","Degradation"))

  #----------------------------
  # 2- Discover Secondary Cerrado - LUC Calculus
  #----------------------------
  # 1. RECUR predicate indicates a class that appear again
  message("Start RECUR ...\n")
  #system.time(
  cerrado_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Cerrado",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline)
  #)
  #head(cerrado_recur)

  message("RECUR ok! ...\n")

  # 2. EVOLVE to verify Cerrado class that occurs after a different class in 2001
  cerrado_evolve <- NULL

  # classes without Cerrado based on original label
  classes <- as.character(c("Forest", "Fallow_Cotton", "Pasture", "Soy", "Sugarcane", "Urban_area", "Water", "Secondary_Vegetation","Degradation"))

  message("Start EVOLVE ...\n")

  #system.time(
  # percor all classes
  for(i in seq_along(classes)){
    print(classes[i])
    temp <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = classes[i],
                             time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                             raster_class2 = "Cerrado",
                             time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                             label = label, timeline = timeline)

    cerrado_evolve <- lucC_merge(cerrado_evolve, temp)
  }
  #)
  message("EVOLVE ok! ...\n")

  # 3. Merge both cerrado_recur and cerrado_evolve datas
  cerrado_secondary <- lucC_merge(cerrado_recur, cerrado_evolve)

  # # plot
  # lucC_plot_bar_events(cerrado_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

  # 4. Remove column 2001 because it' is not used to replace pixels's only support column
  cerrado_sec <- lucC_remove_columns(data_mtx = cerrado_secondary, name_columns = c("2001-09-01"))
  #head(cerrado_sec)

  rm(cerrado_recur, cerrado_evolve, cerrado_secondary)
  gc()

  ## plot
  # lucC_plot_bar_events(cerrado_sec, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

  # # 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
  # lucC_plot_raster_result(raster_obj = rb_sits,
  #                         data_mtx = cerrado_sec,
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
                                    data_mtx = cerrado_sec,           # without 2001
                                    timeline = timeline,
                                    class_to_replace = "Cerrado",     # only class Cerrado
                                    new_pixel_value = number_label)  # new pixel value

  #lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

  # new name
  new_file_name <- paste0(dirname(file),"/", file_name, "_new", sep = "")

  message("Add to list index ", y, "... \n")
  result.list[[y]] <- rb_sits_new

  message("Save image pixel in RasterBrick ok! ...\n")

  # 2. save the update matrix as GeoTIFF RasterBrick
  message("Prepare image 1 ...\n")
  lucC_save_GeoTIFF(raster_obj = rb_sits,
                    data_mtx = rb_sits_new,
                    path_raster_folder = new_file_name, as_RasterBrick = TRUE ) # FALSE before

  message("Prepare image 2 ...\n")
  lucC_save_GeoTIFF(raster_obj = rb_sits,
                    data_mtx = rb_sits_new,
                    path_raster_folder = new_file_name, as_RasterBrick = FALSE ) # FALSE before

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

# renomeei de New_New_New para New_New

library(lucCalculus)

options(digits = 12)

# start time
start.time <- Sys.time()

# merge blocks into a single image
lucC_merge_rasters(path_open_GeoTIFFs = "~/TESTE/MT/MT_SecCerrado/All_blocks_SecCerrado", number_raster = 4, pattern_name = "New_New_Raster_Splitted_", is.rasterBrick = TRUE)
# save each layer of brick as images
lucC_save_rasterBrick_layers(path_name_GeoTIFF_Brick = "~/TESTE/MT/MT_SecCerrado/All_blocks_SecCerrado/Mosaic_New_New_Raster_Splitted_.tif")


# end time
print(Sys.time() - start.time)







