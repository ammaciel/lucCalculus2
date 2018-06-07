# Secondary Vegetation to Pasture, Cerrado and Soy

library(lucCalculus)

options(digits = 12)

# all files in folder
all.the.files <- list.files("~/TESTE/MT/MT_SecVeg", full=TRUE, pattern = ".tif")
all.the.files

# #-------------
# # #Carrega os pacotes necessários para realizar o paralelismo
# library(foreach)
# #
# # #Checa quantos núcleos existem
# ncl <- parallel::detectCores()-10
# ncl
# #Registra os clusters a serem utilizados
# cl <- parallel::makeCluster(ncl) #ncl
# doParallel::registerDoParallel(2)
# foreach::getDoParWorkers()
# #-------------

# start time
start.time <- Sys.time()

result.list <- list(NULL)

#convert.df <- foreach(y = 1:length(all.the.files), .combine=rbind, .packages= c("lucCalculus")) %dopar% {
for (y in 1:length(all.the.files)) {
  # file
  file <- all.the.files[y]

  # create timeline with classified data from SVM method
  timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

  file_name <- basename(tools::file_path_sans_ext(file))

  # library(sits)
  # create a RasterBrick metadata file based on the information about the files
  raster.tb <- sits::sits_coverage(service = "RASTER", name = file_name, timeline = timeline, bands = "ndvi", files = file)

  message("\n--------------------------------------------------\n")
  message(paste0("Load RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

  # new variable with raster object
  rb_sits <- raster.tb$r_objs[[1]][[1]]

  # ------------- define variables to plot raster -------------
  # original label - see QML file, same order
  label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

  message("Start Cerrado evolve from Forest ...\n")
  # Cerrado after Forest
  CerrFor_evolve0 <- NULL
  CerrFor_evolve0 <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = "Forest",
                                      time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                                      raster_class2 = "Cerrado",
                                      time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                                      label = label, timeline = timeline)
  #head(CerrFor_evolve)

  CerrFor_evolve <- lucC_remove_columns(data_mtx = CerrFor_evolve0, name_columns = "2001-09-01")

  # remove
  rm(CerrFor_evolve0)
  gc()

  # lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = CerrFor_evolve,timeline = timeline,
  #                         label = label, custom_palette = TRUE,
  #                         RGB_color = colors, relabel = FALSE, plot_ncol = 5, shape_point = ".")

  # Cerrado after Secondary areas
  message("Start Cerrado holds ...\n")
  cer.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
                             time_interval = c("2001-09-01","2016-09-01"),
                             relation_interval = "contains", label = label, timeline = timeline)
  #head(cer.mtx)

  # lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = cer.mtx, timeline = timeline,
  #                         label = label, custom_palette = TRUE,
  #                         RGB_color = colors, relabel = FALSE, plot_ncol = 6)

  message("Start Secondary_vegetation holds ...\n")
  vsec.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Secondary_Vegetation", #### <- font secondary with V ou v
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains", label = label, timeline = timeline)
  #head(vsec.mtx)

  # lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = vsec.mtx, timeline = timeline,
  #                         label = label, custom_palette = TRUE,
  #                         RGB_color = colors, relabel = FALSE, plot_ncol = 6)

  message("Start Cerrado and Secondary occurs ...\n")
  CerVS.mtx <- lucC_relation_occurs(first_raster = cer.mtx, second_raster = vsec.mtx)
  #head(CerVS.mtx)

  # lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = CerVS.mtx, timeline = timeline,
  #                         label = label, custom_palette = TRUE,
  #                         RGB_color = colors, relabel = FALSE, plot_ncol = 6)

  # only double and single with no occurs
  cer_only.mtx <- dplyr::anti_join(as.data.frame(cer.mtx), as.data.frame(CerVS.mtx), by = c("x","y"))
  VS_only.mtx <- dplyr::anti_join(as.data.frame(vsec.mtx), as.data.frame(CerVS.mtx), by = c("x","y"))

  # check number is ok
  message("check veg_sec and Cerrado not holds with Secondary occurs ...\n")
  nrow(vsec.mtx)
  nrow(CerVS.mtx)
  nrow(vsec.mtx) - nrow(CerVS.mtx)
  nrow(VS_only.mtx) # ok

  nrow(cer.mtx)
  nrow(CerVS.mtx)
  nrow(cer.mtx) - nrow(CerVS.mtx)
  nrow(cer_only.mtx)

  # only cerrado and no secondary vegetation
  CerVS_2.mtx <- as.data.frame(CerVS.mtx) %>%
    tidyr::gather(variable, value, -x, -y) %>%
    dplyr::filter(value == "Cerrado") #%>%
  #tidyr::spread(variable, value) %>%

  CerrFor_evolve2 <- as.data.frame(CerrFor_evolve) %>%
    tidyr::gather(variable, value, -x, -y) %>%
    dplyr::filter(value == "Cerrado")

  # remove
  rm(CerrFor_evolve, CerVS.mtx, VS_only.mtx, cer.mtx, cer_only.mtx, vsec.mtx)
  gc()

  Cerrado <- dplyr::bind_rows(CerVS_2.mtx, CerrFor_evolve2)
  # remove duplicated lines
  Cerrado <- Cerrado[!duplicated(Cerrado), ]

  # return matrix format
  degradation <- Cerrado %>%
    tidyr::spread(variable, value)

  # remove
  rm(CerVS_2.mtx, CerrFor_evolve2, Cerrado)
  gc()

  #----------------------------
  # 3 - Update original raster to add new pixel value
  #----------------------------
  message("Start update pixel in RasterBrick ...\n")

  n_label <- length(label) + 1
  # 1. update original RasterBrick with new class
  rb_sits_new <- lucC_raster_update(raster_obj = rb_sits,
                                    data_mtx = degradation,       # without 2001
                                    timeline = timeline,
                                    class_to_replace = "Cerrado",  # only class Forest
                                    new_pixel_value = n_label)         # new pixel value

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
                    path_raster_folder = new_file_name, as_RasterBrick = FALSE ) # FALSE before

  message("Prepare image 2 ...\n")
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
lucC_merge_rasters(path_open_GeoTIFFs = "~/TESTE/MT/MT_Degradation/All_blocks_Degradation", number_raster = 4, pattern_name = "New_New_Raster_Splitted_", is.rasterBrick = TRUE)
# save each layer of brick as images
lucC_save_rasterBrick_layers(path_name_GeoTIFF_Brick = "~/TESTE/MT/MT_Degradation/All_blocks_Degradation/Mosaic_New_New_Raster_Splitted_.tif")


# end time
print(Sys.time() - start.time)


