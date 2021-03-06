# Degradation to Pasture, Soy and Secondary vegetation

library(lucCalculus)

options(digits = 12)

# all files in folder
#all.the.files <- list.files("~/TESTE/MT/MT_SecVeg", full=TRUE, pattern = ".tif")
#all.the.files <- list.files("~/TESTE/MT/MT_Degradation", full=TRUE, pattern = ".tif")
all.the.files <- list.files("~/TESTE/MT/MT_SecCerrado", full=TRUE, pattern = ".tif")
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

number_Degradation_others <- list(NULL)

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
  label2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation","Degradation", "Secondary_Cerrado"))

  class1 <- c("Degradation")
  classes <- c("Pasture", "Soy", "Forest", "Secondary_Vegetation", "Cerrado", "Secondary_Cerrado") #

  direct_transi.df <- NULL

  message("Start Convert ...\n")
  # along of all classes
  # system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ", "\n")
      temp <- lucC_pred_convert(raster_obj = rb_sits, raster_class1 = class1,
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
  #)

  #Forest_Pasture <- direct_transi.df
  #head(Forest_Pasture)

  #Forest_Pasture[ Forest_Pasture == "Pasture" ] <- "Pasture"
  #Forest_Pasture[ Forest_Pasture == "Soy" ] <- "Soy"
  #head(Forest_Pasture)
  message("Add to list index ", y, "... \n")

  number_Degradation_others[[y]] <- direct_transi.df

  message("Prepare image 1 ...\n")
  lucC_save_raster_result(raster_obj = rb_sits, data_mtx = direct_transi.df, timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/DLUCDegradationOthers/", file_name, sep = ""), as_RasterBrick = TRUE)

  message("Prepare image 2 ...\n")
  lucC_save_raster_result(raster_obj = rb_sits, data_mtx = direct_transi.df, timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/DLUCDegradationOthers/", file_name, sep = ""), as_RasterBrick = FALSE)

  # clear environment, except these elements
  rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "number_Degradation_others"))])
  gc()

  message("--------------------------------------------------\n")
}

message("Save data as list in .rda file ...\n")
#save to rda file
save(number_Degradation_others, file = "~/TESTE/MT/DLUCDegradationOthers/number_Degradation_others.rda")

# #Stop clusters
# parallel::stopCluster(cl)

# end time
print(Sys.time() - start.time)

rm(number_Degradation_others)
gc()

#----------------------------------------------------
# Save results as measures
#----------------------------------------------------


# start time
start.time <- Sys.time()


load(file = "~/TESTE/MT/DLUCDegradationOthers/number_Degradation_others.rda")

output_freq <- lucC_extract_frequency(data_mtx.list = number_Degradation_others, cores_in_parallel = 6)
output_freq

#----------------------
# # plot results
# lucC_plot_bar_events(data_frequency = output_freq,
#                      pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)
#
# lucC_plot_frequency_events(data_frequency = output_freq,
#                      pixel_resolution = 231.656, custom_palette = FALSE)

# Compute values
measuresDegradation_others <- lucC_result_measures(data_frequency = output_freq, pixel_resolution = 231.656)
measuresDegradation_others

write.table(x = measuresDegradation_others, file = "~/TESTE/MT/DLUCDegradationOthers/measuresDegradation_others.csv", quote = FALSE, sep = ";", row.names = FALSE)

save(measuresDegradation_others, file = "~/TESTE/MT/DLUCDegradationOthers/measuresDegradation_others.rda")


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
lucC_merge_rasters(path_open_GeoTIFFs = "~/TESTE/MT/DLUCDegradationOthers/All_blocks_Degradation_others", number_raster = 4, pattern_name = "New_New_New_Raster_Splitted_", is.rasterBrick = TRUE)
# save each layer of brick as images
lucC_save_rasterBrick_layers(path_name_GeoTIFF_Brick = "~/TESTE/MT/DLUCDegradationOthers/All_blocks_Degradation_others/Mosaic_New_New_New_Raster_Splitted_.tif")


# end time
print(Sys.time() - start.time)

