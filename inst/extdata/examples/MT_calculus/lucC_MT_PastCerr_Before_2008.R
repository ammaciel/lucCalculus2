#---------------------------------
# 7- Soybean Moratotium - LUC Calculus
# - Pasture to soybean (deforested before 2008)
#---------------------------------
# 1. All locations (pixels) that are soybean in a year?
# 2. In the past this location (pixel) was pasture in any time?
# 3. This location (pixel) was deforested before 2008? Soy Moratorium.
#
# o = geo-objects, the own df_input data.frame
#---------------------------------

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

number_Past_before_2008 <- list(NULL)

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
  raster.data <- raster.tb$r_objs[[1]][[1]]

  # ------------- define variables to plot raster -------------
  # original label - see QML file, same order
  label2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

  # create timeline with classified data from SVM method
  timeline2 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

  # soy moratorium
  timeline1 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01"))

  classesDegPas <- c("Cerrado", "Pasture")
  # intereting classes
  soybean_before.df <- NULL

  message("Start Compute Cerrado and Pasture Before 2008 ...\n")
  # along of all classes
  # system.time(
  for(x in 2:length(timeline2)){
    #x = 7
    t_1 <- timeline1[x-1]
    t_2 <- timeline2[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    for( z in 1:length(classesDegPas)){
      cat(classesDegPas[z], "\n")

      soybean.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = classesDegPas[z], #"Pasture",
                                    time_interval = c(t_2,t_2),
                                    relation_interval = "equals", label = label2, timeline = timeline)

      # pasture.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Cerrado", ### Pasture
      #                               time_interval = c(timeline1[1],t_1),
      #                               relation_interval = "contains", label = label2, timeline = timeline)

      forest.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                                   time_interval = c(timeline1[1],t_1),
                                   relation_interval = "equals", label = label2, timeline = timeline)

      #fores_past.temp <- lucC_relation_occurs(pasture.df, forest.df)

      temp <- lucC_relation_precedes(soybean.df, forest.df) #fores_past.temp)

      if (!is.null(temp)) {
        tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_2)
      } else {
        tempF <- NULL
      }
      soybean_before.df <- lucC_merge(soybean_before.df, tempF)
    }
  }
  cat("\n")
  # )

  # Soybean_Before_2008 <- soybean_before.df
  # Soybean_Before_2008[ Soybean_Before_2008 == "Soy" ] <- "Soybean_Before_2008"
  # head(Soybean_Before_2008)
  message("Add to list index ", y, "... \n")

  number_Past_before_2008[[y]] <- soybean_before.df

  message("Prepare image 1 ...\n")
  lucC_save_raster_result(raster_obj = raster.data, data_mtx = soybean_before.df, timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/Calc_Past_Before_2008/", file_name, sep = ""), as_RasterBrick = FALSE)

  message("Prepare image 2 ...\n")
  lucC_save_raster_result(raster_obj = raster.data, data_mtx = soybean_before.df, timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/Calc_Past_Before_2008/", file_name, sep = ""), as_RasterBrick = TRUE)

  # clear environment, except these elements
  rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "number_Past_before_2008"))])
  gc()

  message("--------------------------------------------------\n")
}

message("Save data as list in .rda file ...\n")
#save to rda file
save(number_Past_before_2008, file = "~/TESTE/MT/Calc_Past_Before_2008/number_Past_before_2008.rda")

# #Stop clusters
# parallel::stopCluster(cl)

# end time
print(Sys.time() - start.time)

rm(number_Past_before_2008)
gc()

#----------------------------------------------------
# Save results as measures
#----------------------------------------------------


# start time
start.time <- Sys.time()


load(file = "~/TESTE/MT/Calc_Past_Before_2008/number_Past_before_2008.rda")

output_freq <- lucC_extract_frequency(data_mtx.list = number_Past_before_2008, cores_in_parallel = 6)
output_freq

#----------------------
# # plot results
# lucC_plot_bar_events(data_frequency = output_freq,
#                      pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)
#
# lucC_plot_frequency_events(data_frequency = output_freq,
#                      pixel_resolution = 231.656, custom_palette = FALSE)

# Compute values
measuresPast_Before_2008 <- lucC_result_measures(data_frequency = output_freq, pixel_resolution = 231.656)
measuresPast_Before_2008

write.table(x = measuresPast_Before_2008, file = "~/TESTE/MT/Calc_Past_Before_2008/measuresPast_Before_2008.csv", quote = FALSE, sep = ";", row.names = FALSE)

save(measuresPast_Before_2008, file = "~/TESTE/MT/Calc_Past_Before_2008/measuresPast_Before_2008.rda")


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
lucC_merge_rasters(path_open_GeoTIFFs = "~/TESTE/MT/Calc_Past_Before_2008/All_blocks_Past_Before", number_raster = 4, pattern_name = "New_New_Raster_Splitted_", is.rasterBrick = TRUE)
# save each layer of brick as images
lucC_save_rasterBrick_layers(path_name_GeoTIFF_Brick = "~/TESTE/MT/Calc_Past_Before_2008/All_blocks_Past_Before/Mosaic_New_New_Raster_Splitted_.tif")


# end time
print(Sys.time() - start.time)




