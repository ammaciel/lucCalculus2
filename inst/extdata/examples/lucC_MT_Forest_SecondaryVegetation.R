library(lucCalculus)

options(digits = 12)

# all files in folder
all.the.files <- list.files("~/TESTE/MT/MT_SecVeg", full=TRUE, pattern = ".tif")
all.the.files

#-------------
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
#-------------

# start time
start.time <- Sys.time()

number_SV_For <- list()
#for_sv.tb <- NULL

#for_sv.tb <- foreach(i = 1:length(all.the.files), .combine=rbind, .packages= c("lucCalculus")) %dopar%  {
for (i in 1:length(all.the.files)) {
  # file
  file <- all.the.files[i]

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
  label2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

  # original colors set - see QML file, same order
  # colors_2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

  file_name <- basename(tools::file_path_sans_ext(file))

  message("Start Secondary Vegetation holds ...\n")
  # secondary and forest
  secondary.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Secondary_Vegetation",
                                   time_interval = c("2001-09-01","2016-09-01"),
                                   relation_interval = "contains", label = label2, timeline = timeline)
  #head(secondary.mtx)

  message("Start Forest holds ...\n")
  forest.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
                                time_interval = c("2001-09-01","2016-09-01"),
                                relation_interval = "contains", label = label2, timeline = timeline)
  #head(forest.mtx)

  message("Merge Forest and Secondary Vegetation ...\n")
  Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
  #head(Forest_secondary.mtx)

  number_SV_For[[i]] <- Forest_secondary.mtx

  message("Save image in provided path ...\n")
  # save result of secondary vegetation
  lucC_save_raster_result(raster_obj = rb_sits, data_mtx = Forest_secondary.mtx, timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/MT_SecVeg/", file_name, sep = ""))

  # clear environment, except these elements
  rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "end.time", "number_SV_For", "i"))])
  gc()
  gc()

  message("--------------------------------------------------\n")
}

message("Save data as list in .rda file ...\n")
#save to rda file
save(number_SV_For, file = "~/TESTE/MT/HoldsForestSecVeg/number_SV_For.rda")

# #Stop clusters
# parallel::stopCluster(cl)

# end time
print(Sys.time() - start.time)

rm(number_SV_For)
gc()

#----------------------------------------------------
# Save results as measures
#----------------------------------------------------


# start time
start.time <- Sys.time()


load(file = "~/TESTE/MT/HoldsForestSecVeg/number_SV_For.rda")

output_freq <- lucC_extract_frequency(data_mtx.list = number_SV_For, cores_in_parallel = 6)
#output_freq

#----------------------
# # plot results
# lucC_plot_bar_events(data_frequency = output_freq,
#                      pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)
#
# lucC_plot_frequency_events(data_frequency = output_freq,
#                      pixel_resolution = 231.656, custom_palette = FALSE)

# Compute values
measuresFor_Sec <- lucC_result_measures(data_frequency = output_freq, pixel_resolution = 231.656)
measuresFor_Sec

write.table(x = measuresFor_Sec, file = "~/TESTE/MT/HoldsForestSecVeg/measuresFor_Sec.csv", quote = FALSE, sep = ";", row.names = FALSE)

save(measuresFor_Sec, file = "~/TESTE/MT/HoldsForestSecVeg/measuresFor_Sec.rda")


# end time
print(Sys.time() - start.time)


#-----------------------------

