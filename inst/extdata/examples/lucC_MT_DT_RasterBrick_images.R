# RasterBrick Images

library(lucCalculus)

options(digits = 12)

# all files in folder
all.the.files <- list.files("~/TESTE/MT/MT_SecVeg", full=TRUE, pattern = ".tif")
all.the.files

# start time
start.time <- Sys.time()
load("~/TESTE/MT/DLUCCerradoOthers/number_Cerr_others.rda")
load("~/TESTE/MT/DLUCForPasSoy/number_F_P_Soy.rda")
load("~/TESTE/MT/DLUCPastSoySV/number_P_Soy_SV.rda")
load("~/TESTE/MT/DLUCSoyOthers/number_Soy_others.rda")

#convert.df <- foreach(y = 1:length(all.the.files), .combine=rbind, .packages= c("lucCalculus")) %dopar% {
for (y in 1:length(all.the.files)) {
  # file
  file <- all.the.files[y]

  # create timeline with classified data from SVM method
  timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

  # original label - see QML file, same order
  label2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

  file_name <- basename(tools::file_path_sans_ext(file))

  # library(sits)
  # create a RasterBrick metadata file based on the information about the files
  raster.tb <- sits::sits_coverage(service = "RASTER", name = file_name, timeline = timeline, bands = "ndvi", files = file)

  message("\n--------------------------------------------------\n")
  message(paste0("Load RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

  # new variable with raster object
  rb_sits <- raster.tb$r_objs[[1]][[1]]

  message("Cerrado to others ...\n")
  lucC_save_raster_result(raster_obj = rb_sits, data_mtx = number_Cerr_others[[y]], timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/DLUCCerradoOthers/", file_name, sep = ""), as_RasterBrick = TRUE)

  message("Forest to others ...\n")
  lucC_save_raster_result(raster_obj = rb_sits, data_mtx = number_F_P_Soy[[y]], timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/DLUCForPasSoy/", file_name, sep = ""), as_RasterBrick = TRUE)

  message("Pasture to others ...\n")
  lucC_save_raster_result(raster_obj = rb_sits, data_mtx = number_P_Soy_SV[[y]], timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/DLUCPastSoySV/", file_name, sep = ""), as_RasterBrick = TRUE)

  message("Soy to others ...\n")
  lucC_save_raster_result(raster_obj = rb_sits, data_mtx = number_Soy_others[[y]], timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/DLUCSoyOthers/", file_name, sep = ""), as_RasterBrick = TRUE)

  # message("Secondary Vegetation to others ...\n")
  # load("~/TESTE/MT/DLUCSeconVegetOthers/number_SecVeg_others.rda")
  # lucC_save_raster_result(raster_obj = rb_sits, data_mtx = number_SecVeg_others[[y]], timeline = timeline, label = label2, path_raster_folder = paste0("~/TESTE/MT/DLUCSeconVegetOthers/", file_name, sep = ""), as_RasterBrick = TRUE)

  # clear environment, except these elements
  #rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "number_SecVeg_others"))])
  gc()

  message("--------------------------------------------------\n")


}

# end time
print(Sys.time() - start.time)

