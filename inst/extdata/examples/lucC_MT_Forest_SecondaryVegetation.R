library(lucCalculus)

# start time
start.time <- Sys.time()

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
# lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/INPE_2018/Classi_MT_SVM/raster_mt_by_year_2004", path_save_RasterBrick = "~/Desktop")

# ------------- define variables to use in sits -------------
# open files
#file <- c("~/Desktop/INPE_2018/Classi_MT_SVM/raster_mt_by_year_2004.tif")

# all files in folder
#all.the.files <- list.files("~/Desktop/INPE_2018/Classi_MT_SVM/raster_2SecVeg", full=TRUE, pattern = ".tif")
all.the.files <- list.files("~/TESTE/MT/MT_SecVeg", full=TRUE, pattern = ".tif")
all.the.files

# save as list
number_SV_For <- list()

for (i in 1:length(all.the.files)) {

  # file
  # file <- all.the.files[15]
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
  colors_2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

  file_name <- basename(tools::file_path_sans_ext(file))


  # secondary and forest
  secondary.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Secondary_Vegetation",
                                   time_interval = c("2001-09-01","2016-09-01"),
                                   relation_interval = "contains", label = label2, timeline = timeline)
  #head(secondary.mtx)

  forest.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
                                time_interval = c("2001-09-01","2016-09-01"),
                                relation_interval = "contains", label = label2, timeline = timeline)
  #head(forest.mtx)

  Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
  #head(Forest_secondary.mtx)

  number_SV_For[[i]] <- Forest_secondary.mtx

  # clear environment, except these elements
  rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "end.time", "number_SV_For"))])
  gc()
  gc()

  message("--------------------------------------------------\n")
}

#save to rda file
save(number_SV_For, file = "~/Desktop/INPE_2018/Classi_MT_SVM/raster_2SecVeg/number_SV_For.rda")


# end time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#---------------------
load(file = "~/Desktop/INPE_2018/Classi_MT_SVM/raster_2SecVeg/number_SV_For.rda")

output_freq <- lucC_extract_frequency(data_mtx.list = number_SV_For, cores_in_parallel = 3)
output_freq

#----------------------
# plot results
lucC_plot_bar_events(data_frequency = output_freq,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

lucC_plot_frequency_events(data_frequency = output_freq,
                     pixel_resolution = 232, custom_palette = FALSE)


# Compute values
measuresFor_Sec <- lucC_result_measures(data_frequency = output_freq, pixel_resolution = 232)
measuresFor_Sec


#-------------------------------------------

# all files in folder
all.the.files <- list.files("~/Desktop/INPE_2018/Classi_MT_SVM/raster_2SecVeg", full=TRUE, pattern = ".tif")
all.the.files

list_raster <- list()

for (i in 1:length(all.the.files)) {

  file <- all.the.files[i]

  # create timeline with classified data from SVM method
  timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

  file_name <- basename(tools::file_path_sans_ext(file))

  # library(sits)
  # create a RasterBrick metadata file based on the information about the files
  raster.tb <- sits::sits_coverage(service = "RASTER", name = file_name, timeline = timeline, bands = "ndvi", files = file)

  message(paste0("Load RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

  # new variable with raster object
  rb_sits <- raster.tb$r_objs[[1]][[1]]

  list_raster[[i]] <- rb_sits

}

list_raster$fun <- max
list_raster$na.rm <- TRUE

# mosaic
MT_mergeSV <- do.call(raster::mosaic, list_raster)

raster::writeRaster(MT_mergeSV, filename="~/Desktop/fig_TESE/figureMT.tif", format="GTiff", overwrite=TRUE)



#-----------
# open files
file <- c("~/Desktop/INPE_2018/Classi_MT_SVM/raster_2SecVeg/New_0000_0000.tif")

# all files in folder
#all.the.files <- list.files("~/Desktop/INPE_2018/Classi_MT_SVM/raster_1splitted", full=TRUE, pattern = ".tif")
#all.the.files

  # create timeline with classified data from SVM method
  timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

  file_name <- basename(tools::file_path_sans_ext(file))

  #library(sits)
  # create a RasterBrick metadata file based on the information about the files
  raster.tb <- sits::sits_coverage(service = "RASTER", files = file, name = file_name, timeline = timeline, bands = "ndvi")

  message(paste0("\nLoad RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

  # new variable
  rb_sits <- raster.tb$r_objs[[1]][[1]]

  label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

  load(file = "~/Desktop/INPE_2018/Classi_MT_SVM/raster_2SecVeg/number_SV_For.rda")
  b <- number_SV_For[[1]]

  data_new <- lucC_update_raster_result(raster_obj = rb_sits, data_mtx = b, timeline = timeline, label = label)

  data_new

lucC_save_GeoTIFF(raster_obj = rb_sits, data_mtx = data_new, path_raster_folder = "~/Desktop/raster_MT_0000", as_RasterBrick = FALSE)

lucC_plot_raster(rb_sits, timeline, label = label2 )

lucC_plot_raster_result(rb_sits, data_mtx = data_new, timeline, label = label, shape_point = ".", plot_ncol = 4 )




