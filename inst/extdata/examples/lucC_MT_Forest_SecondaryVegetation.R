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
#file <- c("~/Desktop/INPE_2018/Classi_MT_SVM/raster_mt_by_year_2004.tif")

# all files in folder
all.the.files <- list.files("~/Desktop/INPE_2018/Classi_MT_SVM/raster_2SecVeg", full=TRUE, pattern = ".tif")
all.the.files

number_SV_For <- list()
#number_SV_For <- NULL

for (i in 1:length(all.the.files)) {

  # file
  # file <- all.the.files[15]
  file <- all.the.files[i]
  #file

  # create timeline with classified data from SVM method
  timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

  file_name <- basename(tools::file_path_sans_ext(file))

  # library(sits)
  # create a RasterBrick metadata file based on the information about the files
  raster.tb <- sits::sits_coverage(service = "RASTER", name = file_name, timeline = timeline, bands = "ndvi", files = file)

  message(paste0("\nLoad RasterBrick! Name: ", raster.tb$name, " ...\n", sep = ""))

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


  # message("Quantity of Secondary vegetation and Forest Ok! ...\n")

  message("--------------------------------------------------\n")
  # clear environment, except these elements
  rm(list=ls()[!(ls() %in% c('all.the.files', "start.time", "end.time", "number_SV_For"))])
  gc(verbose = TRUE)
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

head(number_SV_For)
# library(magrittr)

meltFromList <- function(x){
  raster_data <- reshape2::melt(as.data.frame(x), id.vars = c("x","y"), na.rm = TRUE) #%>%
    #stats::na.omit()

  raster_data$x = as.numeric(as.character(raster_data$x)) # as.numeric(levels(point_df$x))[point_df$x]
  raster_data$variable = as.character(as.character(raster_data$variable))
  raster_data$y = as.numeric(as.character(raster_data$y))

  raster_data <- raster_data[!duplicated(raster_data), ]

  # count number of values
  result <- data.frame(table(lubridate::year(raster_data$variable), raster_data$value))

  return(result)
}

# Just iterate over the file paths
# Calling one function that reads in the file and melts
system.time(out <- lapply(number_SV_For, meltFromList))

start <- proc.time() # Start clock
# output.tb <- data.frame(do.call("rbind", parallel::mclapply( X = number_SV_For,
#                                                             mc.cores=3, #parallel::detectCores(),
#                                                             FUN = meltFromList)))
output.tb <- parallel::mclapply(X = number_SV_For, mc.cores=3, FUN = meltFromList)

time_elapsed_series <- proc.time() - start # End clock
time_elapsed_series

output.tb

out <- output.tb

do.call(rbind,lapply(out, function(x) {
  colSums(x$Freq[,sapply(x$Freq, is.numeric)])
}))



#----------------------
# plot results
lucC_plot_bar_events(data_mtx = number_SV_For,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# Compute values
measuresFor_Sec <- lucC_result_measures(data_mtx = number_SV_For, pixel_resolution = 232)
measuresFor_Sec





