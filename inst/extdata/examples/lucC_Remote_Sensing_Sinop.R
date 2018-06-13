library(lucCalculus)

options(digits = 12) # always put this element

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
# lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/MT_Chronos/Sinop_2005_2016", path_save_RasterBrick = "~/Desktop/MT_Chronos/Sinop_2005_2016")

# ------------- define variables to use in sits -------------
# open files
file <- c("~/Desktop/MT_Chronos/Sinop_2005_2016/Sinop_2005_2016.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sinop", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]
rb_sits


# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_area", "Water", "Secondary_vegetation"))

#label <- as.character(c("Cerrado", "Single_cropping", "Forest", "Pasture", "Double_cropping", "Double_cropping", "Single_cropping", "Double_cropping", "Double_cropping", "Sugarcane", "Urban_area", "Water", "Secondary_vegetation"))

# colors
colors_1 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red")

# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 4)


#----------------------------
# 2- Discover Single and Double cropping - LUC Calculus
#----------------------------

message("Start Single cropping holds ...\n")
# secondary and forest
single.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Single_cropping",
                                 time_interval = c("2005-09-01","2016-09-01"),
                                 relation_interval = "contains", label = label, timeline = timeline)
#head(secondary.mtx)

message("Start Double cropping holds ...\n")
double.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Double_cropping",
                              time_interval = c("2005-09-01","2016-09-01"),
                              relation_interval = "contains", label = label, timeline = timeline)
#head(forest.mtx)

message("Merge Forest and Secondary Vegetation ...\n")
holds.mtx <- lucC_merge(single.mtx, double.mtx)
#head(Forest_secondary.mtx)

message("Save image in provided path ...\n")
# save result of secondary vegetation

file_name <- basename(tools::file_path_sans_ext(file))

message("Prepare image 1 ...\n")
lucC_save_raster_result(raster_obj = rb_sits, data_mtx = holds.mtx, timeline = timeline, label = label, path_raster_folder = paste0("~/Desktop/MT_Chronos/Sinop_2005_2016/Holds_SC_DC/", file_name, sep = ""), as_RasterBrick = FALSE)

message("Prepare image 2 ...\n")
lucC_save_raster_result(raster_obj = rb_sits, data_mtx = holds.mtx, timeline = timeline, label = label, path_raster_folder = paste0("~/Desktop/MT_Chronos/Sinop_2005_2016/Holds_SC_DC/", file_name, sep = ""), as_RasterBrick = TRUE)


.

#-------------------------------
library(lucCalculus)

options(digits = 12) # always put this element

# open files
file <- c("~/Desktop/MT_Chronos/Sinop_2005_2016/Holds_SC_DC/Sinop_2005_2016/New_Sinop_2005_2016.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

# library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sinop", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]
rb_sits

# change pixel of water by Cerrado, because this class doesn't exist in this municipality
#rb_sits <- raster::reclassify(rb_sits, cbind(0, 0))
#rb_sits <- raster::reclassify(rb_sits, cbind(0, NA))
#rb_sits
#rb_sits[is.na(rb_sits)] <- 0

label <- as.character(c("NA","Single_cropping", "NA", "NA", "Double_cropping"))

# colors
colors_1 <- c("cyan","green","cyan","cyan","blue")


# raster::plot(rb_sits)
# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 4)

# raster::plot(rb_sits) # rb_sits$New_Sinop_2005_2016.1

.

# constant
message("Start Single cropping holds ...\n")
# secondary and forest
single.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Single_cropping",
                              time_interval = c("2005-09-01","2016-09-01"),
                              relation_interval = "contains", label = label, timeline = timeline)
head(single.mtx)


message("Start Double cropping holds ...\n")
double.mtx <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Double_cropping",
                              time_interval = c("2005-09-01","2016-09-01"),
                              relation_interval = "contains", label = label, timeline = timeline)
head(double.mtx)

message("Start Double cropping occurs ...\n")
sindou.mtx <- lucC_relation_occurs(first_raster = single.mtx, second_raster = double.mtx)

# only double and single with no occurs
double_only.mtx <- dplyr::anti_join(as.data.frame(double.mtx), as.data.frame(sindou.mtx), by = c("x","y"))
single_only.mtx <- dplyr::anti_join(as.data.frame(single.mtx), as.data.frame(sindou.mtx), by = c("x","y"))

# check number is ok
nrow(double.mtx)
nrow(sindou.mtx)
nrow(double.mtx) - nrow(sindou.mtx)
nrow(double_only.mtx) # ok

nrow(single.mtx)
nrow(sindou.mtx)
nrow(single.mtx) - nrow(sindou.mtx)
nrow(single_only.mtx)

# new matrix with
total.mtx <- rbind(double_only.mtx, single_only.mtx, sindou.mtx)

indexClassesMin <- function(x, class){
  if(length(which(x == class)) == 0)
    return(0)
  else
    return(min(which(x == class)))
}

indexClasses <- function(x, class){
  if(length(which(x == class)) == 0)
    return(0)
  else
    return(which(x == class))
}


# #trajec.df
class_traj <- function(x){
  dplyr::case_when(all(((x$idxSingle != 0) & (x$idxDouble != 0)) & x$perNA <= 30 & all(max(x$idxSingle) < min(x$idxDouble)))  ~ "2", #"Intensification",
                   all((x$idxSingle != 0) & (x$idxDouble != 0) & x$perNA <= 30 & all(max(x$idxDouble) < min(x$idxSingle)))  ~ "3", #"Reduction",
                   all((x$idxSingle != 0) & (x$idxDouble != 0) & x$perNA <= 30 & (all(max(x$idxDouble) > min(x$idxSingle)) | all(max(x$idxSingle) > min(x$idxDouble)))) ~ "4", #"Interchange",
                   all(x$perNA >= 90)  ~ "6", #"Anomaly",
                   all((x$perSin2005to2016 >= 80 | x$perDou2005to2016 >= 80) & x$perNA <= 25) ~ "1", #"Stable",
                   all(x$perNA > 25 & x$perNA < 90)  ~ "5", #"Expansion",
                   FALSE ~ as.character(NA))
}

#rm(trajec.df)
#total2.mtx <- total.mtx[c(1885:1924), ] #1:20,16581:16616,25081:25116, 34475:34487

trajec.df <- total.mtx %>%
  as.data.frame() %>%
  dplyr::mutate(na = apply(., 1, function(x) which(is.na(x)))) %>%
  dplyr::mutate(perNA = apply(., 1, function(x) (length(which(is.na(x[c(3:14)])))/11) *100 ))  %>%
  dplyr::mutate(perSin2005to2016 = apply(., 1, function(x) (length(which(x[c(3:14)] == "Single_cropping"))/12) *100 ))  %>%
  dplyr::mutate(perDou2005to2016 = apply(., 1, function(x) (length(which(x[c(3:14)] == "Double_cropping"))/12) *100 ))  %>%
  dplyr::mutate(idxSingle = apply(., 1, function(x) indexClasses(x, "Single_cropping"))) %>%
  dplyr::mutate(idxDouble = apply(., 1, function(x) indexClasses(x, "Double_cropping"))) %>%
  dplyr::mutate(class = apply(., 1, function(x) class_traj(x)))  %>%
  dplyr::select(-na, -perNA, -perSin2005to2016, -perDou2005to2016, -idxSingle, -idxDouble)

#head(trajec.df)

#trajec.df

# create raster
data <- dplyr::select(trajec.df, x, y, class)
dfr <- raster::rasterFromXYZ(data)  #Convert first two columns as lon-lat and third as value
#raster::plot(dfr)
dfr


# http://www.color-hex.com/color-palette/60816 - cores
# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label <- as.character(c("Stable", "Intensification", "Reduction", "Interchange", "Expansion", "Anomaly"))

#label <- as.character(c("Cerrado", "Single_cropping", "Forest", "Pasture", "Double_cropping", "Double_cropping", "Single_cropping", "Double_cropping", "Double_cropping", "Sugarcane", "Urban_area", "Water", "Secondary_vegetation"))

# colors
colors_1 <- c("#1a80b6", "#fedc45","#b61a32", "#67fe45", "#ffaeae", "#373d41" ) #"#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6"
timeline <- "2016-01-01"

# plot raster brick
lucC_plot_raster(raster_obj = dfr,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 4, legend_text = "")

data1 <- data
colnames(data1) <- c("x", "y", "2016-09-01")

colors_1 <- c("#1a80b6", "#897714", "#67fe45", "#b6501a", "#373d41" ) #
lucC_plot_bar_events(data1, custom_palette = FALSE, RGB_color = colors_1, pixel_resolution = 231.656, side_by_side = TRUE)



.

.
