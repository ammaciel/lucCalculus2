library(lucCalculus)

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster GeoTIFF classified previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/raster_sample_MT", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files
file <- c("inst/extdata/raster/raster_sample_MT.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

# library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sample_region", timeline = timeline, bands = "ndvi")
raster.tb

# new variable with raster object
rb_sits <- raster.tb$r_objs[[1]][[1]]

# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water"))

# original colors set - see QML file, same order
colors_1 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1" )

# plot rasterBrick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 6)

#-------------
# # alter attributes using labels
# rb_sits@data@attributes <- lapply(rb_sits@data@attributes, function(x)  {x <- data.frame(ID = c(1:length(label)), category = label)} )
# rasterVis::levelplot(rb_sits, col.regions=colors) # par.settings=rasterVis::RdBuTheme


#----------------------------
# 2- LUC Calculus with Allen's interval relations
#----------------------------

#------------- tests - intervals before, meets and follows -- Allen's relations
a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
                     time_interval = c("2001-09-01","2007-09-01"),
                     relation_interval = "equals", label = label, timeline = timeline)
a


b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
                     time_interval = c("2012-09-01","2013-09-01"),
                     relation_interval = "contains", label = label, timeline = timeline)
b

# before
c <- lucC_relation_before(a, b)
#c <- lucC_relation_after(b, a)
#c <- lucC_relation_meets(a, b)
#c <- lucC_relation_met_by(b, a)
#c <- lucC_relation_starts(a, b)
#c <- lucC_relation_started_by(b, a)
#c <- lucC_relation_finishes(b, a)
#c <- lucC_relation_finished_by(a, b)
#c <- lucC_relation_during(a, b)
#c <- lucC_relation_equals(a, b)
c
lucC_plot_sequence_events(c, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(c, custom_palette = FALSE, pixel_resolution = 232, side_by_side = TRUE, legend_text = "Legend")

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = c, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE) #, shape_point = "#")


#----------------------------
# 3- LUC Calculus - verify for secondary vegetation
#----------------------------
# 1. RECUR predicate indicates a class that appear again
forest_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                    time_interval1 = c("2001-09-01","2001-09-01"),
                                    time_interval2 = c("2003-09-01","2016-09-01"),
                                    label = label, timeline = timeline)
head(forest_recur)

#-------------------
# plot some results from RECUR
lucC_plot_sequence_events(forest_recur, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(forest_recur, custom_palette = FALSE, legend_text = "Legend:")

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = forest_recur,
                        timeline = timeline, label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE) #, shape_point = "#")
#-------------------

# 2. EVOLVE to verify Forest class that occurs after a different class in 2001
forest_evolve <- NULL

# classes without Forest based on original label
classes <- as.character(c("Cerrado", "Fallow_Cotton", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water"))

system.time(
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
)
rm(temp, i)
#df <- forest_evolve
#-------------------
# plot some results from EVOLVE
lucC_plot_sequence_events(forest_evolve, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(forest_evolve, custom_palette = FALSE, legend_text = "Legend:")

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = forest_evolve,
                        timeline = timeline, label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE) #, shape_point = "#")
#-------------------

# 3. Merge both forest_recur and forest_evolve datas
system.time(forest_secondary <- lucC_merge(forest_evolve, forest_recur))
head(forest_secondary)

# plot
lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
head(forest_sec)

rm(forest_recur, forest_evolve, forest_secondary, raster.tb)
# plot
lucC_plot_bar_events(forest_sec, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_sec, #forest_sec,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE) #, shape_point = ".")

# create images output
lucC_save_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_sec,       # without 2001
                        timeline = timeline, label = label, path_raster_folder = "~/Desktop/For_sec")         # new pixel value


#----------------------------
# 4 - Update original raster to add new pixel value
#----------------------------

num_label <- length(label) + 1
# 1. update original RasterBrick with new class
system.time(rb_sits_new <- lucC_raster_update(raster_obj = rb_sits,
                                  data_mtx = forest_sec,       # without 2001
                                  timeline = timeline,
                                  class_to_replace = "Forest",  # only class Forest
                                  new_pixel_value = num_label))         # new pixel value

head(rb_sits_new)

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF RasterBrick
lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_new,
                  path_raster_folder = "inst/extdata/raster/raster_sampleSecVeg", as_RasterBrick = FALSE ) # FALSE before
                  #path_raster_folder = "~/Desktop/raster_sampleSecVeg", as_RasterBrick = FALSE ) # FALSE before
#------------
# create a RasterBrick from individual raster GeoTIFF, case saved as separate layers
#lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/raster_sampleSecVeg", path_save_RasterBrick = "inst/extdata/raster")

# open files
#file <- c("inst/extdata/raster/raster_sampleSecVeg.tif")
file <- c("inst/extdata/raster/raster_sampleSecVeg1/New_raster_sampleSecVeg1.tif")

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

# library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sample_region_SecVeg", timeline = timeline, bands = "ndvi")

# new variable with raster object
rb_sits2 <- raster.tb$r_objs[[1]][[1]]

# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

# original colors set - see QML file, same order
colors_2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

# plot rasterBrick
lucC_plot_raster(raster_obj = rb_sits2,
                 timeline = timeline, label = label2,
                 custom_palette = TRUE, RGB_color = colors_2, plot_ncol = 6)


#----------------------------
# 5- Discover Forest and Secondary vegetation - LUC Calculus
#----------------------------

label2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

# original colors set - see QML file, same order
colors_2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

secondary.mtx <- lucC_pred_holds(raster_obj = rb_sits2, raster_class = "Secondary_Vegetation",
                                 time_interval = c("2001-09-01","2016-09-01"),
                                 relation_interval = "contains", label = label2, timeline = timeline)
head(secondary.mtx)

forest.mtx <- lucC_pred_holds(raster_obj = rb_sits2, raster_class = "Forest",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains", label = label2, timeline = timeline)
head(forest.mtx)

Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
head(Forest_secondary.mtx)

Forest_secondary.mtx <- lucC_merge(secondary.mtx, forest.mtx)
#head(Forest_secondary.mtx)
number_SV_For <- list()
number_SV_For[[3]] <- Forest_secondary.mtx

output_freq <- lucC_extract_frequency(data_mtx.list = number_SV_For, cores_in_parallel = 6)
num

# plot results
lucC_plot_bar_events(data_mtx = Forest_secondary.mtx,
                     pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# Compute values
measuresFor_Sec <- lucC_result_measures(data_mtx = Forest_secondary.mtx, pixel_resolution = 232)
measuresFor_Sec

# plot
lucC_plot_raster(raster_obj = rb_sits2, timeline = timeline,
                 label = label2, custom_palette = TRUE,
                 RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)



#----------------------------
# 6- Degradation after Forest and Secondary vegetation - LUC Calculus
#----------------------------
library(lucCalculus)

options(digits = 12) # always put this element

file <- c("inst/extdata/raster/raster_sampleSecVeg1/New_raster_sampleSecVeg1.tif")

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

# library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Sample_region_SecVeg", timeline = timeline, bands = "ndvi")

# new variable with raster object
rb_sits2 <- raster.tb$r_objs[[1]][[1]]

label2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

# original colors set - see QML file, same order
colors_2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

# plot
lucC_plot_raster(raster_obj = rb_sits2, timeline = timeline,
                 label = label2, custom_palette = TRUE,
                 RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)

#-----------------------------------
# 2. EVOLVE to verify Forest class that occurs after a different class in 2001
# Cerrado after Forest
CerrFor_evolve <- NULL
# classes without Forest based on original label
system.time(CerrFor_evolve <- lucC_pred_evolve(raster_obj = rb_sits2, raster_class1 = "Forest",
                             time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                             raster_class2 = "Cerrado",
                             time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                             label = label2, timeline = timeline))
head(CerrFor_evolve)

CerrFor_evolve <- lucC_remove_columns(data_mtx = CerrFor_evolve, name_columns = "2001-09-01")

lucC_plot_raster_result(raster_obj = rb_sits2, data_mtx = CerrFor_evolve,timeline = timeline,
                 label = label2, custom_palette = TRUE,
                 RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)

# Cerrado after Secondary areas
# constant
message("Start Cerrado holds ...\n")
# secondary and forest
cer.mtx <- lucC_pred_holds(raster_obj = rb_sits2, raster_class = "Cerrado",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains", label = label2, timeline = timeline)
head(cer.mtx)


message("Start Secondary_vegetation holds ...\n")
vsec.mtx <- lucC_pred_holds(raster_obj = rb_sits2, raster_class = "Secondary_Vegetation",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains", label = label2, timeline = timeline)
head(vsec.mtx)

message("Start Double cropping occurs ...\n")
CerVS.mtx <- lucC_relation_occurs(first_raster = cer.mtx, second_raster = vsec.mtx)

# only double and single with no occurs
cer_only.mtx <- dplyr::anti_join(as.data.frame(cer.mtx), as.data.frame(CerVS.mtx), by = c("x","y"))
VS_only.mtx <- dplyr::anti_join(as.data.frame(vsec.mtx), as.data.frame(CerVS.mtx), by = c("x","y"))

# check number is ok
nrow(vsec.mtx)
nrow(CerVS.mtx)
nrow(vsec.mtx) - nrow(CerVS.mtx)
nrow(VS_only.mtx) # ok

nrow(cer.mtx)
nrow(CerVS.mtx)
nrow(cer.mtx) - nrow(CerVS.mtx)
nrow(cer_only.mtx)

# only cerrado and no secondary vegetation
CerVS.mtx

CerVS_2.mtx <- as.data.frame(CerVS.mtx) %>%
  tidyr::gather(variable, value, -x, -y) %>%
  dplyr::filter(value == "Cerrado") #%>%
  #tidyr::spread(variable, value) %>%

CerrFor_evolve2 <- as.data.frame(CerrFor_evolve) %>%
  tidyr::gather(variable, value, -x, -y)

Cerrado <- dplyr::bind_rows(CerVS_2.mtx, CerrFor_evolve2)
# remove duplicated lines
Cerrado <- Cerrado[!duplicated(Cerrado), ]
# return matrix format
result <- Cerrado %>%
  tidyr::spread(variable, value)


lucC_plot_raster_result(raster_obj = rb_sits2, data_mtx = CerVS.mtx, timeline = timeline,
                        label = label2, custom_palette = TRUE,
                        RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)

lucC_plot_raster_result(raster_obj = rb_sits2, data_mtx = CerVS_2.mtx, timeline = timeline,
                        label = label2, custom_palette = TRUE,
                        RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)

lucC_plot_raster_result(raster_obj = rb_sits2, data_mtx = result, timeline = timeline,
                        label = label2, custom_palette = TRUE,
                        RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)


# create images output
lucC_save_raster_result(raster_obj = rb_sits2,
                        data_mtx = result,       # without 2001
                        timeline = timeline, label = label2, path_raster_folder = "~/Desktop/rasterItanhangaDegr") # new pixel value


#----------------------------
# 3- Update original raster to add new pixel value
#----------------------------

n_label <- length(label2) + 1

# 1. update original RasterBrick with new class
rb_sits_new <- lucC_raster_update(raster_obj = rb_sits2,
                                  data_mtx = rb_sits2,       # without 2001
                                  timeline = timeline,
                                  class_to_replace = "Cerrado",  # only class Forest
                                  new_pixel_value = n_label)         # new pixel value

head(rb_sits_new)

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF images
lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_new,
                  path_raster_folder = "inst/extdata/raster/rasterItanhangaDegr", as_RasterBrick = FALSE)
#path_raster_folder = "~/Desktop/rasterItanhangaSecVeg", as_RasterBrick = FALSE)


















