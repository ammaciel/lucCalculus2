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
rb_sits <- raster.tb$r_objs[[1]][[1]]

label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

# original colors set - see QML file, same order
colors <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

# # new class Seconary vegetation
# label <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))
#
# # colors
# colors <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red") # "#b3cc33" "#228b22", "#7ecfa4", "blue"

# plot
lucC_plot_raster(raster_obj = rb_sits, timeline = timeline,
                 label = label, custom_palette = TRUE,
                 RGB_color = colors, relabel = FALSE, plot_ncol = 6)

#-----------------------------------
# 2. EVOLVE to verify Cerrado class that occurs after a different class in 2001

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

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = vsec.mtx, timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors, relabel = FALSE, plot_ncol = 6)

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

# as.factor
str(degradation)
# degradation$x <- as.factor(degradation$x)
# degradation$y <- as.factor(degradation$y)
#str(degradation)

# remove
rm(CerVS_2.mtx, CerrFor_evolve2, Cerrado)
gc()

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = degradation, timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors, relabel = FALSE, plot_ncol = 5) #, shape_point = ".")

# create images output
lucC_save_raster_result(raster_obj = rb_sits,
                        data_mtx = degradation,       # all years
                        timeline = timeline, label = label, path_raster_folder = "~/Desktop/Sample_Degrad")         # new pixel value

#----------------------------
# 3- Update original raster to add new pixel value
#----------------------------

n_label <- length(label) + 1

# 1. update original RasterBrick with new class
rb_sits_new <- lucC_raster_update(raster_obj = rb_sits,
                                  data_mtx = degradation,       # without 2001
                                  timeline = timeline,
                                  class_to_replace = "Cerrado",  # only class Forest
                                  new_pixel_value = n_label)         # new pixel value

#head(rb_sits_new)

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF RasterBrick
lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_new,
                  path_raster_folder = "~/Desktop/Sample_Degra_Brick", as_RasterBrick = TRUE ) # FALSE before


lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_new,
                  path_raster_folder = "~/Desktop/Sample_Degra_normal/", as_RasterBrick = FALSE ) # FALSE before

