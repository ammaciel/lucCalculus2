
library(lucCalculus)

# always
options(digits = 12)

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterItanhangaSecVeg", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c("inst/extdata/raster/rasterItanhangaSecVeg.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "ItaVegSec", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits2 <- raster.tb$r_objs[[1]][[1]]
rb_sits2

# new class Seconary vegetation
label2 <- as.character(c("Cerrado", "Double_cropping", "Single_cropping", "Forest", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water", "Secondary_vegetation"))
label2

# colors
colors_2 <- c("#b3cc33", "#cd6155", "#e6b0aa", "#228b22", "#7ecfa4", "green", "#afe3c8", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")

# plot raster brick
lucC_plot_raster(raster_obj = rb_sits2,
                 timeline = timeline, label = label2,
                 custom_palette = TRUE, RGB_color = colors_2, plot_ncol = 6)






#---------------------------------
# 7- Soybean Moratotium - LUC Calculus
# - Pasture to soybean (deforested before 2006)
#---------------------------------
# 1. All locations (pixels) that are soybean in a year?
# 2. In the past this location (pixel) was pasture in any time?
# 3. This location (pixel) was deforested before 2006? Soy Moratorium.
#
# o = geo-objects, the own df_input data.frame
#---------------------------------

#label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask", "Secondary_vegetation"))

label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture", "Pasture", "Pasture", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Water", "Water", "Secondary_vegetation"))
label2

# Soy moratorium
timeline1 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01", "2006-09-01"))
# create timeline with classified data from SVM method
timeline2 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

# # create timeline with classified data from SVM method
# timeline2 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
# # soy moratorium
# timeline1 <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01", "2008-09-01"))

# intereting classes
soybean_before.df <- NULL

raster.data <- rb_sits2

# along of all classes
# system.time(
for(x in 2:length(timeline2)){
  # x = 6
  t_1 <- timeline1[x-1]
  t_2 <- timeline2[x]
  cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

  soybean.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Soybean",
                                time_interval = c(t_2,t_2),
                                relation_interval = "equals", label = label2, timeline = timeline)

  pasture.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                                time_interval = c(timeline1[1],t_1),
                                relation_interval = "contains", label = label2, timeline = timeline)

  forest.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Forest",
                               time_interval = c(timeline1[1],t_1),
                               relation_interval = "contains", label = label2, timeline = timeline)

  fores_past.temp <- lucC_relation_occurs(pasture.df, forest.df)

  temp <- lucC_relation_precedes(soybean.df, fores_past.temp)
  #temp1 <- lucC_relation_follows(fores_past.temp, soybean.df)

  if (!is.null(temp)) {
    tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_2)
  } else {
    tempF <- NULL
  }
  soybean_before.df <- lucC_merge(soybean_before.df, tempF)
}
#)


#Soybean_Before_2006 <- soybean_before.df
#Soybean_Before_2006[ Soybean_Before_2006 == "Soybean" ] <- "Soybean_Before_2006"
#head(Soybean_Before_2006)

# remove(temp, soybean_before.df, forest.df, pasture.df, soybean.df, fores_past.temp, tempF, t_1, t_2, x)

# plot results
lucC_plot_bar_events(data_mtx = soybean_before.df, pixel_resolution = 231.656, custom_palette = FALSE, side_by_side = TRUE)

## Compute values
# Soybean_Before_2006.tb <- lucC_result_measures(data_mtx = Soybean_Before_2006, pixel_resolution = 231.656)
# Soybean_Before_2006.tb
#
# # plot
# colors_3 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#7ecfa4", "#b6a896", "#3a3aff", "red", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff", "red")
#
# lucC_plot_raster(raster_obj = raster.data, timeline = timeline,
#                  label = label2, custom_palette = TRUE,
#                  RGB_color = colors_3, relabel = FALSE, plot_ncol = 6)
#
# lucC_plot_raster_result(raster_obj = raster.data, data_mtx = Soybean_Before_2006, timeline = timeline,
#                  label = label2, custom_palette = TRUE,
#                  RGB_color = colors_3, relabel = FALSE, plot_ncol = 6, shape_point = ".")
#




