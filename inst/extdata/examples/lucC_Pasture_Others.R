#----------------------------
# Header
#----------------------------
# devtools::install_github("ammaciel/lucCalculus")
library(lucCalculus)

# always
options(digits = 12)

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c("inst/extdata/raster/raster_sampleSecVeg1/New_raster_sampleSecVeg1.tif") #rasterItanhangaSecVeg.tif
file

# create timeline with classified data
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "SampleRegion", timeline = timeline, bands = "ndvi")
raster.tb

# # create a raster object associated to the file
# rb_sits2 <- raster::brick(file)
# rb_sits2

# associate only RasterBrick as new variable
rb_sits2 <- raster.tb$r_objs[[1]][[1]]
rb_sits2

# labels, as defined by the QML file, in same order
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

# original colors set - see QML file, same order
colors <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

# plot raster brick
lucC_plot_raster(raster_obj = rb_sits2,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors, plot_ncol = 6)


#----------------------------
# Application of the calculus
# 1- Extract locations, with different class, between two similar classes. For instance, 2001->Pasture, 2002->Other, 2003->Pasture
#----------------------------
# auxiliary variables
timeline1 <- timeline
# final data
past_final.df <- NULL
# raster input
raster.data <- rb_sits2
# set of other classes
classes <- c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Soybean", "Water", "Water", "Secondary_vegetation")

# disregard the first and last timeline
for(x in 2:length(timeline1[-16])){
  #x = 6
  t_1 <- timeline1[x-1] # 2001
  t_2 <- timeline1[x]   # 2002
  t_3 <- timeline1[x+1] # 2003
  cat(paste0(t_1, ", ", t_2, ", ", t_3, sep = ""), "\n")

  # along of all other classes
  for(z in seq_along(classes)){
    #z = 1
    print(classes[z])

    past1.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                                time_interval = c(t_1,t_1),
                                relation_interval = "equals", label = label, timeline = timeline) # 200x-1

    other.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = classes[z],
                                time_interval = c(t_2,t_2),
                                relation_interval = "equals", label = label, timeline = timeline) # 200x

    past2.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                                time_interval = c(t_3,t_3),
                                relation_interval = "equals", label = label, timeline = timeline) # 200x+1

    # Allen's relation MEETS, just pick up locations with relationship
    PastOther.temp <- lucC_relation_meets(past1.df, other.df)
    OtherPast.temp <- lucC_relation_meets(other.df, past2.df)

    # intersection between two times
    temp <- lucC_relation_occurs(PastOther.temp, OtherPast.temp)

    if (!is.null(temp)) {
      # select only the column, into data.frame, with different class between two Pasture classes
      tempF <- lucC_select_columns(data_mtx = temp, name_columns = t_2)
    } else {
      tempF <- NULL
    }
    # add to final data.frame
    past_final.df <- lucC_merge(past_final.df, tempF)
  }
}

rm(past1.df, past2.df, other.df, PastOther.temp, OtherPast.temp, t_1, t_2, t_3, timeline1, x, z, temp, tempF)
gc()

head(past_final.df)
class(past_final.df)

# plot raster and squares with results
lucC_plot_raster_result(raster_obj = rb_sits2, data_mtx = past_final.df,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors, plot_ncol = 6)


#----------------------------
# 2- Updating the original raster to replace new pixel value
#----------------------------

# 1. update original RasterBrick with new class
rb_sits_new <- lucC_raster_update(raster_obj = raster.data,
                                  data_mtx = past_final.df,       # <- from result of using HOLDS
                                  timeline = timeline,
                                  class_to_replace = "Cerrado",   # class
                                  new_pixel_value = 4)            # new pixel value
head(rb_sits_new)

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE, side_by_side = TRUE)

# 2. save the update matrix as GeoTIFF images
lucC_save_GeoTIFF(raster_obj = raster.data,
                  data_mtx = rb_sits_new,
                  # path_raster_folder = "inst/extdata/raster/New_raster_sampleSecVeg2.tif", as_RasterBrick = TRUE)
                  path_raster_folder = "~/Desktop/New_raster_sampleSecVeg2", as_RasterBrick = TRUE) # or as_RasterBrick = FALSE to create individual images for year



#----------------------------
# 3- Open New data set updated
#----------------------------
#----------------------------
# Header
#----------------------------
library(lucCalculus)
# always
options(digits = 12)

# ------------- define variables to use in sits -------------
# open files with new pixel secondary vegetation
file <- c("~/Desktop/New_raster_sampleSecVeg2/New_New_raster_sampleSecVeg2.tif") # <- new file
file

# create timeline with classified data
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "SampleRegion2", timeline = timeline, bands = "ndvi")
raster.tb

# associate only RasterBrick as new variable
rb_sits3 <- raster.tb$r_objs[[1]][[1]]
rb_sits3

# labels, as defined by the QML file, in same order
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "Secondary_Vegetation"))

# original colors set - see QML file, same order
colors <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red" )

# plot raster brick - NEW
lucC_plot_raster(raster_obj = rb_sits3,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors, plot_ncol = 6)

# only see the differences from OLD and NEW raster updated
# plot raster brick - OLD
lucC_plot_raster(raster_obj = rb_sits2,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors, plot_ncol = 6)


#----------------------------
# 4- Comparison between Pasture in both raster, with and without pixel updated
#----------------------------
# old data set - predicate HOLDS
pasture.df <- lucC_pred_holds(raster_obj = raster.data, raster_class = "Pasture",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains",
                              label = label, timeline = timeline )
head(pasture.df)

# new data set - predicate HOLDS
pasture2.df <- lucC_pred_holds(raster_obj = rb_sits3, raster_class = "Pasture",
                              time_interval = c("2001-09-01","2016-09-01"),
                              relation_interval = "contains",
                              label = label, timeline = timeline )
head(pasture2.df)

# comparison
# old
#lucC_plot_sequence_events(data_mtx = pasture.df, custom_palette = FALSE, show_y_index = FALSE)
#lucC_plot_bar_events(data_mtx = pasture.df, custom_palette = FALSE, pixel_resolution = 232)
lucC_plot_frequency_events(data_mtx = pasture.df, custom_palette = FALSE, pixel_resolution = 232)
# new
#lucC_plot_sequence_events(data_mtx = pasture2.df, custom_palette = TRUE, RGB_color = "blue", show_y_index = FALSE)
#lucC_plot_bar_events(data_mtx = pasture2.df, custom_palette = TRUE, RGB_color = "blue", pixel_resolution = 232)
lucC_plot_frequency_events(data_mtx = pasture2.df, custom_palette = TRUE, RGB_color = "blue", pixel_resolution = 232)

