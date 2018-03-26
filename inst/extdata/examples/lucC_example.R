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

#-------------------
# plot some results from EVOLVE
lucC_plot_sequence_events(forest_evolve, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(forest_evolve, custom_palette = FALSE, legend_text = "Legend:")

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = forest_evolve,
                        timeline = timeline, label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE) #, shape_point = "#")
#-------------------

# 3. Merge both forest_recur and forest_evolve datas
forest_secondary <- lucC_merge(forest_evolve, forest_recur)
head(forest_secondary)

# plot
lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
head(forest_sec)

# plot
lucC_plot_bar_events(forest_sec, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_evolve, #forest_sec,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE) #, shape_point = ".")


# create images output
rb_sits_new <- lucC_raster_result(raster_obj = rb_sits,
                                  data_mtx = forest_evolve,       # without 2001
                                  timeline = timeline, label = label)         # new pixel value
rb_sits_new


lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_new, path_raster_folder = "~/Desktop/Neee", as_RasterBrick = FALSE)

#----------------------------
# 4 - Update original raster to add new pixel value
#----------------------------

# 1. update original RasterBrick with new class
rb_sits_new <- lucC_raster_update(raster_obj = rb_sits,
                                  data_mtx = forest_sec,       # without 2001
                                  timeline = timeline,
                                  class_to_replace = "Forest",  # only class Forest
                                  new_pixel_value = 13)         # new pixel value

head(rb_sits_new)

lucC_plot_bar_events(data_mtx = rb_sits_new, pixel_resolution = 232, custom_palette = FALSE)

# 2. save the update matrix as GeoTIFF RasterBrick
lucC_save_GeoTIFF(raster_obj = rb_sits,
                  data_mtx = rb_sits_new,
                  path_raster_folder = "inst/extdata/raster/raster_sampleSecVeg1", as_RasterBrick = TRUE ) # FALSE before

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
















#-------------------------
#-------------------------

#------------- tests - convert
sixth_raster.df <- lucC_pred_convert(raster_obj = rb_sits, raster_class1 = "Forest",
                                     time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "equals",
                                     raster_class2 = "Cerrado",
                                     time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                                     label = label, timeline = timeline)
sixth_raster.df

lucC_plot_sequence_events(sixth_raster.df, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(sixth_raster.df, custom_palette = FALSE)


#------------- tests - evolve
fifth_raster.df <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = "Pasture",
                                    time_interval1 = c("2001-09-01","2001-09-01"), relation_interval1 = "contains",
                                    raster_class2 = "Forest",
                                    time_interval2 = c("2002-09-01","2016-09-01"), relation_interval2 = "contains",
                                    label = label, timeline = timeline)
fifth_raster.df

lucC_plot_sequence_events(fifth_raster.df, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(fifth_raster.df, custom_palette = FALSE)
lucC_plot_frequency_events(fifth_raster.df, custom_palette = FALSE, legend_text = "Legend")


#---------------
a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
                     time_interval = c("2001-09-01","2003-09-01"),
                     relation_interval = "equals", label = label, timeline = timeline)
a

b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
                     time_interval = c("2004-09-01","2007-09-01"),
                     relation_interval = "contains", label = label, timeline = timeline)
b

# before
c1 <- lucC_relation_before(first_raster = a, second_raster = b)
c1
lucC_plot_sequence_events(c1, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(c1, custom_palette = FALSE)

# meets
c2 <- lucC_relation_meets(first_raster = a, second_raster = b)
c2
lucC_plot_sequence_events(c2, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(c2, custom_palette = FALSE)

# follows
c3 <- lucC_relation_follows(first_raster = a, second_raster = b)
c3
lucC_plot_sequence_events(c3, custom_palette = FALSE, show_y_index = TRUE)
lucC_plot_bar_events(c3, custom_palette = FALSE, side_by_side = FALSE, legend_text = "New", column_legend = 2)
lucC_plot_frequency_events(c3, custom_palette = FALSE, legend_text = "New", column_legend = 1)

# in
c4 <- lucC_relation_in(first_raster = a, second_raster = b)
c4
lucC_plot_sequence_events(c4, custom_palette = FALSE, show_y_index = TRUE)

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = c3, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE) #, shape_point = "#")

.
.
# create label with classified data from SVM method
label_2 <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water", "zecondary_vegetation"))
label_2

colors_2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1", "red")


lucC_plot_raster(raster_obj = r, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE) #, shape_point = "#")

lucC_plot_raster(raster_obj = r, timeline = timeline, label = label_2, custom_palette = TRUE, RGB_color = colors_2, relabel = FALSE) #, shape_point = "#")



rb_new <- lucC_update_raster(raster_obj = rb_sits, data_mtx = third_raster.df, timeline = timeline, class_to_replace = "Forest", new_pixel_value = 6)
rb_new

lucC_plot_bar_events(data_mtx = rb_new, pixel_resolution = 232, custom_palette = FALSE)

lucC_save_GeoTIFF(raster_obj = rb_sits, data_mtx = rb_new, path_raster_folder = "~/Desktop/raster222")


lucC_plot_raster(raster_obj = rb_sits, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE)


#------------------------
# Test if change original label by other
#------------------------
# original
lucC_plot_raster(raster_obj = rb_sits, timeline = timeline, label = label, custom_palette = TRUE, RGB_color = colors_1, relabel = FALSE, plot_ncol = 6) #,

colors_2 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1")

label2 <- as.character(c("Pasture", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water"))
label2

#
a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Pasture",
                     time_interval = c("2001-09-01","2016-09-01"),
                     relation_interval = "contains", label = label2, timeline = timeline)
a

#
lucC_plot_raster(raster_obj = rb_sits, timeline = timeline, label = label2, custom_palette = TRUE, RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)

lucC_plot_raster_result(raster_obj = rb_sits, data_mtx = a, timeline = timeline, label = label2, custom_palette = TRUE, RGB_color = colors_2, relabel = FALSE, plot_ncol = 6)




#----------------------------------
library(dplyr)
# You can also unnest multiple columns simultaneously
df <- tibble(
  a = list(c("a", "b", "x"), "c"),
  b = list(c(1:2,4), 3),
  c = c(11, 22)
)

df$a

df %>% tidyr::unnest(a, b)
df

# If you omit the column names, it'll unnest all list-cols
df %>% tidyr::unnest()

# You can also choose to preserve one or more list-cols
df %>% tidyr::unnest(a, .preserve = b)



