library(lucCalculus)

# always
options(digits = 12)

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "inst/extdata/raster/rasterItanhanga", path_save_RasterBrick = "inst/extdata/raster")

# ------------- define variables to use in sits -------------
# open files
file <- c("inst/extdata/raster/rasterItanhanga.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Itanhanga", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]
rb_sits


# ------------- define variables to plot raster -------------
# original label - see QML file, same order
#label <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
label <- as.character(c("Cerrado", "Double_cropping", "Single_cropping", "Forest", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water"))
label

# colors
#colors_1 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1 <- c("#BEEE53", "#cd6155", "#e6b0aa", "#228b22", "#7ecfa4", "#afe3c8",  "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1

# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 5)

# change pixel of water by Cerrado, because this class doesn't exist in this municipality
rb_sits <- raster::reclassify(rb_sits, cbind(15, 1))
rb_sits

lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 5)

# select some layers
rb_sits
layers <- c(1, 3, 5, 7, 9, 11, 13, 15)
rb_sits_2years <- raster::subset(rb_sits, layers)
rb_sits_2years

# create timeline with classified data from SVM method
timeline_n <- lubridate::as_date(c("2001-09-01", "2003-09-01", "2005-09-01", "2007-09-01", "2009-09-01", "2011-09-01", "2013-09-01", "2015-09-01"))
timeline_n

png(filename = "~/Desktop/fig_TESE/fig_ita_land_use2D.png", width = 6.7, height = 5.4, units = 'in', res = 300)
lucC_plot_raster(raster_obj = rb_sits_2years,
                 timeline = timeline_n, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 3,
                 relabel = TRUE, original_labels = c("Cerrado", "Double_cropping", "Single_cropping", "Forest", "Pasture"), new_labels =  c("Degradation", "Double cropping", "Single cropping", "Forest", "Pasture") )
dev.off()

# number of cells
myraster <- rb_sits_2years$rasterItanhangaSecVeg.1
length(myraster[raster::values(myraster)!="NA"] )

#----------------------------
# IJGIS
#----------------------------
# 1- Discover Forest Recur its natural vegetation - LUC Calculus
#----------------------------

system.time(
  forest_recur <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                  time_interval1 = c("2001-09-01","2001-09-01"),
                                  time_interval2 = c("2002-09-01","2016-09-01"),
                                  label = label, timeline = timeline)
)

head(forest_recur)

lucC_plot_bar_events(forest_recur, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_re <- lucC_remove_columns(data_mtx = forest_recur, name_columns = c("2001-09-01"))
head(forest_sec)

lucC_plot_bar_events(forest_re, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_re,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")
# Save results
# create images output
# lucC_save_raster_result(raster_obj = rb_sits,
#                         data_mtx = forest_re,       # without 2001
#                         timeline = timeline, label = label, path_raster_folder = "~/Desktop/rasterItanhanga_RECUR", as_RasterBrick = FALSE)


#----------------------------
# 2. Verify if occur forest EVOLVE from a different class in 2001
#----------------------------

forest_evolve <- NULL
# classes without Forest
#classes <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
classes <- as.character(c("Cerrado", "Double_cropping", "Single_cropping", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water"))

# percor all classes
system.time(
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

head(forest_evolve)


lucC_plot_bar_events(forest_evolve, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")
lucC_plot_frequency_events(forest_evolve, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_ev <- lucC_remove_columns(data_mtx = forest_evolve, name_columns = c("2001-09-01"))
head(forest_ev)

lucC_plot_bar_events(forest_ev, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:")

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_evolve, # forest_ev,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".", plot_ncol = 4)
# Save results
# create images output
# lucC_save_raster_result(raster_obj = rb_sits,
#                         data_mtx = forest_ev,       # without 2001
#                         timeline = timeline, label = label, path_raster_folder = "~/Desktop/rasterItanhanga_EVOLVE", as_RasterBrick = FALSE)
#

forest_recur2 <- forest_recur
forest_recur2[,] <- lapply(forest_recur2, function(x) {as.character(x)}) # remove factor
forest_recur2[ forest_recur2 == "Forest" ] <- "Forest_Recurred"
forest_recur2

forest_evolve2 <- as.data.frame(forest_evolve)
forest_evolve2[,] <- lapply(forest_evolve2, function(x) {as.character(x)}) # remove factor
forest_evolve2[ forest_evolve2 == "Forest" ] <- "Forest_Evolved"
forest_evolve2

# 3. Merge both forest_recur and forest_evolve datas
forest_secondary <- lucC_merge(forest_evolve2, forest_recur2)
head(forest_secondary)

lucC_plot_bar_events(forest_secondary, custom_palette = FALSE, pixel_resolution = 231.656, legend_text = "Legend:")

# 4. Remove column 2001 because it' is not used to replace pixels's only support column
forest_sec <- lucC_remove_columns(data_mtx = forest_secondary, name_columns = c("2001-09-01"))
head(forest_sec)

# forest evolved and recur
#png(filename = "~/Desktop/fig_TESE/ita_bar_for_recur_evolve.png", width = 6.5, height = 4.5, units = 'in', res = 300)
png(filename = "~/Desktop/fig_TESE/fig_ita_bar_for_recur_evolve.png", width = 6.8, height = 4.0, units = 'in', res = 300)
lucC_plot_bar_events(forest_sec, custom_palette = TRUE, RGB_color = c("gray60", "black"), pixel_resolution = 231.656, legend_text = "Legend:", side_by_side = TRUE, relabel = TRUE, original_labels = c("Forest_Evolved", "Forest_Recurred"), new_labels = c("Forest evolved", "Forest recurred")) # c("#7ecfa4", "#228b22")
dev.off()


# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = forest_sec,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".")


#------------------------------------------------
#
#------------------------------------------------

# create a RasterBrick from individual raster saved previously
lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/Itanhanga_IJGIS/rasterItanhanga_RECUR", path_save_RasterBrick = "~/Desktop/Itanhanga_IJGIS")

# ------------- define variables to use in sits -------------
# open files
file <- c("~/Desktop/Itanhanga_IJGIS/rasterItanhanga_RECUR.tif")
file

# create timeline with classified data from SVM method # "2002-09-01",
timeline <- lubridate::as_date(c("2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "Itanhanga", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits3 <- raster.tb$r_objs[[1]][[1]]
rb_sits3


# ------------- define variables to plot raster -------------
# original label - see QML file, same order
#label <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture1", "Pasture2", "Pasture3", "Soybean_Cotton", "Soybean_Crop1", "Soybean_Crop2", "Soybean_Crop3", "Soybean_Crop4", "Soybean_Fallow1", "Soybean_Fallow2", "Water", "Water_mask"))
label <- as.character(c("Cerrado", "Double_cropping", "Single_cropping", "Forest", "Pasture", "Pasture", "Pasture", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Double_cropping", "Single_cropping", "Single_cropping", "Water", "Water"))
label

# colors
#colors_1 <- c("#b3cc33", "#d1f0f7", "#8ddbec", "#228b22", "#afe3c8", "#7ecfa4", "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1 <- c("#b3cc33", "#cd6155", "#e6b0aa", "#228b22", "#7ecfa4", "#afe3c8",  "#64b376", "#e1cdb6", "#b6a896", "#b69872", "#b68549", "#9c6f38", "#e5c6a0", "#e5a352", "#0000ff", "#3a3aff")
colors_1

lucC_plot_raster(raster_obj = rb_sits3,
                 timeline = timeline, label = label,
                 custom_palette = FALSE, RGB_color = colors_1, plot_ncol = 3)




#----------------------------
# 3. Verify if forest CONVERT
#----------------------------

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


#----------------------------
# 6- Discover Land use transitions - LUC Calculus
#----------------------------
# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture", "Pasture", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Soy", "Soy", "Water", "Water"))
label2 <- as.character(c("Cerrado", "Crop_Cotton", "Fallow_Cotton", "Forest", "Pasture", "Pasture", "Pasture", "Soy", "Soy", "Soy", "Soy", "Soy", "Soy", "Soy", "Water", "Water", "Secondary_vegetation"))

class1 <- c("Forest")
classes <- c("Pasture", "Soy", "Cerrado", "Crop_Cotton", "Fallow_Cotton" ) #

direct_transi.df <- NULL

# along of all classes
system.time(
  for(x in 2:length(timeline)){
    t_1 <- timeline[x-1]
    t_2 <- timeline[x]
    cat(paste0(t_1, ", ", t_2, sep = ""), "\n")

    # moves across all classes
    for(i in seq_along(classes)){
      cat(classes[i], collapse = " ", "\n")
      temp <- lucC_pred_convert(raster_obj = rb_sits2, raster_class1 = class1,
                                time_interval1 = c(t_1,t_1), relation_interval1 = "equals",
                                raster_class2 = classes[i],
                                time_interval2 = c(t_2,t_2), relation_interval2 = "equals",
                                label = label2, timeline = timeline)

      if (!is.null(temp)) {
        temp <- lucC_remove_columns(data_mtx = temp, name_columns = as.character(t_1))
      } else{
        temp <- temp
      }

      direct_transi.df <- lucC_merge(direct_transi.df, temp)
    }
    cat("\n")
  }
)

#Forest_Pasture <- direct_transi.df
#head(Forest_Pasture)

#Forest_Pasture[ Forest_Pasture == "Pasture" ] <- "Forest_Pasture"
#head(Forest_Pasture)

# plot results
lucC_plot_frequency_events(data_mtx = direct_transi.df,
                           pixel_resolution = 232, custom_palette = FALSE)

lucC_plot_bar_events(direct_transi.df, custom_palette = FALSE, pixel_resolution = 232, legend_text = "Legend:", side_by_side = TRUE)

# 5. Plot secondary vegetation over raster without column 2001 because it' is not used to replace pixels's only support column
lucC_plot_raster_result(raster_obj = rb_sits,
                        data_mtx = direct_transi.df, # forest_ev,
                        timeline = timeline,
                        label = label, custom_palette = TRUE,
                        RGB_color = colors_1, relabel = FALSE, shape_point = ".", plot_ncol = 4)
# Save results
# create images output
lucC_save_raster_result(raster_obj = rb_sits,
                        data_mtx = direct_transi.df,       # without 2001
                        timeline = timeline, label = label, path_raster_folder = "~/Desktop/rasterItanhanga_CONVERT", as_RasterBrick = FALSE)

# Compute values
measures_Forest_Pasture <- lucC_result_measures(data_mtx = direct_transi.df, pixel_resolution = 232)
measures_Forest_Pasture


# Plot measures MT
before_2008 <- read.csv("~/Desktop/Measures_DLUC/measuresSoy_After_2008.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
before_2008

plot(before_2008$Area_km2)


# quanto de floresta virou sooja ou pastagem
#quanto de pastagem virou floresta e
# example de cada predicado

# case study secondary vegetation sinop
# Effect soy moratorium Mt


#-------------------------

library(lucCalculus)

options(digits = 12)

# all files in folder
all.the.files <- list.files("~/Desktop/MT_Chronos/Measures_DLUC", full=TRUE, pattern = ".csv")
all.the.files

files.list <- lapply(all.the.files, data.table::fread, sep=";")
data <- data.table::rbindlist(files.list, fill = TRUE)
data$Hectare <- ifelse(is.na(data$Hectare) == TRUE, (data$Pixel_number*(231.656^2))/(10000), data$Hectare)
data

data.bar <- data[,c("Years","Classes","Hectare")] # data[,1:3]

lucC_plot_bar_events2(data_frequency = data.bar, custom_palette = FALSE, pixel_resolution = 231.656, side_by_side = TRUE, column_legend = 3)
lucC_plot_frequency_events(data_frequency = data.bar, custom_palette = FALSE, pixel_resolution = 231.656, column_legend = 3)

# soy moratorium
unique(data.bar$Classes)
#classes <- c("Forest_Pasture", "Forest_Cerrado", "Soy_After_2008_Pasture", "Soy_Before_2008_Pasture", "Soy_After_2008_Cerrado", "Soy_Before_2008_Cerrado") #, "Pasture_Soy") #
classes <- c("Soy_After_2008_Pasture", "Soy_Before_2008_Pasture", "Soy_After_2008_Cerrado", "Soy_Before_2008_Cerrado", "Mato Grosso (Soja em grao)", "Soy") #, "Pasture_Soy") #
my_data <- data.bar[(data.bar$Classes %in% classes),]
my_data

#lucC_plot_bar_events(data_frequency = my_data, custom_palette = FALSE, pixel_resolution = 231.656, side_by_side = TRUE)
#lucC_plot_bar_events(data_frequency = my_data, custom_palette = TRUE, RGB_color = c("magenta", "#1a9641", "#000080", "#d7191c", "#daa520"), pixel_resolution = 231.656, side_by_side = TRUE, relabel = TRUE, original_labels = c("Forest_Pasture", "Forest_Soy", "Pasture_Soy", "Soy_After_2008", "Soy_Before_2008"), new_labels = c("Forest to Pasture", "Forest to Soy", "Pasture to Soy", "Pasture to Soy, deforested after 2008", "Pasture to Soy, deforested before 2008"))

my <- my_data[!Years %in% c(2000, 2001, 2002, 2003, 2004, 2005)]

#png(filename = "~/Desktop/fig_TESE/fig_MT_bar_soy.png", width = 9.8, height = 6.2, units = 'in', res = 300)
lucC_plot_bar_events(data_frequency = my, custom_palette = FALSE, RGB_color = c("#1a9641", "#9cecc4", "#c7011a", "#000d92", "#f6546a", "#4169e1"), pixel_resolution = 231.656, side_by_side = TRUE, relabel = FALSE, original_labels = c("Forest_Pasture", "Forest_Cerrado", "Soy_After_2008_Cerrado", "Soy_After_2008_Pasture", "Soy_Before_2008_Cerrado", "Soy_Before_2008_Pasture" ), new_labels = c("Forest to Pasture", "Forest to Degradation", "Degradation to Soy, deforested after 2008", "Pasture to Soy, deforested after 2008", "Degradation to Soy, deforested before 2008", "Pasture to Soy, deforested before 2008"), legend_text = "Land use transitions: ", column_legend = 3)
#dev.off()
#"#1a9641", "#000080", "#d7191c", "#daa520", "magenta", "black"

lucC_plot_frequency_events(data_frequency = my, custom_palette = FALSE, pixel_resolution = 231.656)

# land use transitions
unique(data.bar$Classes)
classes <- c("Cerrado_Pasture", "Cerrado_Secondary_Vegetation", "Cerrado_Soy", "Forest_Cerrado", "Forest_Pasture", "Forest_Soy", "Pasture_Cerrado", "Pasture_Secondary_Vegetation", "Pasture_Soy", "Secondary_Vegetation_Cerrado", "Secondary_Vegetation_Pasture", "Secondary_Vegetation_Soy", "Soy_Cerrado", "Soy_Pasture", "Soy_Secondary_Vegetation") #
my_data2 <- data.bar[(data.bar$Classes %in% classes),]
my_data2

lucC_plot_bar_events(data_frequency = my_data2, custom_palette = FALSE, pixel_resolution = 231.656, side_by_side = TRUE, column_legend = 5)
lucC_plot_frequency_events(data_frequency = my_data2, custom_palette = FALSE, pixel_resolution = 231.656)


# forest x secondary vegetation
unique(data.bar$Classes)
classes <- c("Forest", "Secondary_Vegetation") #
my_data2 <- data.bar[(data.bar$Classes %in% classes),]
my_data2

png(filename = "~/Desktop/fig_TESE/fig_MT_for_sv.png", width = 7.0, height = 4.2, units = 'in', res = 300)
lucC_plot_bar_events(data_frequency = my_data2, custom_palette = TRUE, RGB_color = c("#228b22", "#7ecfa4"), pixel_resolution = 231.656, legend_text = "Legend:", side_by_side = TRUE, relabel = TRUE, original_labels = c("Forest", "Secondary_Vegetation"), new_labels = c("Forest", "Secondary Vegetation")) # c("#7ecfa4", "#228b22") c("gray60", "black")
dev.off()

lucC_plot_frequency_events(data_frequency = my_data2, custom_palette = FALSE, pixel_resolution = 231.656)



#---------------------
library(ggplot2)

dfTab <- as.data.frame((my_data))
colnames(dfTab)[1] <- "x"
dfTab$lab <- as.character(100 * dfTab$Freq / sum(dfTab$Freq))

ggplot(df) + geom_bar(aes(x,fill=x)) +
  geom_text(data=dfTab,aes(x=x,y=Freq,label=lab),vjust=0) +
  theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
       axis.title.x=element_blank(),legend.title=element_blank(),
       axis.title.y=element_blank())

dfTab$lab <- paste(dfTab$Freq,paste("(",dfTab$lab,"%)",sep=""),sep=" ")




