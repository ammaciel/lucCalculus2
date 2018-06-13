library(lucCalculus)

options(digits = 12) # always put this element

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata with SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
#lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/MT_Chronos/Alto_Teles_Pires_2005_2016", path_save_RasterBrick = "~/Desktop/MT_Chronos/Alto_Teles_Pires_2005_2016")

# ------------- define variables to use in sits -------------
# open files
file <- c("~/Desktop/MT_Chronos/Alto_Teles_Pires_2005_2016/Alto_Teles_Pires_2005_2016.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "AltoTP", timeline = timeline, bands = "ndvi")
raster.tb

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]
rb_sits


# ------------- define variables to plot raster -------------
# original label - see QML file, same order
#label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_area", "Water", "Secondary_vegetation"))

label <- as.character(c("Cerrado", "Single_cropping", "Forest", "Pasture", "Double_cropping", "Double_cropping", "Single_cropping", "Double_cropping", "Double_cropping", "Sugarcane", "Urban_area", "Water", "Secondary_vegetation"))

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
lucC_save_raster_result(raster_obj = rb_sits, data_mtx = holds.mtx, timeline = timeline, label = label, path_raster_folder = paste0("~/Desktop/MT_Chronos/Alto_Teles_Pires_2005_2016/Holds_SC_DC/", file_name, sep = ""), as_RasterBrick = FALSE)

message("Prepare image 2 ...\n")
lucC_save_raster_result(raster_obj = rb_sits, data_mtx = holds.mtx, timeline = timeline, label = label, path_raster_folder = paste0("~/Desktop/MT_Chronos/Alto_Teles_Pires_2005_2016/Holds_SC_DC/", file_name, sep = ""), as_RasterBrick = TRUE)


.
#-------------------------------
# start here after
#-------------------------------
library(lucCalculus)

options(digits = 12) # always put this element

# open files
file <- c("~/Desktop/MT_Chronos/Alto_Teles_Pires_2005_2016/Holds_SC_DC/Alto_Teles_Pires_2005_2016/New_Alto_Teles_Pires_2005_2016.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

# library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(files = file, name = "AltoTP", timeline = timeline, bands = "ndvi")
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
colors_1 <- c("cyan","red","cyan","cyan","gray")


# raster::plot(rb_sits)
# plot raster brick
lucC_plot_raster(raster_obj = rb_sits,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_1, plot_ncol = 4)

# raster::plot(rb_sits) # rb_sits$New_Sinop_2005_2016.1


# onlye single and double cropping trajectories
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

# create raster
data <- dplyr::select(trajec.df, x, y, class)
dfr <- raster::rasterFromXYZ(data)  #Convert first two columns as lon-lat and third as value
#raster::plot(dfr)
dfr


# http://www.color-hex.com/color-palette/60816 - cores
# ------------- define variables to plot raster -------------
# original label - see QML file, same order
label <- as.character(c("Stable", "Intensification", "Reduction", "Interchange", "Expansion", "Anomaly"))

# colors
colors_2 <- c("#1a80b6", "#e1d803","#e10000", "#00ba0c", "#ffaeae", "#373d41" ) #"#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6"
timeline <- "2016-01-01"

# plot raster brick
lucC_plot_raster(raster_obj = dfr,
                 timeline = timeline, label = label,
                 custom_palette = TRUE, RGB_color = colors_2, plot_ncol = 4, legend_text = "")

#lucC_save_GeoTIFF(raster_obj = rb_sits, data_mtx = data, path_raster_folder = "~/Desktop/MT_Chronos/Alto_Teles_Pires_2005_2016/data.tif", as_RasterBrick = FALSE)


data1 <- data
colnames(data1) <- c("x", "y", "2016-09-01")

lucC_plot_bar_events2(data1, custom_palette = TRUE,
                     RGB_color = colors_2, pixel_resolution = 231.656,
                     side_by_side = TRUE, relabel = TRUE,
                     original_labels = c("1", "2", "3", "4", "5", "6"),
                     new_labels = c("Stable", "Intensification", "Reduction", "Interchange", "Expansion", "Anomaly"))


data2 <- data.frame(table(data1$`2016-09-01`)) %>%
  dplyr::mutate(year <- "2016") %>%
  dplyr::mutate(hectare <- (.$Freq*(231.656*231.656))/10000)
data2

colnames(data2) <- c("Classes", "Num_pixel", "Year", "Hectare")
data2 <- dplyr::select(data2, "Year", "Classes", "Hectare")
levels(data2$Classes) <- c("Stable", "Intensification", "Reduction", "Interchange", "Expansion", "Anomaly")
data2

data3 <- data2 %>%
  dplyr::group_by(Classes) %>%
  dplyr::summarise( n = Hectare) %>%    # Num_pixel
  dplyr::mutate(FreqRelative = n/sum(n)) %>%
  dplyr::mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%"))
data3

colnames(data3) <- c("Classes", "Num_pixel", "FreqRelative", "Percentage")
data3

#png(filename = "~/Desktop/fig_TESE/fig_MT_data.png", width = 7.0, height = 4.2, units = 'in', res = 300)
lucC_plot_bar_events2(data_frequency = data2, custom_palette = TRUE,
                      RGB_color = colors_2, pixel_resolution = 231.656,
                      side_by_side = TRUE, column_legend = 3, relabel = FALSE)
#dev.off()





#------------------------------------
library(ggplot2)

lucC_plot_bar_events2 <- function(data_mtx = NULL, data_frequency = NULL, custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250, relabel = FALSE, original_labels = NULL, new_labels = NULL, legend_text = "Legend:", column_legend = 2, side_by_side = FALSE){

  # Ensure if parameters exists
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(pixel_resolution, !is.null(pixel_resolution),
                       err_desc = "pixel_resolution must be defined! Default is 250 meters on basis of MODIS image")

  # input data matrix or a frequency table
  if (!is.null(data_mtx)){
    # to data frame
    input_data <- as.data.frame(data_mtx) %>%
      tidyr::gather(variable, value, -x, -y) %>%
      stats::na.omit()

    input_data <- input_data[!duplicated(input_data), ]
    # count number of values
    mapBar <- data.frame(table(lubridate::year(input_data$variable), input_data$value))
  } else if (!is.null(data_frequency)){
    # already
    mapBar <- data_frequency
    colnames(mapBar) <- c("Var1", "Var2", "Freq")
  } else {
    stop("\nProvide at least a 'data_mtx' or a 'data_frequency' to plot graphics!\n")
  }

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(mapBar$Var2))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      my_palette = RGB_color
    }
  } else {
    # more colors
    colour_count = length(unique(mapBar$Var2))
    my_palette = scales::hue_pal()(colour_count)
  }

  original_leg_lab <- unique(as.character(mapBar$Var2)) # levels(droplevels(mapBar$Var2))
  cat("Original legend labels: \n", original_leg_lab, "\n")

  # insert own legend text
  if(relabel == TRUE){
    if(is.null(original_labels) | length(new_labels) != length(unique(mapBar$Var2)) |
       all(original_labels %in% original_leg_lab) == FALSE){
      cat("\nIf relabel = TRUE, a vector with original labels must be defined!")
      cat("\nProvide a list of original labels and new labels with the same length of the legend!\n")
    } else {
      my_original_label = original_labels
      my_new_labels = new_labels
    }
  } else {
    # my legend text
    my_original_label = unique(mapBar$Var2)
    my_new_labels = unique(mapBar$Var2)
  }

  # make side-by-side bar plot
  if (side_by_side == TRUE){
    bar_position = "dodge"
  } else {
    bar_position = "stack"
  }

  # complete space in bars to have the same width of bars in geom_bar
  mapBar <- tidyr::complete(mapBar, Var1, Var2)
  mapBar$Var1 <- as.factor(mapBar$Var1)
  mapBar$Var2 <- as.factor(mapBar$Var2)

  g <- ggplot2::ggplot(mapBar,aes(x=mapBar$Var2, y=mapBar$Freq, fill=mapBar$Var2)) +
    geom_bar(width = 0.7, stat="identity", position = bar_position) +
    theme_bw()+
    #ylab(expression(paste("Area ",km^{2}," = ((pixels number x pixel ", resolution^{2},")/",1000^{2},")"))) +
    ylab("Hectares")+
    xlab("Temporal patterns of agricultural trajectories")+
    scale_fill_manual(name= legend_text, values = my_palette, breaks = my_original_label, labels = my_new_labels) + #Legend

    geom_text(aes(y=mapBar$Freq,
                  label=format(mapBar$Freq, digits=2)),
              position = position_dodge(width = 1),
              vjust = -0.5, size = 3,
              color=rgb(100,100,100, maxColorValue=255)) +

    theme(legend.position = "none", #"bottom",
          legend.text=element_text(size=11), ### ### era 11
          axis.text.x=element_text(angle=45, hjust=1, size = 11),
          axis.text.y=element_text(size = 11),
          legend.key = element_blank(),
          plot.background = element_blank(),
          ##panel.grid.major = element_blank(),
          ##panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#b2b2b2", linetype="dashed", size = 0.2),
          panel.grid.major.x = element_blank()) +

    #draws x and y axis line
    theme(axis.line = element_line(color = 'black'))

  print(g)

}
