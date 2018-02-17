####
# Example: read a json file using lucC_fromJSON and 
# adequate data to extrat a lot of events
library(sits)
library(lucC)

lucC_starting_point()

#*********************************
# Classification time series using a set of temporal patterns
#*********************************

# ita.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_lucC.tb.RData"))
# ita.tb
# ita.tb2 <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_all.tb.RData"))
# ita.tb2

itanhanga.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_all.tb.RData"))
itanhanga.tb

# read a pattern table from a JSON file
patterns.tb <- sits_getdata("~/Dropbox/TWDTWAmazoniaCerrado/Assinaturas/JSON/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")
data.frame(unique(patterns.tb$label))

# cla <- c("Soybean_Pasture", "Water")
# patterns.tb <- dplyr::filter(patterns.tb, !patterns.tb$label %in% cla)

# only this bands have in patterns
bands <- c("ndvi", "evi", "nir")

# plot patterns
sits_plot(patterns.tb, type = "patterns")

# json have other attributes
values <- c("Index", "ndvi", "evi", "nir") # 

# # remove other attributes
# santa_carmem.tb$time_series <- lapply(santa_carmem.tb$time_series,function(p){
#   p <- p[,values, drop = FALSE]
# })

# classify with TWDTW and return a format to lucC
#res <- lucC_applyTWDTW(santa_carmem.tb[1:3,], patterns.tb, bands)
itanhanga_TWDTW_sits.tb <- sits_TWDTW(itanhanga.tb, patterns.tb, bands)
itanhanga_TWDTW_sits.tb

15:36

# use lucC_toJSON despite of decimal digits
save(itanhanga_TWDTW.tb, file = "~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_lucC.tb.RData")

# see maps
lucC_plot_maps_input(itanhanga_TWDTW.tb, EPSG_WGS84 = TRUE, custom_palette = FALSE) 



#*********************************
# Rename for new labels before make questions
#*********************************
library(lucC)

#santa_carmem_TWDTW.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/SantaCarmem/SantaCarmem_part_TWDTW_lucC.tb.RData"))
itanhanga_TWDTW.tb <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_lucC.tb.RData"))
itanhanga_TWDTW.tb

# #"longitude", "latitude", "start_date", "end_date", "label", "id", "index"
# itanhanga_TWDTW.tb <- itanhanga_TWDTW.tb[,1:7]
# 
# # remove data with 2017 in end_data
# itanhanga_TWDTW.tb <- dplyr::filter(itanhanga_TWDTW.tb, 
#                                     !grepl("2017", as.character(itanhanga_TWDTW.tb$end_date), 
#                                            fixed = TRUE))
# 
# save(itanhanga_TWDTW.tb, file = "~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_lucC.tb.RData")

# see maps
png(filename = "~/Desktop/fig0_itanhanga.png", width = 2573, height = 2098, res = 300)
lucC_plot_maps_input(itanhanga_TWDTW.tb, EPSG_WGS84 = TRUE, custom_palette = FALSE) 
dev.off()

# alter start_date and end_date to a especific range in order to extract events
itanhanga_TWDTW_new_date.tb <- lucC_standard_date_events(data_tb = itanhanga_TWDTW.tb, month_year = "09", day_month = "01")
itanhanga_TWDTW_new_date.tb

# Forest --> Pasture --> Cropping
df <- itanhanga_TWDTW_new_date.tb

# amount of data for class
data.frame(table(df$label))

df$"label2" <- df$label

# create progress bar
progress_bar <- txtProgressBar(min = 0, max = nrow(df), style = 3)

# change label for 
for (x in 1:nrow(df)){
  
  if (df$label2[x] == "Savanna") {
    df$"label"[x] = "Cerrado"
  } else if (df$label2[x] %in% c("Pasture1","Pasture2")) {
    df$"label"[x] = "Pasture"
  } else if (df$label2[x] %in% c("Soybean_Fallow1", "Soybean_Fallow2", 
                                 "Fallow_Cotton")) {
    df$"label"[x] = "Single_cropping"
  } else if (df$label2[x] %in% c("Soybean_NonComerc1", "Soybean_NonComerc2", 
                                 "Soybean_Comerc1", "Soybean_Comerc2", 
                                 "NonComerc_Cotton", "Soybean_Cotton")) {
    df$"label"[x] = "Double_cropping"
  }
  # update progress bar
  setTxtProgressBar(progress_bar, x)
}
close(progress_bar)

df

# amount of data for class
data.frame(table(df$label))

# use lucC_toJSON despite of decimal digits
#save(df, file = "~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_new_label.tb.RData")

# amount of data for class
data.frame(table(df$label))

# see maps
lucC_plot_maps_input(df, EPSG_WGS84 = TRUE, custom_palette = FALSE) 
lucC_plot_maps_input(itanhanga_TWDTW_new_date.tb, EPSG_WGS84 = TRUE, custom_palette = FALSE) 

lucC_plot_maps_input(df, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))#,"#00bfff")) # , "#66CC00" secondary_vegetation



#*********************************
# Secondary_vegetation
#*********************************

# AREA "Santa Carmem" - IJGIS - pattern_classification_14patterns_3bands_Area1_Sinop
## Question - Which "Forest" areas have been turned in secondary-vegetation after anyclass? 
# o = geo-objects, the own df_input data.frame

# p = properties of objects 
p1 <- "Forest" 

p2 <- "Cerrado"
p3 <- "Pasture"
p4 <- "Single_cropping"
p5 <- "Double_cropping"

# te = interval:
t1 <- lucC_interval("2000-09-01","2001-09-01")
t2 <- lucC_interval("2001-09-01","2016-09-01")


# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  # create progress bar
  progress_bar <- txtProgressBar(min = 0, max = length(coord), style = 3)###
  
  for(x in 1:length(coord)){
    # x=1
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if ((nrow(ev1.in1 <- lucC_predicate_holds(temp, p1, t1)) >= 1 | #forest in first interval
         
         nrow(ev2.in1 <- lucC_predicate_holds(temp, p2, t1)) >= 1 | # other classes in first interval
         nrow(ev3.in1 <- lucC_predicate_holds(temp, p3, t1)) >= 1 |
         nrow(ev4.in1 <- lucC_predicate_holds(temp, p4, t1)) >= 1 |
         nrow(ev5.in1 <- lucC_predicate_holds(temp, p5, t1)) >= 1 
         
    ) 
    &
    (	  nrow(ev2.in2 <- lucC_predicate_holds(temp, p2, t2)) >= 1 | # other classes in second interval
        nrow(ev3.in2 <- lucC_predicate_holds(temp, p3, t2)) >= 1 |
        nrow(ev4.in2 <- lucC_predicate_holds(temp, p4, t2)) >= 1 |
        nrow(ev5.in2 <- lucC_predicate_holds(temp, p5, t2)) >= 1 
    ) 
    &
    nrow(ev1.in2 <- lucC_predicate_holds(temp, p1, t2)) >= 1 # if occur forest in second interval 
    & 
    # verify if forest discovered in interval 2 occur after start_date other classes. Or f is different from first year 
    # If TRUE, this forest is a secondary_vegetation  
    nrow(event3 <- ev1.in2[which(ev1.in2$start_date > min(
      ev2.in2$start_date, ev3.in2$start_date,
      ev4.in2$start_date, ev5.in2$start_date, na.rm=TRUE) | 
      (ev1.in2$label != head(temp$label,1))),]) >=1 
    
    ){
      # temp0 <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1,
      #                ev2.in2, ev3.in2, ev4.in2, ev5.in2,
      #                event3)
      temp0 <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1, event3)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
    
    # update progress bar
    setTxtProgressBar(progress_bar, x)###
    
  }
  
  close(progress_bar)###
  
  return(output_df)
}

# verify using function
data_sec_veg_occurs <- QuestionOccurs(df_label_2, p = c(p1,p2,p3,p4,p5), t = c(t1,t2))
rm(list = ls(pattern="^p[0-9]")) # remove classes
remove(t1,t2) # remove intervals

# remove duplicated rows
length(which(duplicated(data_sec_veg_occurs)))
#data_sec_veg_occurs <- data_sec_veg_occurs[!duplicated(data_sec_veg_occurs),]

#png(filename = "~/Desktop/fig1_itanhanga_TWDTW.png", width = 2573, height = 2098, res = 300)
#png(filename = "~/Desktop/fig1_area.png", width = 7, height = 7, units = 'in', res = 300)
lucC_plot_maps_input(df_label_2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#1b791f", "#929e6e", "#f5e7a1", "#FFB266")) # , "#66CC00" secondary_vegetation
#dev.off()

#lucC_toGeoTIFF(df, path_raster_folder = "~/Desktop/raster/")

#png(filename = "~/Desktop/fig2_area_eve.png", width = 7, height = 7, units = 'in', res = 300)
lucC_plot_maps_events(data_sec_veg_occurs, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#1b791f", "#929e6e", "#f5e7a1", "#FFB266"), size_square = 1.5)# 0.000001) 

lucC_plot_maps_input(df_new, EPSG_WGS84 = TRUE) 
lucC_plot_barplot_events(df_new[which(df_new$label == "Forest"),]) 


#*********************************
# Rules to change Forest to Secondary_vegetation
#*********************************

df_input <- df

df_temp <- data_sec_veg_occurs

# remove first line of data.frame that contain the from == "2000-08-15" and change by other label
df_first_line <- df_temp[which(df_temp$start_date == "2000-09-01"),]
df_posProc <- df_temp[which(df_temp$start_date != "2000-09-01"),]

# replace Forest to Secondary-vegetation
df_posProc$label <- as.character(df_posProc$label)
df_posProc$label[df_posProc$label == "Forest"] <- "Secondary_vegetation"

# merge first line dataframe with pos-processing dataframe
df_pos <- dplyr::bind_rows(df_first_line,df_posProc)

# original data
df_temp2 <- df_input 

# temp3 dataframe with only id lines not in pos dataframe
df_temp3 <- df_temp2[!(df_temp2$id %in% df_pos$id),]

# merge df_input without id with renamed with lines without changes
df_new <- dplyr::bind_rows(df_pos,df_temp3)
df_new <- data.frame(df_new[order(df_new$id),]) # order
#rownames(df_new) <- 1:nrow(df_new)

# return as tibble 
df_new <- tibble::as_tibble(df_new)

head(df_new)
# remove other
remove(df_temp,df_first_line,df_posProc,df_pos,df_temp2,df_temp3)

#save(df_new, file = "~/Desktop/EST itanhanga_TWDTW_postprocess.RData")

# plot with new type of vegetation
lucC_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) # secondary_vegetation

lucC_plot_barplot_events(df_new[which(df_new$label == "Forest"),]) 

# to know the amount of data per year and classe type
data.frame(table(df_new$end_date, df_new$label))

# area selected
lucC_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#1b791f", "#929e6e", "#66CC00", "#f5e7a1", "#FFB266")) #  secondary_vegetation


#******************************************************************
# Question to journal
#******************************************************************

# open dataset with 5 labels
df_label <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_new_label.tb.RData"))
df_label

# amount of data for class
data.frame(table(df_label$label))

#png(filename = "~/Desktop/fig1_itanhanga.png", width = 2573, height = 2098, res = 300)
lucC_plot_maps_input(df_label, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e",  "#f5e7a1")) 
#dev.off()

# open dataset with 6 labels - secondary vegetation
df_new <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_postprocess.RData"))
df_new
# amount of data for class
data.frame(table(df_new$label))

#png(filename = "~/Desktop/fig2_secondary.png", width = 2573, height = 2098, res = 300)
lucC_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) 
#dev.off()



#---------------------------------
# Question 1 - Which "Forest" areas haven't been replaced by other croppings?
# o = geo-objects, the own df_input data.frame
#---------------------------------

# p = properties of objects :
p1 <- "Forest"

# t = interval:
t1 <- lucC_interval("2000-08-01","2017-03-01")

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if (nrow(event2 <- lucC_predicate_holds(temp, p1, t1)) >= 1
        
    ){
      temp0 <- rbind(event2)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
  }
  return(output_df)
}

output.tb1 <- QuestionOccurs(df_new, p = p1, t = t1)
remove(t1)
remove(p1)

# remove duplicated rows
length(which(duplicated(output.tb1)))
# output.tb1 <- output.tb1[!duplicated(output.tb1),]

#----------
#save(output.tb1, file = "~/Desktop/fig3_only_forest_ita.RData")
output.tb1 <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/fig3_only_forest_ita.RData"))

lucC_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) # secondary_vegetation

#png(filename = "~/Desktop/fig3_forest.png", width = 2573, height = 2098, res = 300)
lucC_plot_maps_events(output.tb1, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00"), shape_point = ".", colour_point = "black", size_point = 0.0000000000001) 
#dev.off()

lucC_plot_maps_events(output.tb1, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00"), shape_point = 21, colour_point = "blue", size_point = 1) 

png(filename = "~/Desktop/fig3_barplot_forest1.png", width = 7, height = 5, units = 'in', res = 300)
lucC_plot_barplot_events(output.tb1, custom_palette = TRUE, RGB_color = "#1b791f", pixel_resolution = 231.6564) 
dev.off()

lucC_plot_sequence_events(output.tb1, show_y_index = FALSE, end_date = "2017-03-01") 

#lucC_plot_barplot_events(df_new[which(output.tb1$label == "Forest"),]) 


#---------------------------------
# Question 2 - Which "Forest" areas have been replaced by Pasture after 2001?
# o = geo-objects, the own df_input data.frame
#---------------------------------

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"

# t = interval:
t1 <- lucC_interval("2000-09-01","2001-09-01")
t2 <- lucC_interval("2001-09-01","2017-09-01")

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if (nrow(ev1 <- lucC_predicate_holds(temp, p1, t1)) >= 1 &
        nrow(ev2 <- lucC_predicate_holds(temp, p2, t2)) >= 1 &
        
        isTRUE(lucC_relation_following(tail(lucC_interval(ev1$start_date, ev1$end_date)),
                                        head(lucC_interval(ev2$start_date, ev2$end_date))))
        
    ){
      temp0 <- rbind(ev1,ev2)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
  }
  return(output_df)
}

output_df2 <- QuestionOccurs(df_new, p = c(p1, p2), t = c(t1,t2))
remove(t1, t2)
remove(p1, p2)


# remove duplicated rows
length(which(duplicated(output_df2)))
#output_df2 <- output_df2[!duplicated(output_df2),]

lucC_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#66CC00", "#f5e7a1")) # secondary_vegetation

lucC_plot_maps_events(output_df2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) 

lucC_plot_barplot_events(output_df2, custom_palette = FALSE) 

lucC_plot_sequence_events(output_df2, show_y_index = FALSE, end_date = "2017-03-01") 
#lucC_plot_barplot_events(output_df2[which(output_df2$label == "Forest"),]) 

#---------------------------------
# Question 3 - Which "Forest" areas have been replaced by Pasture, Double cropping or single cropping after 2001?
# o = geo-objects, the own df_input data.frame
#---------------------------------

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"
p3 <- "Single_cropping"
p4 <- "Double_cropping"

# t = interval:
t1 <- lucC_interval("2000-09-01","2001-09-01")
t2 <- lucC_interval("2001-09-01","2017-09-01")

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if (nrow(ev1 <- lucC_predicate_holds(temp, p1, t1)) >= 1 &
        nrow(ev2 <- lucC_predicate_holds(temp, p2, t2)) >= 1 &
        nrow(ev3 <- lucC_predicate_holds(temp, p3, t2)) >= 1 &
        nrow(ev4 <- lucC_predicate_holds(temp, p4, t2)) >= 1 &
        
        isTRUE(lucC_relation_following(tail(lucC_interval(ev1$start_date, ev1$end_date)),
                                        head(lucC_interval(ev2$start_date, ev2$end_date)))) |
        isTRUE(lucC_relation_following(tail(lucC_interval(ev1$start_date, ev1$end_date)),
                                        head(lucC_interval(ev3$start_date, ev3$end_date)))) |
        isTRUE(lucC_relation_following(tail(lucC_interval(ev1$start_date, ev1$end_date)),
                                        head(lucC_interval(ev4$start_date, ev4$end_date))))
        
    ){
      temp0 <- rbind(ev1,ev2, ev3, ev4)
    } else {
      temp0 <- NULL
    }
    output_df <- dplyr::bind_rows(output_df,temp0)
  }
  return(output_df)
}

output_df2 <- QuestionOccurs(df_new, p = c(p1, p2, p3, p4), t = c(t1,t2))
remove(t1, t2)
remove(p1, p2, p3, p4)


# remove duplicated rows
length(which(duplicated(output_df2)))
#output_df2 <- output_df2[!duplicated(output_df2),]

png(filename = "~/Desktop/fig2_secondary_vegetation.png", width = 8, height = 8, units = 'in', res = 300)
lucC_plot_maps_input(df_new, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) # secondary_vegetation
dev.off()

png(filename = "~/Desktop/fig4_forest_to_others.png", width = 8, height = 8, units = 'in', res = 300)
lucC_plot_maps_events(output_df2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#f5e7a1", "#66CC00")) 
dev.off()

png(filename = "~/Desktop/fig4_barplot_forest_others.png", width = 7, height = 5, units = 'in', res = 300)
lucC_plot_barplot_events(output_df2, custom_palette = TRUE, RGB_color = c( "#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 
dev.off()

lucC_plot_sequence_events(output_df2, show_y_index = FALSE, end_date = "2017-03-01") 

#lucC_plot_barplot_events(output_df2[which(output_df2$label == "Forest"),]) 





#---------------------------------
# Question 4 - Which "Forest" areas have been replaced by Pasture after in some time interval?
# o = geo-objects, the own df_input data.frame
#---------------------------------
# Forest --> Pasture --> Cropping - Type of transition

#transition_string <- c("Pasture","Cropping","Pasture","Cropping","Pasture","Cropping","Pasture","Cropping","Pasture","Cropping")
#transition_string <- c("Forest", "Cerrado", "Secondary_vegetation", "Cropping", "Pasture")
transition_string <- c("Pasture", "Single_cropping", "Double_cropping")

#df <- df_new
df <- df_new_2
# create a tibble with the same column names 
output.tb3 <- df[FALSE,] # Always run this 
coord <- unique(df$index)

# create progress bar
progress_bar <- txtProgressBar(min = 0, max = length(coord), style = 3)###

# Apply over all input data
for(x in 1:length(coord)){
  
  temp.tb <- df[which(as.character(df$index) == coord[x]),]
  temp_final.tb <- lucC_event_transitions(temp.tb, properties = transition_string, time_intervals = lucC_interval("2000-08-01","2017-09-12"))
  output.tb3 <- dplyr::bind_rows(output.tb3, temp_final.tb)
  
  # update progress bar
  setTxtProgressBar(progress_bar, x)#
  
}
output.tb3
close(progress_bar)#

data.frame(table(output.tb3$end_date,output.tb3$label))
data.frame(table(lubridate::year(output.tb3$end_date), output.tb3$label))

# entire data
#save(output.tb3, file = "~/Desktop/transition_FPScDc.RData")
#output.tb3 <- get(load(file = "~/Desktop/ESTUDO_TESE/Studies/Itanhanga/transition_FPScDc.RData"))

# plots
lucC_plot_maps_input(df_new_2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#1b791f", "#929e6e", "#66CC00", "#f5e7a1", "#FFB266")) # secondary_vegetation

#png(filename = "~/Desktop/fig_5_area_sequence.png", width = 7, height = 7, units = 'in', res = 300)
lucC_plot_maps_events(output.tb3, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#1b791f", "#929e6e", "#66CC00", "#f5e7a1", "#FFB266"), shape_point = 0 ,colour_point = "black", size_point = 1.5) 
#dev.off()

png(filename = "~/Desktop/fig_6_sequence.png", width = 7, height = 5, units = 'in', res = 300)
lucC_plot_sequence_events(output.tb3, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#FFB266", "#929e6e", "#f5e7a1")) # "#FFB266", "#1b791f", "#929e6e", "#f5e7a1"
dev.off()

png(filename = "~/Desktop/fig_7_barplot.png", width = 7, height = 5, units = 'in', res = 300)
lucC_plot_barplot_events(output.tb3, custom_palette = TRUE, RGB_color = c("#FFB266", "#929e6e", "#f5e7a1"), pixel_resolution = 231.6564) 
dev.off()



###########################
# open dataset with 6 labels - secondary vegetation
df_new <- get(load("~/Desktop/ESTUDO_TESE/Studies/Itanhanga/itanhanga_TWDTW_postprocess.RData"))
df_new
# amount of data for class
data.frame(table(df_new$label))

selection <- read.csv(file = "~/Desktop/ESTUDO_TESE/Studies/Itanhanga/ita_selection.csv", sep = ",", stringsAsFactors = FALSE)
sel <- selection$id

# select only points into this select region region from new label with 5 classes
df_label_2 <- dplyr::filter(df_label, df$index %in% sel)
df_label_2
# amount of data for class
data.frame(table(df_label_2$label))

# select only points into this select region region from secondary vegetation = 6 classes
df_new_2 <- dplyr::filter(df_new, df_new$index %in% sel)
df_new_2
# amount of data for class
data.frame(table(df_new_2$label))


#png(filename = "~/Desktop/fig2_secondary_vegetation.png", width = 7, height = 7, units = 'in', res = 300)
lucC_plot_maps_input(df_new_2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#1b791f", "#929e6e", "#66CC00","#f5e7a1", "#FFB266")) # , "#66CC00" secondary_vegetation
#dev.off()

png(filename = "~/Desktop/fig2_secondary_vegetation_events.png", width = 7, height = 7, units = 'in', res = 300)
lucC_plot_maps_events(output_df, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#9b7447", "#1b791f", "#929e6e", "#66CC00","#f5e7a1", "#FFB266"), size_square =1.5) 
dev.off()


###########################

library(sits)

#pattern <- sits_getdata("./inst/patterns/patterns_Damien_Ieda_Rodrigo_15classes_3bands_Water.json")
pattern <- sits_getdata("./inst/patterns/temporal_pattern_example.json")

sits_plot(pattern, type = "patterns")

sel <- c("Fallow_Cotton", "Pasture1", "Forest", "Soybean_Cotton", "Water")
pattern1 <- dplyr::filter(pattern, pattern$label %in% sel)
sits_plot(pattern1, type = "patterns")

#sits_save(pattern1, "~/Desktop/patterns_example.json")



#######################################################


########### Novo Horizonte do Norte - MT

library(lucC)

lucC_starting_point()

nhn_1.tb <- lucC_fromJSON("~/Desktop/ESTUDO_TESE/Studies/NHN_p1.json")
nhn_1.tb

nhn_2.tb <- lucC_fromJSON("~/Desktop/ESTUDO_TESE/Studies/NHN_p2.json")
nhn_2.tb

nhn_3.tb <- lucC_fromJSON("~/Desktop/ESTUDO_TESE/Studies/NHN_p3.json")
nhn_3.tb

nhn_4.tb <- lucC_fromJSON("~/Desktop/ESTUDO_TESE/Studies/NHN_p4.json")
nhn_4.tb

nhn2_1.tb <- lucC_fromJSON("~/Desktop/ESTUDO_TESE/Studies/new_nhn2_id_row_1.json")
nhn2_1.tb

nhn2_2.tb <- lucC_fromJSON("~/Desktop/ESTUDO_TESE/Studies/new_nhn2_id_row_2.json")
nhn2_2.tb

nhn2_3.tb <- lucC_fromJSON("~/Desktop/ESTUDO_TESE/Studies/new_nhn2_id_row_3.json")
nhn2_3.tb


nhn_entire <- dplyr::bind_rows(nhn_1.tb, nhn_2.tb, nhn_3.tb, nhn_4.tb, nhn2_1.tb, nhn2_2.tb, nhn2_3.tb)
nhn_entire
# 18625 - 1631 = 16994
# remove duplicated rows
length(which(duplicated(nhn_entire$time_series)))
nhn_entire <- nhn_entire[!duplicated(nhn_entire$time_series),]

plot(nhn_entire$longitude, nhn_entire$latitude)


