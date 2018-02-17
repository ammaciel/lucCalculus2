#*********************************
# Secondary_vegetation
#*********************************

## Question - Which "Forest" areas have been turned in secondary-vegetation after anyclass? 
# o = geo-objects, the own df_input data.frame
library(lucC)

# always run this because the value of sinusoidal lost decimal values
lucC_starting_point()

df_new <- get(load("~/Desktop/Sinop_TWDTW_label_3_new_names.tb.RData"))
df_new

# preparing data
data_tb <- df_new %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01") %>% 
  dplyr::select(longitude, latitude, start_date, end_date, label, id, index)

data_tb <- data_tb[order(data_tb$index),] 
data_tb

data.frame(table(data_tb$label))

#---------------------------------
# 1. Discover forest emerged after other type of land use
# where locations will be characterised as secondary vegetation
#---------------------------------

# Start count time ----
ptm <- proc.time()
ptm1 <- Sys.time()

# p = properties of objects 
p1 <- "Forest" 

p2 <- "Cerrado"
p3 <- "Pasture"
p4 <- "Single_cropping"
p5 <- "Double_cropping"
p6 <- "Water"

# te = interval:
t1 <- lucC_interval("2000-09-01","2001-09-01")
t2 <- lucC_interval("2001-09-01","2016-09-01")

# Test occur for many time series in a dataframe
# Test holds for one time serie
question_holds <- function(data.tb){
  #  data.tb <- tem$`3`
  aux.df = NULL
  temp.df = NULL
  
  if ((nrow(ev1.in1 <- lucC_predicate_holds(data.tb, p1, t1)) >= 1 | #forest in first interval
       
       nrow(ev2.in1 <- lucC_predicate_holds(data.tb, p2, t1)) >= 1 | # other classes in first interval
       nrow(ev3.in1 <- lucC_predicate_holds(data.tb, p3, t1)) >= 1 |
       nrow(ev4.in1 <- lucC_predicate_holds(data.tb, p4, t1)) >= 1 |
       nrow(ev5.in1 <- lucC_predicate_holds(data.tb, p5, t1)) >= 1 |
       nrow(ev6.in1 <- lucC_predicate_holds(data.tb, p6, t1)) >= 1
       
  ) 
  &
  (	  nrow(ev2.in2 <- lucC_predicate_holds(data.tb, p2, t2)) >= 1 | # other classes in second interval
      nrow(ev3.in2 <- lucC_predicate_holds(data.tb, p3, t2)) >= 1 |
      nrow(ev4.in2 <- lucC_predicate_holds(data.tb, p4, t2)) >= 1 |
      nrow(ev5.in2 <- lucC_predicate_holds(data.tb, p5, t2)) >= 1 |
      nrow(ev6.in2 <- lucC_predicate_holds(data.tb, p6, t2)) >= 1
  ) 
  &
  nrow(ev1.in2 <- lucC_predicate_holds(data.tb, p1, t2)) >= 1 # if occur forest in second interval 
  & 
  # verify if forest discovered in interval 2 occur after start_date other classes. 
  # Or f is different from first year 
  # If it is TRUE, this forest is a secondary_vegetation  
  nrow(event3 <- ev1.in2[which(ev1.in2$start_date > min(
    ev2.in2$start_date, ev3.in2$start_date,
    ev4.in2$start_date, ev5.in2$start_date,
    ev6.in2$start_date, na.rm=TRUE) | 
    (ev1.in2$label != head(data.tb$label,1))),]) >=1 
  
  ){
    # temp.df <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1,
    #                ev2.in2, ev3.in2, ev4.in2, ev5.in2,
    #                event3)
    # temp.df <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1, ev6.in1, event3)
    temp.df <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1, ev6.in1, event3)
    aux.df <- dplyr::bind_rows(aux.df,temp.df)
  } else {
    aux.df <- dplyr::bind_rows(aux.df, aux.df)
  }
  # remove duplicated rows
  aux.df <- aux.df[!duplicated(aux.df),]
  
  data.frame(aux.df)
  
}

temp.tb <- data_tb
# temp.tb <- data_tb[which(data_tb$index == 2367),] #43261 2367 6381 --> points to test
# temp.tb <- data_tb[which(data_tb$index >= 1 & data_tb$index <= 100),] --> points to test
# temp.tb
# question_holds(temp.tb)

output_sec_veg_df = data.frame(do.call("rbind", parallel::mclapply(X = split(temp.tb, temp.tb$index), 
                                                                   mc.cores=2, #parallel::detectCores(),
                                                                   FUN = question_holds)))
output_sec_veg_df
rownames(output_sec_veg_df) <- NULL

# Stop count time ----
end_proc <- proc.time() - ptm
end_proc
end_time <- Sys.time() - ptm1
end_time
#---------------------------------


#---------------------------------
# 2. Replace forest discovered previously into original data set
#---------------------------------

# data set ogirinal
df_input <- data_tb

# data set with events
df_temp <- output_sec_veg_df

# remove first line of data.frame that contain the from == "2000-09-01" and change by other label
df_first_line <- df_temp[which(df_temp$start_date == "2000-09-01"),]
df_posProc <- df_temp[which(df_temp$start_date != "2000-09-01"),]

# replace labels of Forest to Secondary-vegetation
df_posProc$label <- as.character(df_posProc$label)
df_posProc$label[df_posProc$label == "Forest"] <- "Secondary_vegetation"

# merge first line dataframe with pos-processing dataframe
df_pos <- dplyr::bind_rows(df_first_line,df_posProc)

# original data
df_temp2 <- df_input 

# temp3 dataframe with only id lines not in pos dataframe
df_temp3 <- df_temp2[!(df_temp2$id %in% df_pos$id),]

# merge df_input without id with renamed with lines without changes
df_new_sv <- dplyr::bind_rows(df_pos,df_temp3)
df_new_sv <- data.frame(df_new_sv[order(df_new_sv$id),]) # order
#rownames(df_new_sv) <- 1:nrow(df_new_sv)

# return as tibble 
df_new_sv <- tibble::as_tibble(df_new_sv)
rownames(df_new_sv) <- NULL # don't jump this

# show data
head(df_new_sv)

# remove variables of environment
remove(df_temp,df_first_line,df_posProc,df_pos,df_temp2,df_temp3)

# use lucC_toJSON despite of decimal digits
save(df_new_sv, file = "~/Desktop/Sinop_TWDTW_label_4_secondary.tb.RData")


#-------------
# Plots
#-------------

df_new_new <- get(load("~/Desktop/Sinop_TWDTW_label_4_secondary.tb.RData"))
df_new_new

lucC_plot_maps_input(df_new_new, EPSG_WGS84 = FALSE, custom_palette = FALSE, RGB_color = c("#9b7447", "#FFB266", "#1b791f", "#929e6e", "#66CC00","#f5e7a1", "#284fcc")) # , "#66CC00" secondary_vegetation

lucC_plot_bar_events(df_new_sv[which(df_new_sv$label == "Forest"| df_new_sv$label == "Secondary_vegetation"),], custom_palette = FALSE, RGB_color = "#1b791f", pixel_resolution = 231.6465, side_by_side = TRUE)


