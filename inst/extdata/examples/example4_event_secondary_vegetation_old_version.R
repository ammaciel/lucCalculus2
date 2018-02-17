########################################
#+++++++++++++++++++++++++++++
# Secondary_vegetation
#+++++++++++++++++++++++++++++ 
# AREA "Part_Sinop" - IJGIS - pattern_classification_14patterns_3bands_Area1_Sinop
## Question - Which "Forest" areas have been turned in secondary-vegetation after anyclass? 
# o = geo-objects, the own df_input data.frame

# p = properties of objects 
p1 <- "Forest" 

p2 <- "Savanna"
p3 <- "Soybean_Fallow2"
p4 <- "Soybean_NonComerc2"
p5 <- "Soybean_Fallow1"
p6 <- "Soybean_Comerc2"
p7 <- "Soybean_Comerc1"
p8 <- "Soybean_NonComerc1"
p9 <- "Pasture2"
p10 <- "Pasture1"
p11 <- "Soybean_Pasture"
p12 <- "NonComerc_Cotton"
p13 <- "Fallow_Cotton"
p14 <- "Soybean_Cotton"

# te = interval:
t1 <- lucC_interval("2000-09-01","2001-09-01")
t2 <- lucC_interval("2001-09-01","2016-09-01")

##

# Test occur for many time series in a dataframe
QuestionOccurs <- function(data_tb, p, t){
  
  df <- data_tb 
  
  coord <- unique(df$index)
  output_df <- df[FALSE,]
  
  # create progress bar
  progress_bar <- txtProgressBar(min = 0, max = length(coord), style = 3)###
  
  for(x in 1:length(coord)){
      
    temp <- df[which(as.character(df$index) == coord[x]),]
    
    if ((nrow(ev1.in1 <- lucC_predicate_holds(temp, p1, t1)) >= 1 | #forest in first interval
         
         nrow(ev2.in1 <- lucC_predicate_holds(temp, p2, t1)) >= 1 | # other classes in first interval
         nrow(ev3.in1 <- lucC_predicate_holds(temp, p3, t1)) >= 1 |
         nrow(ev4.in1 <- lucC_predicate_holds(temp, p4, t1)) >= 1 |
         nrow(ev5.in1 <- lucC_predicate_holds(temp, p5, t1)) >= 1 |
         nrow(ev6.in1 <- lucC_predicate_holds(temp, p6, t1)) >= 1 |
         nrow(ev7.in1 <- lucC_predicate_holds(temp, p7, t1)) >= 1 |
         nrow(ev8.in1 <- lucC_predicate_holds(temp, p8, t1)) >= 1 |
         nrow(ev9.in1 <- lucC_predicate_holds(temp, p9, t1)) >= 1 |
         nrow(ev10.in1 <- lucC_predicate_holds(temp, p10, t1)) >= 1 |
         nrow(ev11.in1 <- lucC_predicate_holds(temp, p11, t1)) >= 1 |
         nrow(ev12.in1 <- lucC_predicate_holds(temp, p12, t1)) >= 1 |
         nrow(ev13.in1 <- lucC_predicate_holds(temp, p13, t1)) >= 1 |
         nrow(ev14.in1 <- lucC_predicate_holds(temp, p14, t1)) >= 1 
         
    ) 
    &
    (	  nrow(ev2.in2 <- lucC_predicate_holds(temp, p2, t2)) >= 1 | # other classes in second interval
        nrow(ev3.in2 <- lucC_predicate_holds(temp, p3, t2)) >= 1 |
        nrow(ev4.in2 <- lucC_predicate_holds(temp, p4, t2)) >= 1 |
        nrow(ev5.in2 <- lucC_predicate_holds(temp, p5, t2)) >= 1 |
        nrow(ev6.in2 <- lucC_predicate_holds(temp, p6, t2)) >= 1 |
        nrow(ev7.in2 <- lucC_predicate_holds(temp, p7, t2)) >= 1 |
        nrow(ev8.in2 <- lucC_predicate_holds(temp, p8, t2)) >= 1 |
        nrow(ev9.in2 <- lucC_predicate_holds(temp, p9, t2)) >= 1 |
        nrow(ev10.in2 <- lucC_predicate_holds(temp, p10, t2)) >= 1 |
        nrow(ev11.in2 <- lucC_predicate_holds(temp, p11, t2)) >= 1 |
        nrow(ev12.in2 <- lucC_predicate_holds(temp, p12, t2)) >= 1 |
        nrow(ev13.in2 <- lucC_predicate_holds(temp, p13, t2)) >= 1 |
        nrow(ev14.in2 <- lucC_predicate_holds(temp, p14, t2)) >= 1 
    ) 
    &
    nrow(ev1.in2 <- lucC_predicate_holds(temp, p1, t2)) >= 1 # if occur forest in second interval 
    & 
    # verify if forest discovered in interval 2 occur after start_date other classes. Or f is different from first year 
    # If TRUE, this forest is a secondary_vegetation  
    nrow(event3 <- ev1.in2[which(ev1.in2$start_date > min(
                                  ev2.in2$start_date, ev3.in2$start_date,
                                  ev4.in2$start_date, ev5.in2$start_date,
                                  ev6.in2$start_date, ev7.in2$start_date,
                                  ev8.in2$start_date, ev9.in2$start_date,
                                  ev10.in2$start_date, ev11.in2$start_date,
                                  ev12.in2$start_date, ev13.in2$start_date,
                                  ev14.in2$start_date, na.rm=TRUE) | 
                                 (ev1.in2$label != head(temp$label,1))),]) >=1 
    
    ){
      temp0 <- rbind(ev1.in1, ev2.in1, ev3.in1, ev4.in1, ev5.in1, ev6.in1, ev7.in1,
                     ev8.in1, ev9.in1, ev10.in1, ev11.in1, ev12.in1, ev13.in1, ev14.in1,
                     ev2.in2, ev3.in2, ev4.in2, ev5.in2, ev6.in2, ev7.in2,
                     ev8.in2, ev9.in2, ev10.in2, ev11.in2, ev12.in2, ev13.in2, ev14.in2,
                     event3)
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
data_sec_veg_occurs <- QuestionOccurs(area_tb, p = c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14), t = c(t1,t2))

rm(list = ls(pattern="^p[0-9]")) # remove classes
remove(t1,t2) # remove intervals

# test with one point
# ab <- area_tb[which(area_tb$index == 27),]
# data_sec_veg_occurs1 <- QuestionOccurs(ab, p = c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14), t = c(t1,t2))
# data_sec_veg_occurs1

# remove duplicated rows
length(which(duplicated(data_sec_veg_occurs)))
#data_sec_veg_occurs <- data_sec_veg_occurs[!duplicated(data_sec_veg_occurs),]

lucC_plot_maps_input(area_tb, EPSG_WGS84 = TRUE) 
lucC_plot_maps_events(data_sec_veg_occurs, EPSG_WGS84 = TRUE) 

lucC_plot_maps_input(df_input_new, EPSG_WGS84 = TRUE) 
lucC_plot_barplot_events(df_input_new[which(df_input_new$label == "Forest"),]) 

#############################
# Rules to change Forest to Secondary_vegetation
#########

df_input <- area_tb

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
df_input_new <- bind_rows(df_pos,df_temp3)
df_input_new <- data.frame(df_input_new[order(df_input_new$id),]) # order
#rownames(df_input_new) <- 1:nrow(df_input_new)

# return as tibble 
df_input_new <- tibble::as_tibble(df_input_new)

head(df_input_new)
# remove other
remove(df_temp,df_first_line,df_posProc,df_pos,df_temp2,df_temp3)

#############################

#lucC_toJSON(df_input_new, "./inst/area_Sinop/part_Sinop_postprocess.json")

