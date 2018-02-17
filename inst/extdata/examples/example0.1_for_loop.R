#*********************************
# Question examples
#*********************************
#---------------------------------
# Question 1 - Only events of Pasture
# o = geo-objects, the own df_input data.frame
#---------------------------------
library(lucC)

data("example_TWDTW")
example_TWDTW

# alter start_date and end_date to a especific range in order to extract events
example_1.tb <- example_TWDTW %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_1.tb

# p = properties of objects :
p1 <- "Pasture"

# t = interval:
t1 <- lucC_interval("2000-09-01","2017-03-01")

# Test occur for many time series
QuestionOccurs <- function(data_tb, p, t){
  
  tb <- data_tb 
  coord <- unique(tb$index)
  output.tb <- tb[FALSE,]
  
  for(x in 1:length(coord)){
    #x=1
    temp <- tb[which(as.character(tb$index) == coord[x]),]
    
    if (nrow(event2 <- lucC_predicate_holds(temp, p1, t1)) >= 1
        
    ){
      temp0 <- rbind(event2)
    } else {
      temp0 <- NULL
    }
    output.tb <- dplyr::bind_rows(output.tb,temp0)
  }
  return(output.tb)
}

output.tb <- QuestionOccurs(example_1.tb, p = p1, t = t1)
output.tb

remove(t1,p1)

# plot results
lucC_plot_maps_input(example_1.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))

#plot events
lucC_plot_maps_events(output.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "black", size_point = 2.3) 

lucC_plot_bar_events(output.tb, custom_palette = TRUE, RGB_color = "#929e6e", pixel_resolution = 250) 

lucC_plot_sequence_events(output.tb, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e") 


#---------------------------------
# Question 2 - Only one point
# o = geo-objects, the own df_input data.frame
#---------------------------------

data("example_TWDTW")
example_TWDTW

# select only one time serie with index equals 13
# alter start_date and end_date to a especific range in order to extract events
example_2.tb <- example_TWDTW %>% 
  dplyr::filter(., .$index == 13) %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_2.tb

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"

# t = interval:
t1 <- lucC_interval("2000-09-01","2004-09-01")
t2 <- lucC_interval("2004-09-01","2017-09-01")

# Test occur for one time serie
QuestionOccurs <- function(data_tb, p, t){
 
  output.tb <- data_tb[FALSE,]
  data_tb
  
  if (nrow(ev1 <- lucC_predicate_holds(data_tb, p1, t1)) >= 1 &
      nrow(ev2 <- lucC_predicate_holds(data_tb, p2, t2)) >= 1 &
      
      isTRUE(lucC_relation_meets(tail(lucC_interval(ev1$start_date, ev1$end_date), 1),
                                  head(lucC_interval(ev2$start_date, ev2$end_date),1)))
  ){
    temp0 <- rbind(ev1,ev2)
  } else {
    temp0 <- NULL
  }
  output.tb <- dplyr::bind_rows(output.tb,temp0)

  return(output.tb)
}

output.tb2 <- QuestionOccurs(example_2.tb, p = c(p1, p2), t = c(t1,t2))
output.tb2

remove(p1, p2, t1, t2)

# plot
lucC_plot_maps_input(example_2.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"))

lucC_plot_maps_events(output.tb2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"), shape_point = 4, colour_point = "blue", size_point = 8) 

lucC_plot_bar_events(output.tb2, custom_palette = FALSE) 

lucC_plot_sequence_events(output.tb2, show_y_index = FALSE, end_date = "2017-03-01") 
#lucC_plot_barplot_events(output_df2[which(output_df2$label == "Forest"),]) 


#---------------------------------
# Question 3 - Only transition "Forest", "Pasture", "Single_cropping", "Double_cropping"
# o = geo-objects, the own df_input data.frame
#---------------------------------
library(lucC)

lucC_starting_point()

data("example_TWDTW")
example_TWDTW

example_3.tb <- example_TWDTW %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01")

example_3.tb

# p = properties of objects :
p1 <- c("Forest", "Pasture", "Single_cropping", "Double_cropping")

# t = interval:
t1 <- lucC_interval("2000-08-01","2017-03-01")

tb <- example_3.tb
output.tb3 <- tb[FALSE,]
coord <- unique(tb$index)

# Apply for each time series based on index
for(x in 1:length(coord)){
  temp.tb <- tb[which(as.character(tb$index) == coord[x]),]
  temp_final.tb <- lucC_event_transitions(temp.tb, properties = p1, time_intervals = t1)
  output.tb3 <- dplyr::bind_rows(output.tb3, temp_final.tb)
}
output.tb3

# plots
lucC_plot_maps_input(example_3.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"))

lucC_plot_maps_events(output.tb3, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), shape_point = 0, colour_point = "blue", size_point = 2.3) 

lucC_plot_barplot_events(output.tb3, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), pixel_resolution = 250) 

lucC_plot_sequence_events(output.tb3, show_y_index = TRUE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 



