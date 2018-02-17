####
# Example: read a json file using lucC_fromJSON and 
# adequate data to extrat a lot of events

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

lucC_plot_bar_events(output.tb3, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1"), pixel_resolution = 250) 

lucC_plot_sequence_events(output.tb3, show_y_index = TRUE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e", "#f5e7a1")) 




# Test with one point
data_tb <- example_TWDTW[which(example_TWDTW$index == 29),]
data_tb

temp_final.tb <- lucC_event_transitions(data_tb, properties = c("Single_cropping", "Pasture", "Double_cropping"))
temp_final.tb





