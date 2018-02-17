####
# Example: read a json file using lucC_fromJSON and 
# perform some operations using predicates and plots

library(lucC)

lucC_starting_point()

file_json = "./inst/extdata/patterns/example_TWDTW.json"
input_tb_raw_json <- file_json %>% 
  lucC_fromJSON() 
input_tb_raw_json

#remove columns and pass to lucC format
new_area_Sinop <- input_tb_raw_json %>% 
  dplyr::select(longitude,latitude,label,start_date,end_date,id,index) %>% 
  lucC_data_preparation()

new_area_Sinop

classes <- unique(new_area_Sinop$label)

# plot maps input data
lucC_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)

# define interval
time_ex1 <- lucC_interval("2002-01-01", "2014-01-01")

# using occur for a class from classes variable
ts_occur1 <- lucC_predicate_holds(locations = input_tb_raw_json, location_properties = "Pasture", time_intervals = time_ex1)
ts_occur1

ts_occur2 <- lucC_predicate_holds(locations = input_tb_raw_json, location_properties = "Forest", time_intervals = time_ex1)
ts_occur2

# events over input map
lucC_plot_maps_events(ts_occur1, EPSG_WGS84 = TRUE)
lucC_plot_maps_events(ts_occur2, EPSG_WGS84 = TRUE)

# plot sequence of events
lucC_plot_sequence_events(ts_occur1, start_date = "2000-01-01", end_date = "2016-12-31")
lucC_plot_sequence_events(ts_occur2, start_date = "2000-01-01", end_date = "2016-12-31")

# plot barplot of events
lucC_plot_bar_events(ts_occur1)
lucC_plot_bar_events(ts_occur2)
