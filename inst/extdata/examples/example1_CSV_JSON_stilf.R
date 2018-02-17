
###############
## Ctrl+Shift+M %>% 
## Alt (-) <- 
###############

#####
# Example: read a csv as lucC format
library(lucC)

lucC_starting_point()

file = "./data/example_TWDTW.csv"

input_tb_raw_csv <- file %>% 
  read.csv(sep = ",", header = TRUE) %>% 
  lucC_data_preparation()
input_tb_raw_csv

#####
# Example: read a csv as using lucC_fromCSV from package lucC

library(lucC)

lucC_starting_point()

file_csv = "./data/example_TWDTW.csv"

input_tb_csv <- file_csv %>% 
  lucC_fromCSV(separator = ",", header_file = TRUE) #%>% 
input_tb_csv

# save the input as json format
output_file = "~/Desktop/example_json.json"
lucC_toJSON(input_tb_csv, output_file)

#####
# Example: apply some operations use Allen's relations

library(lucC)

lucC_starting_point()

# define some intervals to example
time1 <- lucC_interval("2011-09-01","2011-10-01")
time2 <- lucC_interval("2011-09-15","2011-11-01")
time3 <- lucC_interval("2011-10-01","2011-11-01")
time4 <- lucC_interval("2011-10-01","2011-11-01")

# verify some interval are TRUE or FALSE using Allen's relations
lucC_relation_before(time1,time4)

lucC_relation_overlaps(time1,time2)

lucC_relation_follows(time1,time3)

#####
# Example: read a json file using lucC_fromJSON and 
# perform some operations using predicates

library(lucC)

lucC_starting_point()

json_file = "./inst/extdata/patterns/example_TWDTW.json"

input_tb_json <- json_file %>% 
  lucC_fromJSON()  
input_tb_json

# example of application
time_ex1 <- lucC_interval("2001-01-01", "2003-01-01")
time_ex1
time_ex2 <- lucC_interval("2005-01-01", "2010-01-01")
time_ex2

# object_properties
properties <- "Forest"

# example predicate holds
lucC_predicate_holds(locations = input_tb_json, location_properties = "Forest", time_intervals = time_ex1)

lucC_predicate_holds(locations = input_tb_json, location_properties = properties, time_intervals = time_ex2)


#####
# Example: read a json file using lucC_fromJSON and 
# perform some operations using predicates and plots

library(lucC)

lucC_starting_point()

file_json = "./inst/extdata/patterns/example_TWDTW.json"
input_tb_raw_json <- file_json %>% 
  lucC_fromJSON() 
input_tb_raw_json

# plot maps input data
lucC_plot_maps_input(input_tb_raw_json, EPSG_WGS84 = TRUE)

# define interval
time_ex1 <- lucC_interval("2002-01-01", "2014-01-01")
time_ex1

# using occur
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


#####
# Example: save images as GeoTIFF file to open in GIS after
library(lucC)

lucC_starting_point()

file_json = "./inst/extdata/patterns/example_TWDTW.json"
input_tb_raw_json <- file_json %>% 
  lucC_fromJSON() 
input_tb_raw_json

# save rasters in folder 
# create a folder before run this 
lucC_toGeoTIFF (input_tb_raw_json, "~/Desktop/raster")

# #------------------ verificar depois
# pts <- sp::spsample(pts,type="regular",cellsize=231.6564)
# pts1 <-  points2grid(pts, tolerance = 0.186495)
# sp::points2grid(SpatialPoints(pts), tolerance = 0.186495)
# sp::fullgrid(pts)=FALSE 
# sp::gridded(pts) = TRUE
# #------------------
