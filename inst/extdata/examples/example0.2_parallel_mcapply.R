# install or remove package
#devtools::install_github("ammaciel/lucC")
#remove.packages("lucC")

# load library
library(lucC)
lucC_starting_point()

#--------------------------
# Example 1 - Only one type of event -- github
# Question to discover a Pasture area to holds during entire interval
# load data example
data("example_TWDTW")

data_tb <- example_TWDTW %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01") %>% 
  dplyr::select(longitude, latitude, start_date, end_date, label, id, index)
data_tb

lucC_plot_maps_input(data_tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c( "#FFB266", "#1b791f",  "#929e6e", "#f5e7a1"))

# p = properties of locations :
p1 <- "Pasture"

# t = interval:
t1 <- lucC_interval("2000-09-01","2017-03-01")

# Test holds for many time series 
question_holds <- function(data.tb){
  
  aux.df = NULL
  if (nrow(event2 <- lucC_predicate_holds(data.tb, p1, t1)) >= 1)
    aux.df <- event2
  else
    aux.df <- NULL
  
  data.frame(aux.df)
  
}

temp.tb <- data_tb

output.tb = data.frame(do.call("rbind", parallel::mclapply( X = split(temp.tb, temp.tb$index), 
                                                            mc.cores=1, #parallel::detectCores(),
                                                            FUN = question_holds)))
output.tb

# See results
# plot original
lucC_plot_maps_input(data_tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f",  "#929e6e","#f5e7a1"))

# plot events 
lucC_plot_maps_events(output.tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f",  "#929e6e","#f5e7a1"), shape_point = 0, colour_point = "black", size_point = 2.3)

# bar plot
lucC_plot_bar_events(output.tb, custom_palette = TRUE, RGB_color = "#929e6e", pixel_resolution = 231.6564, side_by_side = FALSE)

# plot sequence
lucC_plot_sequence_events(output.tb, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e", show_y_index = FALSE)



#--------------------------
# Example 2 - with two events -- github
# Question to discover a Forest areas have been turned into Pasture areas

# load library
library(lucC)
lucC_starting_point()

# load data example
data("example_TWDTW")
example_TWDTW

# alter start_date and end_date to a especific range in order to extract events
# because we work with annual intervals
# select only one time serie with index equals 13
data_tb <- example_TWDTW %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01") %>% 
  dplyr::select(longitude, latitude, start_date, end_date, label, id, index) %>% 
  dplyr::filter(., .$index == 13)
data_tb

# p = properties of locations :
p1 <- "Forest"
p2 <- "Pasture"

# t = interval:
t1 <- lucC_interval("2000-09-01","2004-09-01")
t2 <- lucC_interval("2004-09-01","2017-09-01")

# Test holds for one time serie
question_holds <- function(data.tb){
  
  aux.df = NULL
  
  if (nrow(ev1 <- lucC_predicate_holds(data.tb, p1, t1)) >= 1 &
      nrow(ev2 <- lucC_predicate_holds(data.tb, p2, t2)) >= 1 ){
    
    if(nrow(ev1) != 0 & nrow(ev2) != 0){
      
      if(isTRUE(lucC_relation_meets(tail(lucC_interval(ev1$start_date, ev1$end_date), 1),
                                    head(lucC_interval(ev2$start_date, ev2$end_date), 1))))
        aux.df <- rbind(ev1,ev2)
    } else 
      aux.df <- NULL
    
  } else {
    aux.df <- NULL
  }
  
  data.frame(aux.df)
}

temp.tb <- data_tb
output.tb2 = data.frame(do.call("rbind", parallel::mclapply( X = split(temp.tb, temp.tb$index), 
                                                             mc.cores=1, #parallel::detectCores(),
                                                             FUN = question_holds)))
output.tb2

# See results
lucC_plot_maps_input(data_tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"))

lucC_plot_maps_events(output.tb2, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f", "#929e6e"), shape_point = 4, colour_point = "blue", size_point = 8)

lucC_plot_bar_events(output.tb2, custom_palette = TRUE, RGB_color = c("#1b791f", "#929e6e"), pixel_resolution = 250, side_by_side = FALSE) 

#lucC_plot_sequence_events(output.tb2, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#1b791f", "#929e6e"),relabel = TRUE, original_labels = c("Forest", "Pasture"),new_labels = c("F","P"))

#lucC_plot_frequency_events(output.tb2, custom_palette = TRUE, RGB_color = c("#1b791f", "#929e6e"), pixel_resolution = 250) 



#--------------------------
# Example 3 - with two events -- github
# Question to discover a Forest areas have been turned into Pasture areas

# load library
library(lucC)
lucC_starting_point()

# load data example
data("example_TWDTW")
example_TWDTW

# alter start_date and end_date to a especific range in order to extract events
# because we work with annual intervals
data_tb <- example_TWDTW %>% 
  lucC_standard_date_events(data_tb = ., month_year = "09", day_month = "01") %>% 
  dplyr::select(longitude, latitude, start_date, end_date, label, id, index) 
data_tb

# p = properties of locations :
p1 <- c("Forest", "Pasture", "Single_cropping", "Double_cropping")

# t = interval:
t1 <- lucC_interval("2000-09-01","2017-09-01")

# Test holds for one time serie
question_holds <- function(data.tb){
  
  aux.df = NULL
  
  aux.df <- lucC_event_transitions(data.tb, properties = p1, time_intervals = t1)
  
  data.frame(aux.df)
}

temp.tb <- data_tb
output.tb3 = data.frame(do.call("rbind", parallel::mclapply( X = split(temp.tb, temp.tb$index), 
                                                             mc.cores=1, #parallel::detectCores(),
                                                             FUN = question_holds)))
output.tb3


# See results
lucC_plot_maps_input(data_tb, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f",  "#929e6e","#f5e7a1"))

lucC_plot_maps_events(output.tb3, EPSG_WGS84 = TRUE, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f",  "#929e6e","#f5e7a1"), shape_point = 0, colour_point = "blue", size_point = 2.3)

lucC_plot_bar_events(output.tb3, custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f",  "#929e6e","#f5e7a1"), pixel_resolution = 250, side_by_side = FALSE) 

lucC_plot_sequence_events(output.tb3, show_y_index = FALSE, end_date = "2017-03-01", custom_palette = TRUE, RGB_color = c("#FFB266", "#1b791f",  "#929e6e","#f5e7a1"), relabel = FALSE, original_labels = c("Double_cropping", "Forest", "Pasture", "Single_cropping"), new_labels = c("DC","F","P","SC"))

#lucC_plot_frequency_events(output.tb2, custom_palette = TRUE, RGB_color = c("#1b791f", "#929e6e"), pixel_resolution = 250) 

