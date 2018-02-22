library(dplyr)

MT_samples <- "~/Desktop/Classi_MT_SVM/raster_sample_mt"
#MT_samples <- "~/Desktop/raster"

# create a rasterBrick with data
MT_samples_brick <- list.files(MT_samples,
                             full.names = TRUE,
                             pattern = ".tif$") %>%
  raster::stack(.) %>%
  raster::brick(.) %>%
  #rts::rts(.,timeline) %>%
  raster::writeRaster(., "~/Desktop/Classi_MT_SVM/sample_MT.tif", overwrite=TRUE)

# open files
file <- c("~/Desktop/Classi_MT_SVM/sample_MT.tif")
file

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
timeline

# create label with classified data from SVM method
#label <- as.character(c("Double_cropping", "Forest", "Pasture", "Single_cropping"))
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water"))
label

#library(sits)
# create a raster metadata file based on the information about the files
raster.tb <- sits::sits_coverage(service = "RASTER", product = "MOD13Q1", name = "Sample_region",
                                 timeline = timeline, bands = "NULL", files = file)
raster.tb

class(raster.tb$r_obj[[1]])

# new variable
rb_sits <- raster.tb$r_obj[[1]]

# resolution
raster::res(rb_sits)

#raster::plot(rb_sits)
names(rb_sits)

#-------------
# alter attributes using labels
rb_sits@data@attributes <- lapply(rb_sits@data@attributes, function(x)  {x <- data.frame(ID = c(1:length(label)), category = label)} )

colors <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1" )
rasterVis::levelplot(rb_sits, col.regions=colors) # par.settings=rasterVis::RdBuTheme


#------------- tests - intervals before, meets and follows
library(sits.LUC.Calculus)

first_raster.df <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
                  time_interval = c("2001-09-01","2004-09-01"),
                  relation_interval = "contains", label = label, timeline = timeline)
first_raster.df

second_raster.df <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
                       time_interval = c("2005-09-01","2014-09-01"),
                       relation_interval = "contains", label = label, timeline = timeline)

second_raster.df

# before
a <- lucC_relation_before(first_raster = first_raster.df, second_raster = second_raster.df)
a
a1 <- lucC_result_format(a)
a1
lucC_plot_sequence_events(a1, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(a1, custom_palette = FALSE)


b <- lucC_relation_meets(first_raster = first_raster.df, second_raster = second_raster.df)
b
b1 <- lucC_result_format(b)
b1
lucC_plot_sequence_events(b1, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(b1, custom_palette = FALSE)

c <- lucC_relation_follows(first_raster = first_raster.df, second_raster = second_raster.df)
c
c1 <- lucC_result_format(d)
c1
lucC_plot_sequence_events(c1, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(c1, custom_palette = FALSE)


#------------- tests - recur, evolve, convert
third_raster.df <- lucC_pred_recur(raster_obj = rb_sits, raster_class = "Forest",
                                    time_interval1 = c("2001-09-01","2003-09-01"),
                                    time_interval2 = c("2002-09-01","2007-09-01"),
                                    label = label, timeline = timeline)
third_raster.df

######-------------
# verify is time_interval1 is < time_interval2 in luc_pred_recur
######-------------

d1 <- lucC_result_format(third_raster.df)
d1
lucC_plot_sequence_events(d1, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(d1, custom_palette = FALSE)

#---------------

fourth_raster.df <- lucC_pred_evolve(raster_obj = rb_sits, raster_class1 = "Forest",
                                   time_interval1 = c("2001-09-01","2001-09-01"),
                                   raster_class2 = "Cerrado",
                                   time_interval2 = c("2002-09-01","2016-09-01"),
                                   label = label, timeline = timeline)
fourth_raster.df

e1 <- lucC_result_format(fourth_raster.df)
lucC_plot_sequence_events(e1, custom_palette = FALSE, show_y_index = FALSE)
lucC_plot_bar_events(e1, custom_palette = FALSE)


c <- c("20000-09-32")

c <- lucC_interval("2002-12-1","2006-10-3")
c
#.check_date_format <- function(time_interval = NULL){

  date <- try( as.Date( c, format= "%Y-%m-%d " ))
  if( class( date ) == "try-error" || is.na( date ) )
    stop( "\nDate format incorrect! -> YYYY-MM-DD\n" )
 # return(date)
#}

  .dateFormat <- function(date, format = "%Y-%m-%d") {
    tryCatch(!is.na(as.Date(date, format)), error = function(err) {FALSE})
  }

.dateFormat(c)

lubridate::is.interval(c)

.check_date_format(date)







lubridate::is.interval(c)


.
.
.





























# rb_sits2 <- rb_sits
# names(rb_sits2) <- as.character(timeline)
# biblinoames(rb_sits2)
# plot(rb_sits2)

# # plot
# raster::plot(rb_sits, 1)

# # define values to query
# property <- match("Forest", label) # Forest
# date_start <- match(lubridate::as_date('2001-09-01'), timeline) # 2001-09-01
# date_end <- match(lubridate::as_date('2006-09-01'), timeline) # 2006-09-01

# define values to query
property <- match("Pasture", label) # Forest
date_start <- match(lubridate::as_date('2007-09-01'), timeline) # 2001-09-01
date_end <- match(lubridate::as_date('2010-09-01'), timeline) # 2006-09-01

# subset with all lcoations from raster holds during a time interval
holds_raster <- function(ras.obj, property.ras, start_date.ras, end_date.ras) {
  temp <- raster::subset(ras.obj, start_date.ras:end_date.ras)
  output <- temp == property.ras
  return(output)
}

# apply holds_raster to obtain results
output_holds <- holds_raster(rb_sits, property, date_start, date_end) # rt
#raster::plot(output_holds)

rasterVis::levelplot(output_holds, col.regions=colors) #
#rasterVis::levelplot(output_holds2, col.regions=colors) #

longLatFromRaster <- NULL
# extract x, y, and values from raster output_holds
longLatFromRaster <- raster::rasterToPoints(output_holds)
#longLatFromRaster <- raster::as.data.frame(output_holds, xy = TRUE)

relation_allen = "contains"

if (relation_allen == "equals") {
  longLatFromRaster.df <- longLatFromRaster[
    rowSums(longLatFromRaster[,c(3:ncol(longLatFromRaster))] &
              !is.na(longLatFromRaster[,c(3:ncol(longLatFromRaster))])) == length(3:ncol(longLatFromRaster)),]
} else if (relation_allen == "contains")  {
  longLatFromRaster.df <- longLatFromRaster[
    rowSums(longLatFromRaster[,c(3:ncol(longLatFromRaster))] &
              !is.na(longLatFromRaster[,c(3:ncol(longLatFromRaster))])) > 0,]

}else{
  stop("\nInvalide option: 'equals' or 'contains'\n
         must be defined!\n")
}

longLatFromRaster.df
nrow(longLatFromRaster.df)

## test rows entire FALSE values
#dplyr::anti_join(longLatFromRaster,longLatFromRaster.df)

# define timeline from raster output_holds
timeline_holds = timeline[ timeline >= timeline[date_start] & timeline <= timeline[date_end]]

# alter column names of data.frame
colnames(longLatFromRaster.df)[c(3:ncol(longLatFromRaster.df))] <- as.character(timeline_holds)
longLatFromRaster.df

# alter label for original value in character
longLatFromRaster.df[,c(3:ncol(longLatFromRaster.df))] <-
  as.character(ifelse(longLatFromRaster.df[,c(3:ncol(longLatFromRaster.df))] == 1, property, NA))

longLatFromRaster.df

# information about number of lines
longLatFromRaster.df <- cbind(longLatFromRaster.df[,1:2],
                           id = 1:nrow(longLatFromRaster.df),
                           longLatFromRaster.df[,3:ncol(longLatFromRaster.df)])
head(longLatFromRaster.df)

# empty data.frame
holds_raster.df <- NULL

# extract values from longLatFromRaster.df to a data.frame format lucC
data.df <- function(x, nreps = length(timeline_holds)){
  label_idx <- 4:NROW(x)
  longitude <- x[1]
  latitude <- x[2]
  start_date <- as.character(lubridate::ymd(timeline_holds) - lubridate::years(1))
  end_date <- as.character(lubridate::ymd(timeline_holds))
  label <- as.character(x[label_idx])
  id <- 0
  index <- as.numeric(x[3])

  out <- data.frame(longitude = rep(longitude, times = nreps),
                    latitude = rep(latitude, times = nreps),
                    start_date = start_date,
                    end_date = end_date,
                    label = label,
                    id = rep(id, times = nreps),
                    index = index,
                    stringsAsFactors = FALSE)
  return(out)

}

holds_raster.df <- do.call("rbind", apply(longLatFromRaster.df, 1, data.df))
holds_raster.df

# with for loop
# # holds_raster.df <- NULL
# # extract values from longLatFromRaster to a data.frame format lucC
# for(i in 1:nrow(longLatFromRaster)){
#   nreps = length(timeline_holds)
#
#   longitude = as.double(rep(longLatFromRaster[i,1], times = nreps))
#   latitude = as.double(rep(longLatFromRaster[i,2], times = nreps))
#   start_date = as.character(lubridate::ymd(timeline_holds) - lubridate::years(1))
#   end_date = as.character(lubridate::ymd(timeline_holds))
#   label = as.character(longLatFromRaster[i,c(4:ncol(longLatFromRaster))])
#   id = 0
#   index = as.numeric(rep(i, times = length(timeline_holds)))
#
#   temp <- cbind(longitude, latitude, start_date, end_date, label, id, index)
#   holds_raster.df <- rbind(holds_raster.df,temp)
# }
#holds_raster.df
#holds_raster.df$id <- 1:nrow(holds_raster.df)
#head(holds_raster.df)
#str(holds_raster.df)

# create data.frame with results
output_holds_raster.df <- data.frame(longitude  = as.numeric(as.character(holds_raster.df$longitude)),
                                     latitude   = as.numeric(as.character(holds_raster.df$latitude)),
                                     start_date = lubridate::as_date(as.character(holds_raster.df$start_date)),
                                     end_date   = lubridate::as_date(as.character(holds_raster.df$end_date)),
                                     label      = as.character(ifelse(holds_raster.df$label == 1, property, NA)),
                                     id         = as.numeric(1:nrow(holds_raster.df)),
                                     index      = as.numeric(as.character(holds_raster.df$index)),
                                     stringsAsFactors = FALSE)

# remove factor
str(output_holds_raster.df)

# create tibble as result
output_holds_raster.tb <- tibble::as_tibble(output_holds_raster.df)
head(output_holds_raster.tb)

# remove location without values == NA
output_holds_raster.tb <- output_holds_raster.tb %>%
  tidyr::drop_na()

output_holds_raster.tb$id <- as.numeric(1:nrow(output_holds_raster.tb))

output_holds_raster.tb






# visualize plot with data
lucC::lucC_plot_bar_events(output_holds_raster.tb, custom_palette = FALSE, pixel_resolution = 231.656)
#lucC::lucC_plot_frequency_events(cellRaster.df2, custom_palette = FALSE, pixel_resolution = 231.656)

lucC::lucC_plot_maps_input(output_holds_raster.tb, EPSG_WGS84 = FALSE)
lucC::lucC_plot_maps_events(output_holds_raster.tb, EPSG_WGS84 = FALSE)


#-------------



# # merge two data frames by ID and Country columns
# total <- merge(a1 , a2, by=c("x","y"))
# total


# data.frame from raster to use LUC Calculus
# raster.df <- data.frame(longitude = as.double(),
#                         latitude = as.double(),
#                         start_date = as.Date(as.character()),
#                         end_date = as.Date(as.character()),
#                         label = as.character(),
#                         id = as.numeric(),
#                         index = as.double(),
#                         stringsAsFactors = FALSE)
# raster.df
# str(raster.df)






library(lucC)
lucC_starting_point()
# p = properties of locations :
p1 <- 4

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

temp.tb <- cellRaster.df2

output.tb = data.frame(do.call("rbind", parallel::mclapply( X = split(temp.tb, temp.tb$index),
                                                            mc.cores=1, #parallel::detectCores(),
                                                            FUN = question_holds)))
output.tb

# plot result
lucC_plot_maps_events(output.tb, EPSG_WGS84 = FALSE, custom_palette = FALSE)

# plots
lucC_plot_bar_events(output.tb, custom_palette = FALSE, pixel_resolution = 231.656)

lucC_plot_sequence_events(output.tb, custom_palette = FALSE, show_y_index = FALSE)



