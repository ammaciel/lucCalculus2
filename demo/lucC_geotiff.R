
# # create timeline with classified data from SVM method
# timeline_SVM_mt <- read.csv("~/github_projects/sits.LUC.Calculus/data/timeline_SVM_mt.csv", stringsAsFactors = FALSE, header = FALSE)
# timeline_SVM_mt
#
# # save rda file with timeline
# save(timeline_SVM_mt, file = "~/github_projects/sits.LUC.Calculus/data/timeline_SVM_mt.rda")
#
# select the files for processing
# open stack all classified images, processing performed using Quantum GIS
file <- c("./data/raster_sample_stack_01_16.tif")
file

# define the timeline
data(timeline_SVM_mt)
timeline <- lubridate::as_date(timeline_SVM_mt$V1)
timeline

# library(sits)
# create a raster metadata file based on the information about the files
raster.tb <- sits::sits_coverage(service = "RASTER", product = "MOD13Q1", name = "Sample_region",
                           timeline = timeline, bands = "NULL", files = file)
raster.tb

class(raster.tb$r_obj[[1]])

rb_sits <- raster.tb$r_obj[[1]]
#
# #library(raster)
# # plot each layer separately
# layer <- raster::raster(raster.tb$r_obj[[1]], layer = 2)
# raster::plot(layer)

# raster time series
rt <- rts::rts(raster.tb$r_obj[[1]], timeline)
rt
raster::plot(rt)

# # another example
# # library(rts)
#
#
# library(magrittr)
# # -----------------------------------
#
# timeline_c <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2004-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))
#
# timeline_c
#
# MT_samples <- "~/Desktop/Classi_MT_SVM/raster_sample_mt"
# all_MT_samples <- list.files(MT_samples,
#                              full.names = TRUE,
#                              pattern = ".tif$")
# all_MT_samples
#
# MT_samples_stack <- raster::stack(all_MT_samples)
# MT_samples_stack
#
# # view crs of rasters
# raster::crs(MT_samples_stack)
#
# # view extent of raster in stack
# raster::extent(MT_samples_stack)
#
# # view the y resolution of our rasters
# raster::yres(MT_samples_stack)
#
# # view the x resolution of our rasters
# raster::xres(MT_samples_stack)
#
# # plot
# raster::plot(MT_samples_stack)
#
# # histogram
# raster::hist(MT_samples_stack, xlim = c(0,5))
#
# # create a stack
# rts_image <- rts::rts(MT_samples_stack, timeline_c)
# rts_image
#
# # alias for raster
# rts_image@raster
# rb <- rts_image@raster

raster::plot(rb_sits) #, rb)

# create a copy
# rts_image.r <- rb

# data frame from raster to use LUC Calculus
cellRaster.df <- data.frame( longitude = as.numeric(),
                          latitude = as.numeric(),
                          start_date = as.Date(as.character()),
                          end_date = as.Date(as.character()),
                          label = as.character(),
                          id = as.numeric(),
                          index = as.numeric(),
                          stringsAsFactors = FALSE)
cellRaster.df

# extract x, y, and values raster
cellXYvalues <- raster::rasterToPoints(rb_sits) #rb)
#head(cellXYvalues)

start_date <- as.character(lubridate::ymd(timeline) - lubridate::years(1))
end_date <- as.character(lubridate::ymd(timeline))

# extract values from raster file
for(i in 1:nrow(cellXYvalues)){
  longitude <- as.double(rep(cellXYvalues[i,1], times = length(timeline)))
  latitude <- as.double(rep(cellXYvalues[i,2], times = length(timeline)))
  label <- as.numeric(cellXYvalues[i,c(3:ncol(cellXYvalues))])
  index <- as.numeric(rep(i, times = length(timeline)))

  temp <- cbind(longitude, latitude, label, index)

  cellRaster.df <- rbind(cellRaster.df,temp)
}

 <- cbind(longitude, latitude, start_date, end_date, label, id, index)

cellRaster.df$id <- 1:nrow(cellRaster.df)

head(cellRaster.df)


# visualize plot with data
lucC::lucC_plot_maps_input(cellRaster.df,EPSG_WGS84 = FALSE)









# # scanner all coverage series
# for(i in 1:(raster::ncell(rts_image@raster))){
#   location_time <- as.numeric(raster::extract(rts_image, i))
#   rts_image.r[i] <- as.numeric(eval(parse(text = returned)))
# }




# # extract x, y, and values raster too
vals<-extract(rb,1:ncell(rb))
coord<-xyFromCell(rb,1:ncell(rb))
combine<-cbind(coord,vals)







#-------------------------------
# n3 <- extract(rts_image, 1, "201009/")
# n3

# all periods
ep <- rts::endpoints(rts_image, "years")
ep

# a function:
f <- function(x) {
  #raster::extract(x, "201009/")
   if (min(x) < 2)
     max(x+30)
   else 0

}

rts_image1 <- rts_image %>%
  rts::extract(., 1, "201009/") %>%
  rts::write.rts(., filename = new, overwrite = TRUE)

for (x in length(raster::layer(rts_image@raster))) {
  t <- raster::extract(x, ep , "201009/")
  rs <- t
}

# call parser
returned = try(system("../parser/teste05 < expr.txt", intern = TRUE))



plot(rts_image@raster)

rts_image@raster@history


length(rts_image@raster)

rts_image1 <- period.apply(rts_image, ep, f)
rts_image1

plot(rts_image1)
#plot(rts_image)

#index(rts_image)




library(lucC)

library(raster)

d = array(1:(3*4*7*3),c(3,4,7*3))
b = brick(d)

names(b) = paste("rain",outer(1:7,2001:2003,paste,sep="-"),sep="-")

names(b)

pts = data.frame(x=runif(3),y=runif(3), month=c(5,1,3),year = c(2001,2001,2003))
pts

pts$layername = paste("rain",pts$month,pts$year,sep=".")
pts$layerindex = match(pts$layername, names(b))


lapply(1:nrow(pts), function(i){extract(b, cbind(pts$x[i],pts$y[i]), layer=pts$layerindex[i], nl=1)})

sapply(1:nrow(pts), function(i){extract(b, cbind(pts$x[i],pts$y[i]), layer=pts$layerindex[i], nl=1)})

extract(b, cbind(pts$x, pts$y))[
  cbind(1:nrow(pts),match(pts$layername, names(b)))
  ]


# url: https://stackoverflow.com/questions/34294376/how-to-extract-data-from-a-rasterbrick

ry <- rx <- rc <- rts_image@raster

cellRaster <- data.frame("longitude", "latitude", "start_date", "end_date", "label", "id", "index" )
cellRaster


s <- stack(file())

ry[] <- yFromCell(rts_image@raster, getValues(rts_image@raster))
# rx[] <- xFromCell(rts_image@raster, getValues(rts_image@raster))

extract(MT_samples_stack,SpatialPoints(cbind(0.1,0.2)))





library(raster)
point <- cbind(-6753713, -982753.3)
resul <- extract(rts_image@raster, SpatialPoints(point))
resul

rowColFromCell(MT_samples_stack)

temp0 <- NULL

for(i in 1:(raster::ncell(rts_image@raster))){
  temp <- rowColFromCell(MT_samples_stack, cellFromXY(MT_samples_stack, i))
  temp0 <- rbind(temp0,temp)
}

temp0

cellRaster$longitude <- yFromCell(MT_samples_stack, getValues(MT_samples_stack))
cellRaster$latitude <- xFromCell(rts_image@raster, getValues(rts_image@raster))


# -----------------------------
## An example SpatialBrick
b <- brick(system.file("external/rlogo.grd", package="raster"))
nlayers(b)
# [1] 3
names(b)
# [1] "red"   "green" "blue"

## Extract data from given cells in the "green" layer,
ii <- match("green", names(b))
extract(b, 1000:1003, layer=ii, nl=1)
#      green
# [1,]   254
# [2,]   255
# [3,]   255
# [4,]   255



mat <- extract( rts_image@raster , 1:ncell(rts_image@raster) )
head( mat )







