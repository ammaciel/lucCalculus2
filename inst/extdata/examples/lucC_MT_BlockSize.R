library(lucCalculus)

# start time

#----------------------------
# 1- Open idividual images and create a RasterBrick with each one and metadata ith SITS
#----------------------------

# create a RasterBrick from individual raster saved previously
# lucC_create_RasterBrick(path_open_GeoTIFFs = "~/Desktop/INPE_2018/Classi_MT_SVM/raster_mt_by_year_2004", path_save_RasterBrick = "~/Desktop")

# ------------- define variables to use in sits -------------
# open files
file <- c("~/Desktop/INPE_2018/Classi_MT_SVM/raster_mt_by_year_2004.tif")

# create timeline with classified data from SVM method
timeline <- lubridate::as_date(c("2001-09-01", "2002-09-01", "2003-09-01", "2005-09-01", "2006-09-01", "2007-09-01", "2008-09-01", "2009-09-01", "2010-09-01", "2011-09-01", "2012-09-01", "2013-09-01", "2014-09-01", "2015-09-01", "2016-09-01"))

file_name <- basename(tools::file_path_sans_ext(file))

#library(sits)
# create a RasterBrick metadata file based on the information about the files
raster.tb <- sits::sits_coverage(service = "RASTER", files = file, name = file_name, timeline = timeline, bands = "ndvi")

# new variable
rb_sits <- raster.tb$r_objs[[1]][[1]]

# original label - see QML file, same order
label <- as.character(c("Cerrado", "Fallow_Cotton", "Forest", "Pasture", "Soy_Corn", "Soy_Cotton", "Soy_Fallow", "Soy_Millet", "Soy_Sunflower", "Sugarcane", "Urban_Area", "Water"))

# original colors set - see QML file, same order
colors_1 <- c("#b3cc33", "#8ddbec", "#228b22", "#afe3c8", "#b6a896", "#e1cdb6", "#e5c6a0", "#b69872", "#b68549", "#dec000", "#cc18b4", "#0000f1" )

#-------------
plot(rb_sits)

# # alter attributes using labels
rb_sits@data@attributes <- lapply(rb_sits@data@attributes, function(x)  {x <- data.frame(ID = c(1:length(label)), category = label)} )
rasterVis::levelplot(rb_sits, col.regions=colors_1) # par.settings=rasterVis::RdBuTheme


