
raster_obj = rb_sits
data_mtx = forest_sec       # without 2001
timeline = timeline
label = label
path_raster_folder = "~/Desktop/Ne4"


#-------------------- prepare rasterBrick --------------------------------
# original raster
df <- raster::rasterToPoints(raster_obj) %>%
  data.frame()

# replace colnames to timeline
colnames(df)[c(3:ncol(df))] <- as.character(lubridate::year(timeline))
#raster_df <- reshape2::melt(df, id.vars = c("x","y"))
raster_df <- df %>%
  tidyr::gather(variable, value, -x, -y)

#rm(df)
#gc()

# remove factor
#raster_df$variable = as.character(levels(raster_df$variable))[raster_df$variable]
raster_df$value = 0

#-------------------- prepare matrix with events --------------------------------
# replace new clase by new pixel value
class_name <- unique(data_mtx[3:ncol(data_mtx)][!duplicated(as.vector(data_mtx[3:ncol(data_mtx)])) & !is.na(data_mtx[3:ncol(data_mtx)])] )

class <- which(label %in% class_name)
#data_mtx$x = as.numeric(as.character(data_mtx$x))
#data_mtx$y = as.numeric(as.character(data_mtx$y))
#data_mtx[3:ncol(data_mtx)] = as.character(as.character(data_mtx[3:ncol(data_mtx)]))

for( i in 1:length(class_name)){
  #data_mtx <- replace(data_mtx, data_mtx == class_name[i], class[i])
  #data_mtx[c(3:ncol(data_mtx))] <- replace(data_mtx[c(3:ncol(data_mtx))], data_mtx[c(3:ncol(data_mtx))] == class_name[i], class[i])
  data_mtx[c(3:ncol(data_mtx))] <- ifelse(data_mtx[c(3:ncol(data_mtx))] == class_name[i], class[i], NA)
}

# data matrix to new raster
new_df <- as.data.frame(data_mtx)
colnames(new_df)[c(3:ncol(new_df))] <- as.character(lubridate::year(colnames(new_df)[c(3:ncol(new_df))]))

#rm(data_mtx)
#gc()

# point_df <- reshape2::melt(new_df, id.vars = c("x","y")) %>%
#   stats::na.omit()
point_df <- new_df %>%
  tidyr::gather(variable, value, -x, -y) %>%
  stats::na.omit()

# remove factors
# point_df$x = as.numeric(as.character(point_df$x)) # as.numeric(levels(point_df$x))[point_df$x]
# point_df$y = as.numeric(as.character(point_df$y))
# point_df$variable = as.character(as.character(point_df$variable))

#rm(new_df)
#gc()
# ------------------ replace point_df in raster_df ---------------------

# replace in entire raster
raster_df_temp <- base::merge(raster_df, point_df, by = c("x","y","variable"), all = TRUE) %>%
  dplyr::mutate(value = ifelse(!is.na(.$value.y), .$value.y, .$value.x)) %>%
  dplyr::select(-value.x, -value.y) %>%
  .[order(.$variable),]

raster_df_temp


# # replace in entire raster
# raster_df_temp <- base::merge(raster_df, point_df, by = c("x","y","variable"), all = TRUE) %>%
#   dplyr::mutate(value = ifelse(!is.na(.$value.y), .$value.y, .$value.x)) %>%
#   dplyr::select(-value.x, -value.y) %>%
#   .[order(.$variable),]

#rm(raster_df, point_df)
#gc()

# remove duplicated lines
raster_df_temp <- raster_df_temp[!duplicated(raster_df_temp), ]

#raster_df_temp[is.na(raster_df_temp)] <- 0

#raster_df_update <- reshape2::dcast(raster_df_temp, x+y ~ variable, value.var= "value")
raster_df_update <- raster_df_temp %>%
  tidyr::spread(variable, value)

colnames(raster_df_update)[c(3:ncol(raster_df_update))] <- as.character(timeline)

lucC_save_GeoTIFF(raster_obj = raster_obj, data_mtx = raster_df_update, path_raster_folder = path_raster_folder, as_RasterBrick = FALSE)

rm(raster_obj, raster_df_temp)
gc()

return(raster_df_update)



.

a1 <- c(1, 2, 3)
a2 <- c(3, 4, 5)
a3 <- c(a1, a2)

a1 <- as.data.table(forest_recur)
a2 <- as.data.table(forest_evolve)
a3 <- c(a1, a2)
a3



a3$x



data.tab


.
x <- as.matrix(100)
