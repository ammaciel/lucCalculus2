
library(ggplot2)

lucC_plot_bar_events2 <- function(data_mtx = NULL, data_frequency = NULL, custom_palette = FALSE, RGB_color = NULL, pixel_resolution = 250, relabel = FALSE, original_labels = NULL, new_labels = NULL, legend_text = "Legend:", column_legend = 2, side_by_side = FALSE){

  # Ensure if parameters exists
  #ensurer::ensure_that(data_mtx, !is.null(data_mtx),
  #                     err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(custom_palette, !is.null(custom_palette),
                       err_desc = "custom_palette must be defined, if wants use its own color palette setting! Default is FALSE")
  ensurer::ensure_that(pixel_resolution, !is.null(pixel_resolution),
                       err_desc = "pixel_resolution must be defined! Default is 250 meters on basis of MODIS image")

  # input data matrix or a frequency table
  if (!is.null(data_mtx)){
    # to data frame
    #input_data <- reshape2::melt(as.data.frame(data_mtx), id = c("x","y"), na.rm = TRUE)
    input_data <- as.data.frame(data_mtx) %>%
      tidyr::gather(variable, value, -x, -y) %>%
      stats::na.omit()

    input_data <- input_data[!duplicated(input_data), ]
    # count number of values
    mapBar <- data.frame(table(lubridate::year(input_data$variable), input_data$value))
  } else if (!is.null(data_frequency)){
    # already
    mapBar <- data_frequency
    colnames(mapBar) <- c("Var1", "Var2", "Freq")
  } else {
    stop("\nProvide at least a 'data_mtx' or a 'data_frequency' to plot graphics!\n")
  }

  # insert own colors palette
  if(custom_palette == TRUE){
    if(is.null(RGB_color) | length(RGB_color) != length(unique(mapBar$Var2))){
      cat("\nIf custom_palette = TRUE, a RGB_color vector with colors must be defined!")
      cat("\nProvide a list of colors with the same length of the number of legend!\n")
    } else {
      my_palette = RGB_color
    }
  } else {
    # more colors
    colour_count = length(unique(mapBar$Var2))
    my_palette = scales::hue_pal()(colour_count)
  }

  original_leg_lab <- unique(as.character(mapBar$Var2)) # levels(droplevels(mapBar$Var2))
  cat("Original legend labels: \n", original_leg_lab, "\n")

  # insert own legend text
  if(relabel == TRUE){
    if(is.null(original_labels) | length(new_labels) != length(unique(mapBar$Var2)) |
       all(original_labels %in% original_leg_lab) == FALSE){
      cat("\nIf relabel = TRUE, a vector with original labels must be defined!")
      cat("\nProvide a list of original labels and new labels with the same length of the legend!\n")
    } else {
      my_original_label = original_labels
      my_new_labels = new_labels
    }
  } else {
    # my legend text
    my_original_label = unique(mapBar$Var2)
    my_new_labels = unique(mapBar$Var2)
  }

  # make side-by-side bar plot
  if (side_by_side == TRUE){
    bar_position = "dodge"
  } else {
    bar_position = "stack"
  }

  # complete space in bars to have the same width of bars in geom_bar
  mapBar <- tidyr::complete(mapBar, Var1, Var2)
  mapBar$Var1 <- as.factor(mapBar$Var1)
  mapBar$Var2 <- as.factor(mapBar$Var2)

  g <- ggplot2::ggplot(mapBar,aes(x=mapBar$Var1, y=mapBar$Freq, fill=mapBar$Var2)) +
    geom_bar(width = 0.7, stat="identity", position = bar_position) +
    theme_bw()+
    #ylab(expression(paste("Area ",km^{2}," = ((pixels number x pixel ", resolution^{2},")/",1000^{2},")"))) +
    ylab("Hectares")+
    xlab("Time")+
    scale_fill_manual(name= legend_text, values = my_palette, breaks = my_original_label, labels = my_new_labels) + #Legend
    # scale_x_continuous(breaks=unique(mapBar$Var1), labels=unique(mapBar$Var1)) +
    #scale_fill_grey(name = "Legend:", start = 0, end = 0.8) +
    # theme(legend.position = "bottom",
    #       legend.text=element_text(size=11),  ###
    #       legend.key = element_blank())

    guides(fill=guide_legend(ncol = column_legend)) + # number of columns - legend plot

    # annotate("text", x = "2009", y = 1980000, label = "Soy Moratorium") +
    # geom_segment(aes(x = "2008", xend = "2016", y = 1900000, yend = 1900000),
    #              #arrow=arrow(length=unit(0.30,"cm"), angle = 20, ends = "both", type = "closed"), # length=unit(0.30,"cm") linetype="dashed",
    #              linetype="dashed", color = "black") +

    theme(legend.position = "bottom",
          legend.text=element_text(size=11), ### ### era 11
          axis.text.x=element_text(angle=45, hjust=1, size = 11),
          axis.text.y=element_text(size = 11),
          legend.key = element_blank(),
          plot.background = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#b2b2b2", linetype="dashed", size = 0.2),
          panel.grid.major.x = element_blank()) +

    #draws x and y axis line
    theme(axis.line = element_line(color = 'black'))

  print(g)

}

#--------------------------

library(lucCalculus)

options(digits = 12)

# all files in folder
all.the.files <- list.files("~/Desktop/MT_Chronos_SecCerrado/Measures_DLUC/", full=TRUE, pattern = ".csv")
all.the.files

files.list <- lapply(all.the.files, data.table::fread, sep=";")
data <- data.table::rbindlist(files.list, fill = TRUE)
# data SoybeanMT contains Hectare value to Brazil

# replace by hectare value
for (i in 1:nrow(data)){
  if(is.na(data[i,]$Hectare)){
    data[i,]$Hectare <- (data[i,]$Pixel_number*(231.656*231.656))/10000
  }
  else
    data[i,]$Hectare <- data[i,]$Hectare
}
data

data.bar <- data[,c("Years","Classes","Hectare")] # data[,1:3]

lucC_plot_bar_events2(data_frequency = data.bar, custom_palette = FALSE, pixel_resolution = 231.656, side_by_side = TRUE, column_legend = 3)
#lucC_plot_frequency_events(data_frequency = data.bar, custom_palette = FALSE, pixel_resolution = 231.656, column_legend = 3)

# soy moratorium
unique(data.bar$Classes)
#classes <- c("Forest_Pasture", "Forest_Cerrado", "Soy_After_2008_Pasture", "Soy_Before_2008_Pasture", "Soy_After_2008_Cerrado", "Soy_Before_2008_Cerrado") #, "Pasture_Soy") #
#classes <- c("Soy_After_2008_Pasture", "Soy_Before_2008_Pasture", "Soy_After_2008_Cerrado", "Soy_Before_2008_Cerrado", "Mato Grosso (Soja em grao)", "Soy") #, "Pasture_Soy") #
classes1 <- c("Soy_After_2008_Pasture", "Soy_After_2008_Degrad") #, "Pasture_Soy") #
my_data1 <- data.bar[(data.bar$Classes %in% classes1),]
my_data1

result_after <- my_data1 %>%
  dplyr::group_by(., Years) %>%
  dplyr::summarise(., sum(Hectare)) %>%
  dplyr::mutate( Hectare = `sum(Hectare)`) %>%
  dplyr::mutate(Classes = "Soy_After_2008") %>%
  dplyr::select(Years, Classes, Hectare) %>%
  as.data.frame()
result_after

classes1 <- c("Soy_Before_2008_Pasture", "Soy_Before_2008_Degrad") #, "Pasture_Soy") #
my_data2 <- data.bar[(data.bar$Classes %in% classes1),]
my_data2

result_before <- my_data2 %>%
  dplyr::group_by(., Years) %>%
  dplyr::summarise(., sum(Hectare)) %>%
  dplyr::mutate( Hectare = `sum(Hectare)`) %>%
  dplyr::mutate(Classes = "Soy_Before_2008") %>%
  dplyr::select(Years, Classes, Hectare) %>%
  as.data.frame()
result_before

unique(data.bar$Classes)
#-------------
#classes <- c("Forest_Pasture", "Forest_Cerrado", "Soy_After_2008_Pasture", "Soy_Before_2008_Pasture", "Soy_After_2008_Cerrado", "Soy_Before_2008_Cerrado") #, "Pasture_Soy") #
classes <- c("Soy_After_2008", "Soy_Before_2008", "Forest_Degradation", "Forest_Pasture") #
my_data <- data.bar[(data.bar$Classes %in% classes),]
my_data

my_data <- rbind(my_data, result_after, result_before)
my_data

my <- my_data[!Years %in% c(2000, 2001, 2002, 2003, 2004, 2005)]
#-------------
classes2 <- c("Soy_Before_2008") # Forest
my_data2 <- my_data[(my_data$Classes %in% classes2),]
my_data2


# figure bar plot
#png(filename = "~/Desktop/fig_TESE/fig_MT_bar_soy331.png", width = 8.5, height = 5.4, units = 'in', res = 300) #"#1a9641", "#9cecc4", "#c7011a", "#000d92", "#f6546a", "#4169e1"
lucC_plot_bar_events2(data_frequency = my, custom_palette = TRUE, RGB_color = c("#005900", "#66b266", "#CC2D25", "#968a8e"), pixel_resolution = 231.656, side_by_side = TRUE, relabel = TRUE, original_labels = c("Forest_Degradation", "Forest_Pasture", "Soy_After_2008", "Soy_Before_2008"), new_labels = c("Forest to Degradation", "Forest to Pasture", "Pasture and Degradation to Soy, deforested after 2008", "Pasture and Degradation to Soy, deforested before 2008"), legend_text = "Land use transitions: ", column_legend = 2)
# "#968a8e", "#ff7f44", "#CC2D25", "#47258B"
# "#78D283", "#00510A", "#CC2D25", "#47258B"
#dev.off()
#"#1a9641", "#000080", "#d7191c", "#daa520", "magenta", "black"
# original_labels = c("Soy", "Mato Grosso (Soja em grao)", "Soy_After_2008", "Soy_Before_2008" ), new_labels = c("Soybean", "Mato Grosso (soy bean)", "Pasture and Degradation to Soy, deforested after 2008", "Pasture and Degradation to Soy, deforested before 2008")


lucC_plot_frequency_events(data_frequency = my, custom_palette = FALSE, pixel_resolution = 231.656)

# land use transitions
unique(data.bar$Classes)
classes <- c("Cerrado_Pasture", "Cerrado_Secondary_Vegetation", "Cerrado_Soy", "Forest_Cerrado", "Forest_Pasture", "Forest_Soy", "Pasture_Cerrado", "Pasture_Secondary_Vegetation", "Pasture_Soy", "Secondary_Vegetation_Cerrado", "Secondary_Vegetation_Pasture", "Secondary_Vegetation_Soy", "Soy_Cerrado", "Soy_Pasture", "Soy_Secondary_Vegetation") #
my_data2 <- data.bar[(data.bar$Classes %in% classes),]
my_data2

lucC_plot_bar_events(data_frequency = my_data2, custom_palette = FALSE, pixel_resolution = 231.656, side_by_side = TRUE, column_legend = 5)
lucC_plot_frequency_events(data_frequency = my_data2, custom_palette = FALSE, pixel_resolution = 231.656)


# forest x secondary vegetation
unique(data.bar$Classes)
classes <- c("Forest", "Secondary_Vegetation") #
my_data2 <- data.bar[(data.bar$Classes %in% classes),]
my_data2

#png(filename = "~/Desktop/fig_TESE/fig_MT_for_sv.png", width = 7.0, height = 4.2, units = 'in', res = 300)
lucC_plot_bar_events(data_frequency = my_data2, custom_palette = TRUE, RGB_color = c("#228b22", "#7ecfa4"), pixel_resolution = 231.656, legend_text = "Legend:", side_by_side = TRUE, relabel = TRUE, original_labels = c("Forest", "Secondary_Vegetation"), new_labels = c("Forest", "Secondary Vegetation")) # c("#7ecfa4", "#228b22") c("gray60", "black")
#dev.off()

lucC_plot_frequency_events(data_frequency = my_data2, custom_palette = FALSE, pixel_resolution = 231.656)


#---------------------
library(ggplot2)

dfTab <- as.data.frame((my_data))
colnames(dfTab)[1] <- "x"
dfTab$lab <- as.character(100 * dfTab$Freq / sum(dfTab$Freq))

ggplot(df) + geom_bar(aes(x,fill=x)) +
  geom_text(data=dfTab,aes(x=x,y=Freq,label=lab),vjust=0) +
  theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),legend.title=element_blank(),
        axis.title.y=element_blank())

dfTab$lab <- paste(dfTab$Freq,paste("(",dfTab$lab,"%)",sep=""),sep=" ")


# growth rate
#--------------------

data.bar[data.bar$Classes == "Soy",]
GR_class <- "Soy"
# soy
GR <- (((data.bar[data.bar$Classes == GR_class & data.bar$Years == 2016, "Hectare"]) -
          (data.bar[data.bar$Classes == GR_class & data.bar$Years == 2001, "Hectare"])) /
         data.bar[data.bar$Classes == GR_class & data.bar$Years == 2001, "Hectare"]) * 100
GR

#------------- GAGR
df <- my_data
GR_class <- "Soy_Before_2008"
df[df$Classes == GR_class,]

GAGR <- (((df[df$Classes == GR_class & df$Years == 2007, "Hectare"])/
            (df[df$Classes == GR_class & df$Years == 2003, "Hectare"])) ^ (1/(2007-2003)) -1) * 100
GAGR

#------------- GR
data.bar[data.bar$Classes == "Soy",]
GR_class <- "Soy"
# soy
GR <- (((df[df$Classes == GR_class & df$Years == 2016, "Hectare"]) -
          (df[df$Classes == GR_class & df$Years == 2001, "Hectare"])) /
         df[df$Classes == GR_class & df$Years == 2001, "Hectare"]) * 100
GR


