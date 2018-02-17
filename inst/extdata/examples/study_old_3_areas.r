#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with queries in a study area MT_newClass         ##
##                                                             ##  
##                                             2016-09-27      ##
##                                                             ##
#################################################################


# install packages
packages <- c("dplyr","lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
}
remove(packages)

#************************
# 1. Load scripts "events_AllensIntervals.r" and "events_Occurs&Holds.r"
#************************

getwd()
#setwd("~/Dropbox/AA-Doutorado/PROJETO ATUAL/Eventos/")
setwd("~/Desktop/ESTUDO_TESE/Eventos - Alterar/")
getwd()

source("scripts_events/events_AllensIntervals.r")
source("scripts_events/events_Occurs&Holds.r")
source("scripts_events/events_Figures.r")


#************************
# 2. Data preparation
#************************

# open file
#input = "Eventos_02_MT"
#dfTS = read.csv(sprintf("%s/area3.csv",input), sep=",")

##########################
dfTS = read.csv("~/Desktop/ESTUDO_TESE/Studies/Area_Sinop/classification_Area_1_Sinop.csv", sep=",", stringsAsFactors = FALSE)
head(dfTS)

df_input <- dfTS[colnames(dfTS) != "twdtw_dist"]
head(df_input)

# create new columns with an uniform date
df_input$"from" <- format(as.Date(df_input$from, origin="1970-01-01"), format = '%Y-08-15')
df_input$"to" <- format(as.Date(df_input$to, origin="1970-01-01"), format = '%Y-08-15')
head(df_input)

remove(dfTS)
##########################

head(df)

#df <- dfTS[dfTS$time_id==1,]
#df <- dfTS

#df$label_id = df$label
# change label for legends
for (x in 1:nrow(df)){
    if (df$label_id[x] == 1) {
      df$"label"[x] = "Cotton-Fallow"
    }
    else if (df$label_id[x] == 2) {
      df$"label"[x] = "Forest"
    }
    else if (df$label_id[x] == 3) {
      df$"label"[x] = "Low-vegetation"
    }
    else if (df$label_id[x] == 4) {
      df$"label"[x] = "Pasture"
    }
    else if (df$label_id[x] == 5) {
      df$"label"[x] = "Soybean-Cotton"
    }
    else if (df$label_id[x] == 6) {
      df$"label"[x] = "Soybean-Fallow"
    }
    else if (df$label_id[x] == 7) {
      df$"label"[x] = "Soybean-Maize"
    }
    else if (df$label_id[x] == 8) {
      df$"label"[x] = "Soybean-Millet"
    }
    else if (df$label_id[x] == 9) {
      df$"label"[x] = "Soybean-Sunflower"
    }
    else if (df$label_id[x] == 10) {
      df$"label"[x] = "Water"
    }
    else if (df$label_id[x] == 11) {
      df$"label"[x] = "Wetland"
    }
}

# create new columns with original date
#df$"from0" <- format(as.Date(df$from, origin="1970-01-01"), format = '%Y-%m-%d')
#df$"to0" <- format(as.Date(df$to, origin="1970-01-01"), format = '%Y-%m-%d')

# create new columns with an uniform date
df$"from2" <- format(as.Date(df$from, origin="1970-01-01"), format = '%Y-08-15')
df$"to2" <- format(as.Date(df$to, origin="1970-01-01"), format = '%Y-08-15')

# create list sorted by longitude and latitude values
df$"ID" <- as.integer(interaction(df$col_id, df$row_id)) 

# create index
df$"index"<-as.numeric(rownames(df))
head(df)

# rename columns
colnames(df) <- c('longitude', 'latitude', 'time_id', 'from', 'to', 'label_id', 'twdtw_dist', 'label', 'from2', 'to2', 'ID', 'index')
head(df)

# stringsAsFactors = FALSE always put this
df_input <- data.frame("longitude"=df$longitude,
                    "latitude"=df$latitude,
                    "label"=df$label,
                    "from"=df$from2,
                    "to"=df$to2,
                    "ID"=df$ID, 
                    "index"=df$index, stringsAsFactors = FALSE)  
head(df_input)
remove(df,dfTS,x,input)

# save in file.csv
# write.table(df_input, "path/name_file.csv",quote = FALSE,sep = ",", row.names = FALSE)


#************************
# 3. Questions
#************************

#+++++++++++++++++++++++++++++
## Question 0 - holds(o,p,t)
## Which "Forest" areas are TRUE in interval from 2000-08-01 to 2001-08-31
# o = geo-objects, the own df_input data.frame
# p = properties of objects :
p1 <- "Pasture"
# t = interval:
t1 <- interval(ymd("2003-08-01"),ymd("2008-08-31"))

# Test holds for many time series in a dataframe
QuestionHolds <- function(df, p, t){
  # df <- df_input 
  # p <- p1
  # t <- t1
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
  
    if (nrow(event1 <- holds(temp, p, t)) >=1){
      temp0 <- event1
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
  #return(assign("df_output",df_output,envir = .GlobalEnv))
}

df_outputHolds <- QuestionHolds(df_input, p1, t1)
remove(t1)
remove(p1)

plotSequenceEvents(df_outputHolds, dateStart="2000-01-01", dateEnd="2016-12-31") 


##########################################
# Questions - occur(o,p,t)
##########################################

#+++++++++++++++++++++++++++++
## Question 1 - Which "Forest" areas have been replaced by "Pasture" and then turned into "Soybean-Millet"?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"
p3 <- "Soybean-Millet"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2001-08-15"))
t2 <- interval(ymd("2001-08-15"),ymd("2002-08-15"))
t3 <- interval(ymd("2002-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  # df <- df_input 
  # p <- c(p1,p2,p3)
  # t <- c(t1,t2,t3)
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if (nrow(event1 <- occur(temp, p1, t1)) >= 1 &
        nrow(event2 <- occur(temp, p2, t2)) >= 1 &
        isTRUE(meets(interval(head(event1$from,1),tail(event1$to,1)),
                     interval(head(event2$from,1),tail(event2$to,1)))) &
        nrow(event3 <- occur(temp, p3, t3)) >= 1 &
        isTRUE(nextInt(interval(head(event2$from,1),tail(event2$to,1)),
                       interval(head(event3$from,1),tail(event3$to,1))))
        # (
        #   isTRUE(meets(interval(head(event2$from,1),tail(event2$to,1)),
        #                interval(head(event3$from,1),tail(event3$to,1)))) ||
        #   isTRUE(before(interval(head(event2$from,1),tail(event2$to,1)),
        #                 interval(head(event3$from,1),tail(event3$to,1))))
        # )
    ){
      temp0 <- rbind(event1,event2,event3)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
  #return(assign("df_output",df_output,envir = .GlobalEnv))
}

df_outputOccurs1 <- QuestionOccurs(df_input, p = c(p1,p2,p3), t = c(t1,t2,t3))
remove(t1,t2,t3)
remove(p1,p2,p3)

plotMapsInput(df_input, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs1, LongLat = FALSE, sizeSquare = 1.4) 
df_outputO1 <- data.e
plotBarplotEvents(df_outputO1)
plotSequenceEvents(df_outputOccurs1, dateStart="2000-01-01", dateEnd="2016-12-31") 

# verify for duplicated rows
#length(which(duplicated(df_outputOccurs1)))


#+++++++++++++++++++++++++++++
## Question 2 - Which "Forest" areas have been replaced by "Soybean-..."??
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Forest"
p2.1 <- "Soybean-Cotton"
p2.2 <- "Soybean-Fallow"
p2.3 <- "Soybean-Maize"
p2.4 <- "Soybean-Millet"
p2.5 <- "Soybean-Sunflower"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2006-08-15"))
t2 <- interval(ymd("2006-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if (nrow(event1 <- occur(temp, p1, t1)) == 6 &
        (
          nrow(event2.1 <- occur(temp, p2.1, t2)) >= 1 |
          nrow(event2.2 <- occur(temp, p2.2, t2)) >= 1 |
          nrow(event2.3 <- occur(temp, p2.3, t2)) >= 1 |
          nrow(event2.4 <- occur(temp, p2.4, t2)) >= 1 |
          nrow(event2.5 <- occur(temp, p2.5, t2)) >= 1
        )
        &
        (
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.1$from,1),tail(event2.1$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.2$from,1),tail(event2.2$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.3$from,1),tail(event2.3$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.4$from,1),tail(event2.4$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.5$from,1),tail(event2.5$to,1))))
        )
  
    ){
      temp0 <- rbind(event1,event2.1,event2.2,event2.3,event2.4,event2.5)
    } else {
    temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs2 <- QuestionOccurs(df_input, p = c(p1,p2.1,p2.2,p2.3,p2.4,p2.5), t = c(t1,t2))
remove(t1,t2)
remove(p1,p2.1,p2.2,p2.3,p2.4,p2.5)

plotMapsInput(df_input, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs2, LongLat = FALSE, sizeSquare = 1.4) 
df_outputO2 <- data.e
plotBarplotEvents(df_outputO2)
plotSequenceEvents(df_outputOccurs2, dateStart="2000-01-01", dateEnd="2016-12-31")  


#+++++++++++++++++++++++++++++ Area 1 - SBSR
## Question 3 - Which "Pasture" areas before have been replaced by soybean-millet in 2008?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Pasture"
p2 <- "Soybean-Millet"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2007-08-15"))
t2 <- interval(ymd("2007-08-15"),ymd("2008-08-15"))
t3 <- interval(ymd("2008-08-15"),ymd("2015-08-15"))


# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if (nrow(event1 <- occur(temp, p1, t1)) >= 1 &
        nrow(event2 <- occur(temp, p2, t2)) >= 1 &
        nrow(event3 <- occur(temp, p2, t3)) >= 1 &
        
        isTRUE(precedingInt(interval(head(event2$from,1),tail(event2$to,1)),
                            interval(head(event1$from,1),tail(event1$to,1))))   &
        isTRUE(nextInt(interval(head(event2$from,1),tail(event2$to,1)),
                       interval(head(event3$from,1),tail(event3$to,1))))
    ){
      temp0 <- rbind(event1, event2, event3)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs3 <- QuestionOccurs(df_input_new, p = c(p1,p2), t = c(t1,t2,t3))
remove(t1,t2,t3)
remove(p1,p2)

# remove duplicated rows
length(which(duplicated(df_outputOccurs3)))
#df_outputOccurs3 <- df_outputOccurs3[!duplicated(df_outputOccurs3),]

plotMapsInput(df_input_new, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs3, LongLat = FALSE, sizeSquare = 1.4) 
df_outputO3 <- data.e
plotBarplotEvents(df_outputO3)
plotSequenceEvents(df_outputOccurs3, dateStart="2000-01-01", dateEnd="2016-12-31")    
  
  
#+++++++++++++++++++++++++++++
## Question 4 - Which "Pasture" areas have been replaced by "Soybean-Millet" at any time?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Cotton-Fallow"
p2 <- "Soybean-Fallow"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if (nrow(event1 <- occur(temp, p1, t1)) >= 1 &
        nrow(event2 <- occur(temp, p2, t1)) >= 1 #&
        #isTRUE(contains(interval(head(event1$from,1),tail(event1$to,1)),
         #             interval(head(event2$from,1),tail(event2$to,1))))
    ){
      temp0 <- rbind(event1,event2)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs4 <- QuestionOccurs(df_input, p = c(p1,p2), t = t1)
remove(t1)
remove(p1,p2)

plotMapsInput(df_input, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs4, LongLat = FALSE, sizeSquare = 1.4) 
df_outputO4 <- data.e
plotBarplotEvents(df_outputO4)
plotSequenceEvents(df_outputOccurs4, dateStart="2000-01-01", dateEnd="2016-12-31")    


#+++++++++++++++++++++++++++++ IJGIS - area 1
## Question 5 - Which "Pasture" areas have been replaced by "Cropping" at any time?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Pasture"
p2.1 <- "Soybean-Cotton"
p2.2 <- "Soybean-Fallow"
p2.3 <- "Soybean-Maize"
p2.4 <- "Soybean-Millet"
p2.5 <- "Soybean-Sunflower"
p2.6 <- "Cotton-Fallow"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2001-08-15"))
t2 <- interval(ymd("2001-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if (nrow(event1 <- occur(temp, p1, t1)) >= 1 &
        (
          nrow(event2.1 <- occur(temp, p2.1, t2)) >= 1 |
          nrow(event2.2 <- occur(temp, p2.2, t2)) >= 1 |
          nrow(event2.3 <- occur(temp, p2.3, t2)) >= 1 |
          nrow(event2.4 <- occur(temp, p2.4, t2)) >= 1 |
          nrow(event2.5 <- occur(temp, p2.5, t2)) >= 1 |
          nrow(event2.6 <- occur(temp, p2.6, t2)) >= 1
        )
        &
        (
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.1$from,1),tail(event2.1$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.2$from,1),tail(event2.2$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.3$from,1),tail(event2.3$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.4$from,1),tail(event2.4$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.5$from,1),tail(event2.5$to,1)))) |
          isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                         interval(head(event2.6$from,1),tail(event2.6$to,1))))
        )
        
    ){
      temp0 <- rbind(event1,event2.1,event2.2,event2.3,event2.4,event2.5,event2.6)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs5 <- QuestionOccurs(df_input_new, p = c(p1,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6), t = c(t1,t2))
remove(t1,t2)
remove(p1,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6)

## remove duplicated rows
length(which(duplicated(df_outputOccurs5)))
#df_outputOccurs5 <- df_outputOccurs5[!duplicated(df_outputOccurs5),]

plotMapsInput(df_input_new, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs5, LongLat = FALSE, sizeSquare = 1) 
df_outputO5 <- data.e
plotBarplotEvents(df_outputO5)
plotSequenceEvents(df_outputOccurs5, dateStart="2000-01-01", dateEnd="2016-12-31")    















#---------------------------------------------

#+++++++++++++++++++++++++++++ 
# AREA "2" -- SBSR
## Question 6 - Which "Forest" areas have been replaced by Pasture and low-vegetation?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"
p2.1 <- "Low-vegetation"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2001-08-15"))
t2 <- interval(ymd("2001-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if( nrow(event1 <- occur(temp, p1, t1)) >= 1 &
      (
        nrow(event2 <- occur(temp, p2, t2)) >= 1 &
        nrow(event2.1 <- occur(temp, p2.1, t2)) >= 1
      )
         & 
      (
        isTRUE(nextInt(interval(head(event1$from,1),head(event1$to,1)),
                     interval(head(event2$from,1),head(event2$to,1)))) &
        isTRUE(nextInt(interval(head(event1$from,1),head(event1$to,1)),
                     interval(head(event2.1$from,1),head(event2.1$to,1)))) 
      )
      ){
      temp0 <- rbind(event1,event2,event2.1)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs6 <- QuestionOccurs(df_input_new, p = c(p1,p2,p2.1), t = c(t1,t2))
remove(t1,t2)
remove(p1,p2,p2.1)

# remove duplicated rows
length(which(duplicated(df_outputOccurs6)))
#df_outputOccurs6 <- df_outputOccurs6[!duplicated(df_outputOccurs6),]

plotMapsInput(df_input_new, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs6, LongLat = FALSE, sizeSquare = 1.4) 
df_outputO6 <- data.e
plotBarplotEvents(df_outputO6)
plotSequenceEvents(df_outputOccurs6, dateStart="2000-01-01", dateEnd="2016-12-31")    


#+++++++++++++++++++++++++++++
# Areas 1 and 2, 2 is better
## Question 7 - Which "Forest" areas have been replaced by other croppings and return in Forest?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :

p1 <- "Forest" 

p1.1 <- "Low-vegetation" 
p1.2 <- "Pasture"
p1.3 <- "Wetland" 

p2.1 <- "Soybean-Cotton" 
p2.2 <- "Soybean-Fallow" 
p2.3 <- "Soybean-Maize" 
p2.4 <- "Soybean-Millet" 
p2.5 <- "Soybean-Sunflower" 
p2.6 <- "Cotton-Fallow" 

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2001-08-15"))
t2 <- interval(ymd("2001-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if (nrow(event1 <- occur(temp, p1, t1)) >= 1 &
        (
          nrow(event1.1 <- occur(temp, p1.1, t2)) >= 1 |
          nrow(event1.2 <- occur(temp, p1.2, t2)) >= 1 |
          nrow(event1.3 <- occur(temp, p1.3, t2)) >= 1 |
          
          nrow(event2.1 <- occur(temp, p2.1, t2)) >= 1 |
          nrow(event2.2 <- occur(temp, p2.2, t2)) >= 1 |
          nrow(event2.3 <- occur(temp, p2.3, t2)) >= 1 |
          nrow(event2.4 <- occur(temp, p2.4, t2)) >= 1 |
          nrow(event2.5 <- occur(temp, p2.5, t2)) >= 1 |
          nrow(event2.6 <- occur(temp, p2.6, t2)) >= 1
        ) 
        &
          nrow(event3 <- occur(temp, p1, t2)) >= 1 
        &
          ((
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event1.1$from,1),tail(event1.1$to,1)))) &
            isTRUE(nextInt(interval(head(event1.1$from,1),tail(event1.1$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event1.2$from,1),tail(event1.2$to,1)))) &
            isTRUE(nextInt(interval(head(event1.2$from,1),tail(event1.2$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event1.3$from,1),tail(event1.3$to,1)))) &
            isTRUE(nextInt(interval(head(event1.3$from,1),tail(event1.3$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event2.1$from,1),tail(event2.1$to,1)))) &
            isTRUE(nextInt(interval(head(event2.1$from,1),tail(event2.1$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event2.2$from,1),tail(event2.2$to,1)))) &
            isTRUE(nextInt(interval(head(event2.2$from,1),tail(event2.2$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event2.3$from,1),tail(event2.3$to,1)))) &
            isTRUE(nextInt(interval(head(event2.3$from,1),tail(event2.3$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event2.4$from,1),tail(event2.4$to,1)))) &
            isTRUE(nextInt(interval(head(event2.4$from,1),tail(event2.4$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event2.5$from,1),tail(event2.5$to,1)))) &
            isTRUE(nextInt(interval(head(event2.5$from,1),tail(event2.5$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          )|(
            isTRUE(nextInt(interval(head(event1$from,1),tail(event1$to,1)),
                           interval(head(event2.6$from,1),tail(event2.6$to,1)))) &
            isTRUE(nextInt(interval(head(event2.6$from,1),tail(event2.6$to,1)),
                           interval(head(event3$from,1),tail(event3$to,1)))) 
          ))
    ){
      temp0 <- rbind(event1,event1.1,event1.2,event1.3,event2.1,event2.2,event2.3,event2.4,event2.5,event2.6,event3)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs7 <- QuestionOccurs(df_input, p = c(p1,p1.1,p1.2,p1.3,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6,p3), t = c(t1,t2))
remove(t1,t2)
remove(p1,p1.1,p1.2,p1.3,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6,p3)

# remove duplicated rows
length(duplicated(df_outputOccurs7))
#df_outputOccurs7 <- df_outputOccurs7[!duplicated(df_outputOccurs7),]

plotMapsInput(df_input, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs7, LongLat = FALSE, sizeSquare = 1.4) 
df_outputO7 <- data.e
plotBarplotEvents(df_outputO7)
plotSequenceEvents(df_outputOccurs7, dateStart="2000-01-01", dateEnd="2016-12-31")  



#+++++++++++++++++++++++++++++ SBSR - area 3
## Question 8 - Which "Forest" areas haven't been replaced by other croppings?
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Forest"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if (nrow(event2 <- occur(temp, p1, t1)) >= 1
       
    ){
      temp0 <- rbind(event2)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs8 <- QuestionOccurs(df_input_new, p = p1, t = t1)
remove(t1)
remove(p1)

# remove duplicated rows
length(which(duplicated(df_outputOccurs8)))
#df_outputOccurs8 <- df_outputOccurs8[!duplicated(df_outputOccurs8),]

plotMapsInput(df_input_new, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs8, LongLat = FALSE, sizeSquare = 1.4) 
df_outputO8 <- data.e
plotBarplotEvents(df_outputO8)
plotSequenceEvents(df_outputOccurs8, dateStart="2000-01-01", dateEnd="2016-12-31")    




#+++++++++++++++++++++++++++++ 
# AREA "2" - IJGIS 
## Question 9 - Which "Forest" areas have been replaced by Pasture and low-vegetation and return to "Forest" area? -> "Secondary-vegetation"
# o = geo-objects, the own df_input data.frame

# p = properties of objects :
p1 <- "Forest"
p2 <- "Pasture"
#p2.1 <- "Low-vegetation"
p3 <- "Secondary-vegetation"

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2001-08-15"))
t2 <- interval(ymd("2001-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    #x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if( nrow(event1 <- occur(temp, p1, t1)) >= 1 &
        (
          nrow(event2 <- occur(temp, p2, t2)) >= 1 #|
 #         nrow(event2.1 <- occur(temp, p2.1, t2)) >= 1
        )
        &
        nrow(event3 <- occur(temp, p3, t2)) >= 1 
        & 
        #(
          (
          isTRUE(nextInt(interval(head(event1$from,1),head(event1$to,1)),
                         interval(head(event2$from,1),head(event2$to,1)))) &
          isTRUE(nextInt(interval(head(event2$from,1),head(event2$to,1)),
                         interval(head(event3$from,1),head(event3$to,1)))) 
        )
#         |
#         (
#           isTRUE(nextInt(interval(head(event1$from,1),head(event1$to,1)),
#                          interval(head(event2.1$from,1),head(event2.1$to,1)))) &
#           isTRUE(nextInt(interval(head(event2.1$from,1),head(event2.1$to,1)),
#                          interval(head(event3$from,1),head(event3$to,1)))) 
#         ))
    ){
      temp0 <- rbind(event1,event2,event3) #,event2.1
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs9 <- QuestionOccurs(df_input_new, p = c(p1,p2,p3), t = c(t1,t2)) # ,p2.1
remove(t1,t2)
remove(p1,p2,p3) #,p2.1

# remove duplicated rows
length(which(duplicated(df_outputOccurs9)))
#df_outputOccurs9 <- df_outputOccurs9[!duplicated(df_outputOccurs9),]

plotMapsInput(df_input_new, LongLat = TRUE) 
plotMapsOutputEvents(df_outputOccurs9, LongLat = TRUE, sizeSquare = 1.4) 
df_outputO9 <- data.e
plotBarplotEvents(df_outputO9)
plotSequenceEvents(df_outputOccurs9, dateStart="2000-01-01", dateEnd="2016-12-31")    

remove(list=grep(x= ls(pos=1), pattern="pts_", value=TRUE), data.e, data.m)
remove(list=grep(x= ls(pos=1), pattern="ptsE_", value=TRUE))



#+++++++++++++++++++++++++++++ 
# AREA "3" - IJGIS 
## Question 10 - Which "Forest" areas have been turned in secondary-vegetation after anyclass? 
# o = geo-objects, the own df_input data.frame

# p = properties of objects 
p1 <- "Forest" 

#p1.1 <- "Low-vegetation" 
p1.2 <- "Pasture"
#p1.3 <- "Wetland" 

p2.1 <- "Soybean-Cotton" 
p2.2 <- "Soybean-Fallow" 
p2.3 <- "Soybean-Maize" 
p2.4 <- "Soybean-Millet" 
p2.5 <- "Soybean-Sunflower" 
p2.6 <- "Cotton-Fallow" 

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2001-08-15"))
t2 <- interval(ymd("2001-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  #df <- df_input
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    # x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if ((nrow(event1 <- occur(temp, p1, t1)) >= 1 |
         # nrow(event1.01 <- occur(temp, p1.1, t1)) >= 1 |
          nrow(event1.02 <- occur(temp, p1.2, t1)) >= 1 |
          #nrow(event1.03 <- occur(temp, p1.3, t1)) >= 1 |
          
          nrow(event1.04 <- occur(temp, p2.1, t1)) >= 1 |
          nrow(event1.05 <- occur(temp, p2.2, t1)) >= 1 |
          nrow(event1.06 <- occur(temp, p2.3, t1)) >= 1 |
          nrow(event1.07 <- occur(temp, p2.4, t1)) >= 1 |
          nrow(event1.08 <- occur(temp, p2.5, t1)) >= 1 |
          nrow(event1.09 <- occur(temp, p2.6, t1)) >= 1
        ) 
        &
        (
        #  nrow(event1.1 <- occur(temp, p1.1, t2)) >= 1 |
          nrow(event1.2 <- occur(temp, p1.2, t2)) >= 1 |
         # nrow(event1.3 <- occur(temp, p1.3, t2)) >= 1 |
          
          nrow(event2.1 <- occur(temp, p2.1, t2)) >= 1 |
          nrow(event2.2 <- occur(temp, p2.2, t2)) >= 1 |
          nrow(event2.3 <- occur(temp, p2.3, t2)) >= 1 |
          nrow(event2.4 <- occur(temp, p2.4, t2)) >= 1 |
          nrow(event2.5 <- occur(temp, p2.5, t2)) >= 1 |
          nrow(event2.6 <- occur(temp, p2.6, t2)) >= 1
        ) 
        &
        nrow(event3.0 <- occur(temp, p1, t2)) >= 1 
        & 
        nrow(event3 <- event3.0[which(event3.0$from > min(event1.1$from,event1.2$from,event1.3$from,event2.1$from,event2.2$from,event2.3$from,event2.4$from,event2.5$from,event2.6$from, na.rm=TRUE)),]) >=1 
        
    ){
      temp0 <- rbind(event1,event1.01,event1.02,event1.03,event1.04,event1.05,event1.06,event1.07,event1.08,event1.09,event1.1,event1.2,event1.3,event2.1,event2.2,event2.3,event2.4,event2.5,event2.6,event3)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}

df_outputOccurs10 <- QuestionOccurs(df_input, p = c(p1,p1.1,p1.2,p1.3,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6), t = c(t1,t2))
remove(t1,t2)
remove(p1,p1.1,p1.2,p1.3,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6)


# remove duplicated rows
length(which(duplicated(df_outputOccurs10)))
#df_outputOccurs10 <- df_outputOccurs10[!duplicated(df_outputOccurs10),]

plotMapsInput(df_input, LongLat = FALSE) 
plotMapsOutputEvents(df_outputOccurs10, LongLat = FALSE, sizeSquare = 1.4) 
df_output10 <- data.e
plotBarplotEvents(df_output10)
plotSequenceEvents(df_outputOccurs10, dateStart="2000-01-01", dateEnd="2016-12-31")    


#############################
# Rules
#########


# remove first line of data.frame that contain the from == "2000-08-15" and change by other label
df_temp <- df_outputOccurs10
df_first_line <- df_temp[which(df_temp$from == "2000-08-15"),]
df_posProc <- df_temp[which(df_temp$from != "2000-08-15"),]

# replace Forest to Secondary-vegetation
df_posProc$label <- as.character(df_posProc$label)
df_posProc$label[df_posProc$label == "Forest"] <- "Secondary-vegetation"

# merge first line dataframe with pos-processing dataframe
df_pos <- bind_rows(df_first_line,df_posProc)

# original data
df_temp2 <- df_input 

# temp3 dataframe with only index lines not in pos dataframe
df_temp3 <- df_temp2[!(df_temp2$index %in% df_pos$index),]

# merge df_input without index with renamed with lines without changes
df_input_new <- bind_rows(df_pos,df_temp3)
df_input_new <- data.frame(df_input_new[order(df_input_new$index),]) # order
#rownames(df_input_new) <- 1:nrow(df_input_new)

head(df_input_new)
# remove other
remove(df_temp,df_first_line,df_posProc,df_pos,df_temp2,df_temp3)

#############################





########################################

#+++++++++++++++++++++++++++++ 
# AREA "3" - IJGIS 
## Question 10 - Which "Forest" areas have been turned in secondary-vegetation after anyclass? 
# o = geo-objects, the own df_input data.frame

# p = properties of objects 
p1 <- "Forest" 

p1.2 <- "Pasture"

p2.1 <- "Soybean_Cotton" 
p2.2 <- "Soybean_Fallow" 
p2.3 <- "Soybean_Maize" 
p2.4 <- "Soybean_Millet" 
p2.5 <- "Soybean_Sunflower" 
p2.6 <- "Cotton_Fallow" 

# t = interval:
t1 <- interval(ymd("2000-08-15"),ymd("2001-08-15"))
t2 <- interval(ymd("2001-08-15"),ymd("2015-08-15"))

# Test occur for many time series in a dataframe
QuestionOccurs <- function(df, p, t){
  
  #df <- df_input
  library(dplyr)
  indexLong <- which(colnames(df) == "longitude")
  indexLat <- which(colnames(df) == "latitude")
  coord <- distinct(df[indexLong:indexLat])
  df_output = df[FALSE,]
  
  for(x in 1:nrow(coord)){
    # x=1
    temp <- filter(df, grepl(coord[x,1], as.character(df[,indexLong]), fixed = TRUE) &
                     grepl(coord[x,2], as.character(df[,indexLat]), fixed = TRUE))
    
    if ((nrow(event1 <- occur(temp, p1, t1)) >= 1 |
         # nrow(event1.01 <- occur(temp, p1.1, t1)) >= 1 |
         nrow(event1.02 <- occur(temp, p1.2, t1)) >= 1 |
         #nrow(event1.03 <- occur(temp, p1.3, t1)) >= 1 |
         
         nrow(event1.04 <- occur(temp, p2.1, t1)) >= 1 |
         nrow(event1.05 <- occur(temp, p2.2, t1)) >= 1 |
         nrow(event1.06 <- occur(temp, p2.3, t1)) >= 1 |
         nrow(event1.07 <- occur(temp, p2.4, t1)) >= 1 |
         nrow(event1.08 <- occur(temp, p2.5, t1)) >= 1 |
         nrow(event1.09 <- occur(temp, p2.6, t1)) >= 1
    ) 
    &
    (
      #  nrow(event1.1 <- occur(temp, p1.1, t2)) >= 1 |
      nrow(event1.2 <- occur(temp, p1.2, t2)) >= 1 |
      # nrow(event1.3 <- occur(temp, p1.3, t2)) >= 1 |
      
      nrow(event2.1 <- occur(temp, p2.1, t2)) >= 1 |
      nrow(event2.2 <- occur(temp, p2.2, t2)) >= 1 |
      nrow(event2.3 <- occur(temp, p2.3, t2)) >= 1 |
      nrow(event2.4 <- occur(temp, p2.4, t2)) >= 1 |
      nrow(event2.5 <- occur(temp, p2.5, t2)) >= 1 |
      nrow(event2.6 <- occur(temp, p2.6, t2)) >= 1
    ) 
    &
    nrow(event3.0 <- occur(temp, p1, t2)) >= 1 
    & 
    nrow(event3 <- event3.0[which(event3.0$from > min(event1.2$from,event2.1$from,event2.2$from,event2.3$from,event2.4$from,event2.5$from,event2.6$from, na.rm=TRUE)),]) >=1 
    
    ){
      temp0 <- rbind(event1,event1.02,event1.04,event1.05,event1.06,event1.07,event1.08,event1.09,event1.2,event2.1,event2.2,event2.3,event2.4,event2.5,event2.6,event3)
    } else {
      temp0 <- NULL
    }
    df_output <- bind_rows(df_output,temp0)
  }
  return(df_output)
}


df_outputOccurs10 <- QuestionOccurs(df_input, p = c(p1,p1.2,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6), t = c(t1,t2))
remove(t1,t2)
remove(p1,p1.2,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6)

# df_outputOccurs10 <- QuestionOccurs(df_input, p = c(p1,p1.1,p1.2,p1.3,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6), t = c(t1,t2))
# remove(t1,t2)
# remove(p1,p1.1,p1.2,p1.3,p2.1,p2.2,p2.3,p2.4,p2.5,p2.6)


# remove duplicated rows
length(which(duplicated(df_outputOccurs10)))
#df_outputOccurs10 <- df_outputOccurs10[!duplicated(df_outputOccurs10),]

plotMapsInput(df_input, LongLat = TRUE) 
plotMapsOutputEvents(df_outputOccurs10, LongLat = TRUE, sizeSquare = 1.4) 
df_output10 <- data.e
plotBarplotEvents(df_output10)
plotSequenceEvents(df_outputOccurs10, dateStart="2000-01-01", dateEnd="2016-12-31")    







###############################
# plot with secondary vegetation in R

#write.table(df_input_new, file = "~/Desktop/Sinop_Area_1.csv", sep = ",", quote = FALSE, row.names = FALSE)

df_input_new = read.table("~/Desktop/ESTUDO_TESE/Studies/Area_Sinop/Sinop1_postprocessing.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)

plotMapsInput(df_input_new, LongLat = TRUE) 





###############################
# transform in raster image
library(sp)
library(rgdal)
library(graphics)
library(raster)

pts = read.table("~/Desktop/ESTUDO_TESE/Studies/Area_Sinop/Sinop1_postprocessing.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE)

head(pts)
colnames(pts) <- c("x", "y", "z", "from", "to", "ID", "index")

# remove factor
pts$z <- as.numeric(factor(pts$z, levels=unique(pts$z)))

pts <- pts[pts$to == '2001-08-15',] 
pts <- pts[,1:3]
head(pts)

# # set up an 'empty' raster, here via an extent object derived from your data
# e <- extent(pts[,1:2])
# e <- e + 1000 # add this as all y's are the same
# 
# r <- raster(e, ncol=10, nrow=2)
# # or r <- raster(xmn=, xmx=,  ...
# 
# # you need to provide a function 'fun' for when there are multiple points per cell
# x <- rasterize(pts[, 1:2], r, pts[,3], fun=mean)
# plot(x)
# 

#pts[pts$z == '2',] 
coordinates(pts)=~x+y

proj4string(pts) = CRS("+init=epsg:4326") # set it to lat-long
gridded(pts) = TRUE

#pts = spTransform(pts,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# pts1 <- points2grid(pts,tolerance=0.140769)
# 
# r = raster(pts1)
# projection(r) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# r
# 
head(pts)

points <- SpatialPoints(pts[,c('x','y')], pts[,c('z')])
pixels <- SpatialPixelsDataFrame(points, tolerance = 0.916421, points@data)
raster <- raster(pixels[,'z'])








