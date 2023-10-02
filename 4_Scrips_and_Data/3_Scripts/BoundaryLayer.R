


library(pacman)
library(lubridate)
library(readr)
library(plyr)
library(tidyverse)
library(ggplot2)   
library(hexbin)
library(gridExtra)
library(reshape2)
library(openair)
library(cowplot)
library(patchwork)
library(dplyr)
library(GGally)
library(ggthemes)
library(ggvis)
library(httr)
library(plotly)
library(rio)
library(rmarkdown)
library(shiny)
library(stringr)
library(tidyr)
library(pracma)


#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automatically access the data.
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

source("3_Scripts/Functions.R")
source("3_Scripts/CH4_Transportmodel.R")

StartTime <- as.POSIXct('2021-08-01 22:03:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2021-09-10 00:00:00', 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz ="utc")

# Total Timeseries: 2022-03-29 00:00:00
# Hamburg Campagne Timeseries: 2021-09-06 00:00:00
# Hamburg Campaine #2: 2021-09-17 10:21:00

########### Read the CSV File #############

TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")

TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)

TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)

TotalData$UTC <- as.POSIXct(TotalData$UTC, 
                            format = "%d-%m-%Y %H:%M:%S", 
                            tz = "utc")

TotalData$Direction[TotalData$Direction > 361] <- NA
TotalData$Speed[TotalData$Speed > 99] <- NA

n = 2

  
# TotalData_CH4 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
TotalData_CH4 <- TotalData[,c("UTC", "X.CH4.", "ERA5_BLH", "LIDAR_BLH")]
TotalData_CH4 <- fill(TotalData_CH4, starts_with("ERA5_BLH"), .direction = "up")
TotalData_CH4 <- fill(TotalData_CH4, starts_with("LIDAR_BLH"), .direction = "up")

# WL_CH4_Data <- melt(TotalData_CH4, id.var="UTC")
TotalData_CH4_BLH <- TotalData_CH4[complete.cases(TotalData_CH4[ , c("UTC", "X.CH4.")]),]



  #Split Timeline into Panels
  TotalData_CH4_BLH <- panel_function(TotalData_CH4_BLH, n)
  m <- panel_No_function(n)
  
#TotalData_CH4_BLH <- fill(TotalData_CH4_BLH, starts_with("ERA5_BLH"), .direction = "up")
#TotalData_CH4_BLH <- fill(TotalData_CH4_BLH, starts_with("LIDAR_BLH"), .direction = "up")
  
  # Plot CH4, Waterlevel Vs Time
  CH4_TimeLine <- ggplot(TotalData_CH4_BLH) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y =expression("CH"[4]*" concentration [ppb]"),
         title = "Methane concentration & BLH ERA5 vs. time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    # limits = c(as.POSIXct('2021-08-01 00:00:00', 
    #                       format = "%Y-%m-%d %H:%M:%S", 
    #                       tz ="utc"), as.POSIXct('2021-08-18 00:00:00', 
    #                                              format = "%Y-%m-%d %H:%M:%S", 
    #                                              tz ="utc"))) +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = ERA5_BLH*3),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./3,
                                           name="BLH, m"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  CH4_TimeLine
  
  #Export the plot to PNG file
  ggsave("1_CH4_BLH_ERA5.png", CH4_TimeLine, path = "4_Data/OutputData/Plots/SecondPaper/BondaryLayer", width = 10, height = 5)
  
  
  
  CH4_TimeLine <- ggplot(TotalData_CH4_BLH) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y =expression("CH"[4]*" concentration [ppb]"),
         title = "Methane concentration & Elbe BLH LIDAR vs. time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    # limits = c(as.POSIXct('2021-08-01 00:00:00', 
    #                       format = "%Y-%m-%d %H:%M:%S", 
    #                       tz ="utc"), as.POSIXct('2021-08-18 00:00:00', 
    #                                              format = "%Y-%m-%d %H:%M:%S", 
    #                                              tz ="utc"))) +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = LIDAR_BLH*3),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./3,
                                           name="BLH,m"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  CH4_TimeLine
  
  #Export the plot to PNG file
  ggsave("1_CH4_BLH_LIDAR.png", CH4_TimeLine, path = "4_Data/OutputData/Plots/SecondPaper/BondaryLayer", width = 10, height = 5)  








TotalData_CH4 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
TotalData_CH4 <- TotalData_CH4[,c("UTC", "X.CH4.", "ERA5_BLH")]
TotalData_CH4 <- fill(TotalData_CH4, starts_with("ERA5_BLH"), .direction = "up")

# WL_CH4_Data <- melt(TotalData_CH4, id.var="UTC")
TotalData_CH4_BLH <- TotalData[complete.cases(TotalData[ , c("UTC", "ERA5_BLH")]),]

 png(file="4_Data/OutputData/Plots/SecondPaper/BondaryLayer/16_Basic_Plot_CH4_ERA5.png",
     width=1200, height=600)
par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
plot(TotalData_CH4_BLH$UTC, TotalData_CH4_BLH$ERA5_BLH,
     type = "l",
     cex = 10,
     xlab = "Date/Time UTC",
     ylab = "Boundary Layer Hight, m",
     xlim = c(StartTime, FinishTime))

par(new = TRUE)
plot(TotalData_CH4$UTC, TotalData_CH4$X.CH4.,
     main = "ERA5 Boundary Layer Hight/CH4 Concentation Vs. Time",
     type = "l",
     cex = 10,
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(c(StartTime, FinishTime)))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)
 dev.off() 

 TotalData_CH4 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
 TotalData_CH4 <- TotalData_CH4[,c("UTC", "X.CH4.", "LIDAR_BLH")]
 
 #WL_CH4_Data <- melt(TotalData_CH4, id.var="UTC")
 TotalData_CH4_BLH <- TotalData[complete.cases(TotalData[ , c("UTC", "LIDAR_BLH")]),]
 
 png(file="4_Data/OutputData/Plots/SecondPaper/BondaryLayer/16_Basic_Plot_CH4_LIDAR.png",
     width=1200, height=600)
 par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
 plot(TotalData_CH4_BLH$UTC, TotalData_CH4_BLH$LIDAR_BLH,
      type = "l",
      cex = 10,
      xlab = "Date/Time UTC",
      ylab = "Boundary Layer Hight, m",
      xlim = c(StartTime, FinishTime))
 
 par(new = TRUE)
 plot(TotalData_CH4$UTC, TotalData_CH4$X.CH4.,
      main = "LIDAR_BLH Boundary Layer Hight/CH4 Concentation Vs. Time",
      type = "l",
      cex = 10,
      col="red",
      axes = FALSE,
      bty = "n",
      xlab = "",
      ylab = "",
      xlim = c(c(StartTime, FinishTime)))
 
 axis(side=4,
      col.axis="red",
      col="red")
 mtext("CH4 Concentration",
       col="red",
       side=4,
       line=3)
 dev.off() 
 
