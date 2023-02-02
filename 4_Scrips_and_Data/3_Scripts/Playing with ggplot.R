# Script to Plot the Data From the "CombineMeteorologicalData.csv" created by the script "Combine_All_Data_To_CSV_File.R"
# Author Juan Bettinelli
# Last change: 19.1.23


# Script just to Play, 25.1.23!!!!!!

library(pacman)
library(lubridate)
library(readr)
library(plyr)
library(tidyverse)
library(ggplot2)   
library(hexbin)
library(gridExtra)
library(reshape2)
library(cowplot)
library(patchwork)


pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 

#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automaticaly access the data.
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


StartTime <- as.POSIXct('2021-08-01 22:03:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2021-08-7 00:00:00', 
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



########## ?????????????? #########
# cor(TotalData$WindSpeed,TotalData$WindDirction)

######## Plot CH4/Water level#############

# 
TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]), c("UTC", "X.CH4.")]

# test <- findpeaks(TotalData_CH4_WL$X.CH4., minpeakdistance = 50 , sortstr=TRUE) #npeaks=4, threshold=1, minpeakdistance = 100
# TotalData_CH4_WL[test[,2],]
# 
# for (i in test[,2]){
#   TotalData_CH4_WL['X.CH4.'][TotalData_CH4_WL['c'] == 14] <- 24
#   
#   TotalData_CH4_WL$Peak <- 1
# }

# CH4_Peaks <- as.data.frame(findpeaks(TotalData_CH4_WL$X.CH4., minpeakdistance = 5, threshold = 30, sortstr=TRUE))
# names(CH4_Peaks) <- c("CH4", "UTC", "UTC_Beginning", "UTC_Ending")
# CH4_Peaks$UTC_Beginning <- TotalData_CH4_WL[CH4_Peaks$UTC_Beginning,"UTC"]
# CH4_Peaks$UTC_Ending <- TotalData_CH4_WL[CH4_Peaks$UTC_Ending,"UTC"]
# CH4_Peaks$UTC <- TotalData_CH4_WL[CH4_Peaks$UTC,"UTC"]


# plot(TotalData_CH4_WL, type="l", col="navy")
# grid()
# points(CH4_Peaks$UTC, CH4_Peaks$CH4, pch=20, col="maroon")
# rect(xleft=CH4_Peaks$UTC_Beginning,xright = CH4_Peaks$UTC_Ending, ybottom= range(TotalData_CH4_WL$X.CH4.)[1] ,ytop=range(TotalData_CH4_WL$X.CH4.)[2], density=10, col = "blue") # ybottom=range(CVD$cvd)[1],ytop=range(CVD$cvd)[2],
# abline(v=CH4_Peaks$UTC_Beginning, col="green")
# abline(v=CH4_Peaks$UTC_Ending, col="red")


panel_function <- function(TotalData, n){
  if (n == 0){
    #for fixed panel
    TotalData$panel[TotalData$UTC <= "2021-08-10 23:59:00"] <- 0
    TotalData$panel[TotalData$UTC >= "2021-08-11 00:00:00" & TotalData$UTC <= "2021-08-18 23:59:00"] <- 1
    TotalData$panel[TotalData$UTC >= "2021-08-19 00:00:00" & TotalData$UTC <= "2021-08-28 23:59:00"] <- 2
    TotalData$panel[TotalData$UTC >= "2021-08-29 00:00:00"] <- 3
    return(TotalData)
  }
  else{
    #for automatic panel
    TotalData <- TotalData %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData))*n))
    return(TotalData)
  }
}
panel_No_function <- function(n){
  if (n == 0){
    m <- 1
    return(m)
  }
  else{
    m <- n
    return(m)
  }
}


CH4_TimeLine <- function(TotalData, StartTime, FinishTime, n){
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "panel")]
  
  
  # TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]), c("UTC", "X.CH4.")]
  
  CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4., minpeakdistance = 10, threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,
  names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
  CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
  CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
  CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]
  CH4_Peaks$panel <- CH4Data[match(CH4_Peaks$UTC, CH4Data$UTC),"panel"]
  CH4_min <- min(CH4Data$X.CH4.)
  CH4_max <- max(CH4Data$X.CH4.)
  

  CH4_TimeLine <- ggplot(CH4Data, aes(x = UTC, y = X.CH4.)) +
    geom_line() +
    labs(x = "Fill Time [UTC]",
         y ="CH4 mole fraction [ppb]",
         title = "CH4 mole fraction vs. Time") +
    scale_x_datetime(date_breaks = "2 day",
                     date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
    theme(axis.text.x=element_text(angle=60,
                                   hjust=1),
          strip.text.x = element_blank(),
          legend.position="none")+
    geom_rect(data=CH4_Peaks, inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
                                                ymax=CH4_max), color="transparent", fill="orange", alpha=0.3)+ #, group=group
    geom_point(data=CH4_Peaks, aes(x = UTC, y = X.CH4., col = "red"))
    facet_wrap(~panel,
               scales = 'free',
               nrow = m)
  
  ggsave(paste0("4_CH4_Timeline.png"),
         CH4_TimeLine,
         path = "4_Data/OutputData/Plots",
         width = 10,
         height = 5)
  
}



CH4_TimeLine_Separate_files <- function(TotalData, StartTime, FinishTime, n){
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "panel")]
  
  
  # TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]), c("UTC", "X.CH4.")]
  
  CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4., minpeakdistance = 10, threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,
  names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
  CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
  CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
  CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]
  CH4_Peaks$panel <- CH4Data[match(CH4_Peaks$UTC, CH4Data$UTC),"panel"]
  CH4_min <- min(CH4Data$X.CH4.)
  CH4_max <- max(CH4Data$X.CH4.)
  
  
  for (i in seq(0:m)){
    CH4_TimeLine <- ggplot(CH4Data[CH4Data$panel == i, ], aes(x = UTC, y = X.CH4.)) +
      geom_line() +
      labs(x = "Fill Time [UTC]",
           y ="CH4 mole fraction [ppb]",
           title = "CH4 mole fraction vs. Time") +
      scale_x_datetime(date_breaks = "2 day",
                       date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
      theme(axis.text.x=element_text(angle=60,
                                     hjust=1),
            strip.text.x = element_blank(),
            legend.position="none")+
      geom_rect(data=CH4_Peaks[CH4_Peaks$panel == i, ], inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
                                                       ymax=CH4_max), color="transparent", fill="orange", alpha=0.3)+ 
      geom_point(data=CH4_Peaks, aes(x = UTC, y = X.CH4., col = "red"))
    # facet_wrap(~panel,
    #            scales = 'free',
    #            nrow = m)
    # 
    ggsave(paste0("4_CH4_Timeline",i,".png"),
           CH4_TimeLine,
           path = "4_Data/OutputData/Plots",
           width = 10,
           height = 5)
    
  }
  
}


CH4_TimeLine_Separate_files(TotalData, StartTime, FinishTime, 4)



# 
# 
# 
# n <- 4
# TotalData_CH4_WL <- TotalData_CH4_WL %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData_CH4_WL))*n))
# 
# CH4_TimeLine <- ggplot(TotalData_CH4_WL) +
#     geom_line(aes(x = UTC,
#                   y = X.CH4.),
#               col = "red") +
#     labs(x = "Fill Time [UTC]",
#          y ="CH4 mole fraction [ppb]",
#          title = "CH4 mole fraction vs. Time") +
#     scale_x_datetime(date_breaks = "1 day",
#                      date_labels = "%d-%m-%Y") +
#     theme(axis.text.x=element_text(angle=60, hjust=1),
#           axis.title.y = element_text(color = "red",
#                                       size=13),
#           axis.text.y = element_text(color = "red"),
#           axis.title.y.right = element_text(color = "blue",
#                                             size=13),
#           axis.text.y.right = element_text(color = "blue"),
#           strip.text.x = element_blank()) +
#     geom_line(aes(x = UTC,
#                   y = Water_Level*5),
#               col = "blue") +
#     scale_y_continuous(sec.axis = sec_axis(trans = ~./5,
#                                            name="Water Level, mm"))+
#   facet_wrap(~panel, scales = 'free', nrow = n)
# 
# 
#   CH4_TimeLine

# ,
#  limit=c(StartTime, 
#          FinishTime)
  

# TotalData$panel[TotalData$UTC <= "2021-08-10 23:59:00"] <- 0
# TotalData$panel[TotalData$UTC >= "2021-08-11 00:00:00" & TotalData$UTC <= "2021-08-18 23:59:00"] <- 1
# TotalData$panel[TotalData$UTC >= "2021-08-19 00:00:00" & TotalData$UTC <= "2021-08-28 23:59:00"] <- 2
# TotalData$panel[TotalData$UTC >= "2021-08-29 00:00:00"] <- 3
# 
# TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),c("UTC", "X.CH4.","panel")]
# # names(TotalData_CH4)[names(TotalData_CH4)=="UTC"] <- "UTC1"
# TotalData_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "Water_Level")]),c("UTC", "Water_Level","panel")]
# # names(TotalData_WL)[names(TotalData_WL)=="UTC"] <- "UTC2"
# TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Direction", "Speed")]),c("UTC", "Direction", "Speed","panel")]
# # names(TotalData_Wind)[names(TotalData_Wind)=="UTC"] <- "UTC3"


# n <-4
# for(i in 0:(n-1)){
#   p1 <- ggplot(TotalData_CH4[TotalData_CH4$panel == i,], aes(x = UTC,
#                                                               y = X.CH4.)) + 
#     ylim(1600, 4300) +
#     labs(y ="CH4 Concentration")+
#     geom_line() +
#     theme(axis.line = element_line(),
#           plot.margin = margin(0, 0, 0, 0))
#   p1
#   
#   p2 <- ggplot(TotalData_WL[TotalData_WL$panel == i,], aes(x = UTC,
#                                      y = Water_Level)) +
#     geom_line() +
#     theme(axis.line = element_line(),
#           plot.margin = margin(0, 0, 0, 0))
#   p2
#     
#   p3 <- ggplot(data = TotalData_Wind[TotalData_Wind$panel == i,], aes(x = UTC, y = Direction)) +
#     geom_line(aes(color = "Wind Dircection")) + 
#     ylim(0, 360) +
#     labs(x = "UTC",
#          y ="Wind Direction, °",
#          title = "Wind Direction, Waterlevel, CH4 Concentration vs. Time") +
#     geom_line(data = TotalData_WL[TotalData_WL$panel == i,], aes(x = UTC, y = (Water_Level/1.5-200), color = "Water Level")) +
#     scale_x_datetime(date_breaks = "1 day",
#                      date_labels = "%d-%b")+
#     geom_line(TotalData_CH4[TotalData_CH4$panel == i,], mapping = aes(x = UTC, y = (X.CH4./7-250) , color = "CH4.")) +
#     scale_y_continuous(sec.axis = sec_axis(trans = ~(.*1.5+200),
#                                            name="Waterlevel, mm"))+
#     theme(axis.line = element_line(), 
#           plot.margin = margin(0, 0, 0, 20),
#           axis.text.x=element_text(angle=60, hjust=1),
#           axis.title.y = element_text(color = "black",
#                                       size=13),
#           axis.text.y = element_text(color = "black"),
#           strip.text.x = element_blank(),
#           legend.position = "bottom",
#           legend.title=element_blank())
#   p3
#   
#   p4 <- wrap_elements(get_plot_component(p1, "ylab-l")) +
#           wrap_elements(get_y_axis(p1)) +
#           # wrap_elements(get_plot_component(p2, "ylab-l")) +
#           # wrap_elements(get_y_axis(p2)) +
#           p3 + 
#           plot_layout(widths = c(1, 1, 40))
#   p4
#   
#   ggsave(paste0("5_CH4_Wl_WD_",i,".png"), p4, path = "4_Data/OutputData/Plots", width = 10, height = 5)
#   
# }
#   