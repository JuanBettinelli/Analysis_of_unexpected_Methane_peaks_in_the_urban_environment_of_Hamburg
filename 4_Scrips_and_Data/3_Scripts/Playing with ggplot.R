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

FinishTime <- as.POSIXct('2021-09-06 00:00:00', 
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
TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
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
  
  

  p1 <- ggplot(TotalData_CH4_WL, aes(x = UTC,
                                     y = Direction*10)) + 
    geom_point() +
    theme(axis.line = element_line())
  p1
  
  p2 <- ggplot(TotalData_CH4_WL, aes(x = UTC,
                                     y = Water_Level)) +
    geom_point() +
    theme(axis.line = element_line())
  p2
  
  p3 <- ggplot(TotalData_CH4_WL, aes(x = UTC,
                       y = X.CH4.)) + 
    geom_point(aes(color = "CH4")) +
    geom_point(aes(y = Water_Level*5, color = "WL")) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./5,
                                            name="Water Level, mm"))+
    geom_point(aes(y = Direction*10, color = "WD")) +
    theme(axis.line = element_line(),
          plot.margin = margin(10, 10, 10, 30))
  p3
  wrap_elements(get_plot_component(p1, "ylab-l")) +
    wrap_elements(get_y_axis(p1)) +
    wrap_elements(get_plot_component(p2, "ylab-l")) +
    wrap_elements(get_y_axis(p2)) +
    p3 + 
    plot_layout(widths = c(3, 1, 3, 1, 40))

  