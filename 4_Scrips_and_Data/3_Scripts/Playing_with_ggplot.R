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


print("hallo wold")
