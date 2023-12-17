# Script that produces correlation plots between CH4 concentration and BLH of the air
# Author:
# Juan Bettinelli,
# Script that preformed a Keeling analyse of the CH4 Measuments at the Geomatikum
# The the Analyse is done depending on the wind direction. 
# The Peaks can be seen separate.
# 22.5.2023

# Declare librarys used

library(tidyverse)
library(ggplot2)   
library(hexbin)
library(dplyr)
library(lubridate)
library(plotly)
library(rio)
library(pracma)
library(tidyr)

# Set Working Directory
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


## Select the dates to be analyed
# Set Starting and Finish Time
StartTime <- as.POSIXct('2021-08-02 00:00:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00
FinishTime <- as.POSIXct('2021-11-11 00:00:00', 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz ="utc")
# Total Timeseries: 2022-03-29 00:00:00
# Hamburg Campagne Timeseries: 2021-09-06 00:00:00
# Hamburg Campaine #2: 2021-09-17 10:21:00

# Wind_Provider = 2 # Wind_Provider = 1(Geomatikum), 2(Mast 50m) 3(Mast 110m), 4(DWD)


########### Read data from the CSV File #############

# Read the CSV File
TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
# format the Date 'UTC'
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
# Convert the format of 'X.CH4.' to numeric
TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)
# Filter out all the dated that are outside the selected Starting and Finish time of the campaign
TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)
# Remove Empty Cells n data frame
TotalData <- TotalData[!is.na(TotalData$UTC),]

# Calculate 1/Mole Fraction for C13 & H2 for the Keeling analyse and add as new column
TotalData$c13C <- 1/TotalData$X.CH4..13C
TotalData$c2H <- 1/TotalData$X.CH4..2H

TotalData <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC","X.CH4.", "ERA5_BLH", "LIDAR_BLH")]

# TotalData <- fill(TotalData, ERA5_BLH, LIDAR_BLH, .direction = "up")


TotalData_ERA5 <- TotalData[complete.cases(TotalData[ , "ERA5_BLH"]),c("UTC","X.CH4.", "ERA5_BLH", "LIDAR_BLH")]

ERA5_BLH_Correlation <- cor.test(TotalData_ERA5$X.CH4., TotalData_ERA5$ERA5_BLH, use="complete.obs")
print(ERA5_BLH_Correlation)

# Calculate the R-squared
r2_ERA5 <- cor(TotalData_ERA5$X.CH4., TotalData_ERA5$ERA5_BLH)^2
print("R^2 for ERA5:")
print(r2_ERA5)



TotalData_Lidar <- TotalData[complete.cases(TotalData[ , "LIDAR_BLH"]),c("UTC","X.CH4.", "ERA5_BLH", "LIDAR_BLH")]


Lidar_BLH_Correlation <- cor.test(TotalData_Lidar$X.CH4., TotalData_Lidar$LIDAR_BLH, use="complete.obs")
print(Lidar_BLH_Correlation)

# Calculate the R-squared
r2_Lidar <- cor(TotalData_Lidar$X.CH4., TotalData_Lidar$LIDAR_BLH)^2
print("R^2 for Lidar:")
print(r2_Lidar)
