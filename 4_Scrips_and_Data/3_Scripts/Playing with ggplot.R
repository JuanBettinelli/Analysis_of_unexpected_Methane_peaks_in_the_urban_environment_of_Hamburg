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


# 
# # Function to Plot a CH4 Timeline with A Peak detection
# CH4_Peak_Finder <- function(TotalData, StartTime, FinishTime){
# 
# 
#   #Select the Data from dataframe with CH4 Concentration
#   CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]
#   
#   # Find the Peaks in the timeline
#   CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4., minpeakdistance = 10, threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,
#   
#   # Format the Peak Dataframe
#   names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
#   CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
#   CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
#   CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]
#   
#   # Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
#   # get all Coloum Names
#   Heads <- colnames(TotalData)
#   for (j in Heads){
#     # Create new Coloums with same Names
#     CH4_Peaks[,j] <- NA
#     for(i in 1:nrow(CH4_Peaks)) {       # for-loop over rows
#       # Find the mean Values during the Peak
#       CH4_Peaks[i, j] <- mean(TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], j], na.rm = TRUE)
#     }
#   }
#   write.csv(CH4_Peaks, "4_Data/OutputData/CH4_Peaks.csv", row.names=TRUE)
#   # return(CH4_Peaks)
# }
# 
# 
# CH4_Peak_Finder(TotalData, StartTime, FinishTime)


# 
# if(!require('openair')) {
#   install.packages('openair')
#   library('openair')
# }
# 
# # load example data from package data(mydata)
# 
# # basic plot
# windRose(mydata)
# 
# # one windRose for each year
# windRose(mydata,type = "year")
# 
# # windRose in 10 degree intervals with gridlines and width adjusted
# if (FALSE) {
#   windRose(mydata, angle = 10, width = 0.2, grid.line = 1)
# }
# 
# # pollutionRose of nox
# pollutionRose(mydata, pollutant = "nox")
# head(mydata)
# 
# ## source apportionment plot - contribution to mean
# if (FALSE) {
#   pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")
# }
# 
# ## example of comparing 2 met sites
# ## first we will make some new ws/wd data with a postive bias
# mydata$ws2 = mydata$ws + 2 * rnorm(nrow(mydata)) + 1
# mydata$wd2 = mydata$wd + 30 * rnorm(nrow(mydata)) + 30
# 
# ## need to correct negative wd
# id <- which(mydata$wd2 < 0)
# mydata$wd2[id] <- mydata$wd2[id] + 360
# 
# ## results show positive bias in wd and ws
# pollutionRose(mydata, ws = "ws", wd = "wd", ws2 = "ws2", wd2 = "wd2")


source("3_Scripts/Functions.R")


WindRose_Plots(TotalData, StartTime, FinishTime)
