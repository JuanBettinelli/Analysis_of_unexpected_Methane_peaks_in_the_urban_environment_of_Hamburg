# Working Script to create traks by wind 10.2.23
# Need reworking and tidying
# also check "Playing with Partical Traks.R" for code that is not includet



library(geosphere)
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
library(plotKML)
library(ggmap)
library(tidyverse)
library(sf)
library(mapview)


setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

source("3_Scripts/Functions.R")

StartTime <- as.POSIXct('2021-08-01 22:03:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2022-03-29 00:00:00', 
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


######## Finding the Peaks, The Average Meteorological Data during Peak, Saving csv File #########
# CH4_Peak_Finder(TotalData, TRUE)
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, npeaks = , threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,

names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]


WLData <- TotalData[complete.cases(TotalData[ , "Water_Level"]),c("UTC", "Water_Level")]

Total_Peaks <- data.frame()
for(j in 1:nrow(CH4_Peaks)){
  Geomatikum <- cbind(9.973287, 53.568073) # (Lon, Lat)
  x <- data.frame(lon = Geomatikum[1], lat = Geomatikum[2], Peak_No = j, UTC = CH4_Peaks[j, "UTC"])
  
  test <- CH4_Peaks[j, "UTC"]
  WL_at_Peak <- WLData[WLData$UTC >= (test - (12*60*60)) & WLData$UTC <= test,]
  MinWL <- WL_at_Peak[which.min(WL_at_Peak$Water_Level),]
  
  All_Wind <- TotalData[TotalData$UTC >= MinWL$UTC & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]  

  if (nrow(All_Wind) < 4) {
    All_Wind <- TotalData[TotalData$UTC >= (CH4_Peaks[j,"UTC"]-60*60) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]  
  }
  
  All_Wind <- All_Wind[complete.cases(All_Wind[ , c("Speed", "Direction")]), c("UTC", "Speed", "Direction")]
  
  if (nrow(All_Wind) == 0) {
    All_Wind <- TotalData[TotalData$UTC >= (CH4_Peaks[j,"UTC"]-1*60*60) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]}
  
  
  for(i in 2:nrow(All_Wind)){ 
    x[i,1:2]  <- destPoint(x[(i-1),1:2],(All_Wind[i,"Direction"]),(All_Wind[i,"Speed"]*60*10)) ################## Check Wind direktion
    x[i,3] <- j
    x[i,4] <- All_Wind[i, "UTC"]
  }
  Total_Peaks <- rbind(Total_Peaks,x)

}

Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , c("lon", "lat")]),]


sbux_sf <- st_as_sf(Total_Peaks, coords = c("lon", "lat"),  crs = 4326)

mapview(Total_Peaks, xcol = "lon", ycol = "lat", zcol = "Peak_No", crs = 4326, map.types = "Stamen.Toner")
mapview(sbux_sf,  zcol = "Peak_No", map.types = "Stamen.Toner") 

