# Working Script to create traks by wind 10.2.23
# Need reworking and tidying
# also check "Playing with Partical Traks.R" for code that is not include
# script is now in CH4_Transportmodel.R



library(geosphere)
# library(pacman)
# library(lubridate)
# library(readr)
# library(plyr)
# library(tidyverse)
# library(ggplot2)   
# library(hexbin)
# library(gridExtra)
# library(reshape2)
# library(openair)
# library(cowplot)
# library(patchwork)
library(dplyr)
# library(GGally)
# library(ggthemes)
# library(ggvis)
# library(httr)
library(plotly)
library(rio)
# library(rmarkdown)
# library(shiny)
# library(stringr)
# library(tidyr)
library(pracma)
# library(plotKML)
# library(ggmap)
library(sf)
library(mapview)
library(MASS)
library(raster)
# library(animation)
# library(htmlwidgets)
# library(webshot)


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

# Find the CH4 Peaks
CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, npeaks = , threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,

# format the the Data frame
names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]

# create a Waterlevel Dataframe
WLData <- TotalData[complete.cases(TotalData[ , "Water_Level"]),c("UTC", "Water_Level")]

# Input Variables
sd_Speed <- 0.5
sd_Direction <- 30
Geomatikum <- cbind(9.973287, 53.568073) # (Lon, Lat)

Total_Peaks <- data.frame()
for(j in 1:nrow(CH4_Peaks)){
  
  # loop for how many wind Partical are released
  for(l in 1:10){
    # Create the Startpoint (at Geomatikum) data frame
    Single_Point <- data.frame(UTC = CH4_Peaks[j, "UTC"], lon = Geomatikum[1], lat = Geomatikum[2], Peak_No = j, it_No = l)
    
    # Find the meak max time
    Peak_Time <- CH4_Peaks[j, "UTC"]
    
    # Get the Waterlevel for the 12h before the Peak
    WL_at_Peak <- WLData[WLData$UTC >= (Peak_Time - (12*60*60)) & WLData$UTC <= Peak_Time,]
    
    # Find the minimum Water level
    MinWL <- WL_at_Peak[which.min(WL_at_Peak$Water_Level),]
    
    # Get all the Wind data between the Max CH4 Peak and the Waterlevel Lowpoint before the Peak plus 0.5 hour (One hour earlier)
    All_Wind <- TotalData[TotalData$UTC >= (MinWL$UTC - 0.5*60*60) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]  
    
    # Check if there is less then 4 wind data enterys (peak at min for example), if that's the case take 1 hour before the peak
    if (nrow(All_Wind) < 4) {
      All_Wind <- TotalData[TotalData$UTC >= (CH4_Peaks[j,"UTC"]-60*60) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]  
    }
    
    # Selects only enteris with Wind an Speed Direction, takes only wind data and time
    All_Wind <- All_Wind[complete.cases(All_Wind[ , c("Wind_Speed", "Wind_Direction")]), c("UTC", "Wind_Speed", "Wind_Direction")] #Geomatikum Dind data

        # checks if data frame is empty
    if (nrow(All_Wind) == 0) {
      All_Wind <- TotalData[TotalData$UTC >= (CH4_Peaks[j,"UTC"]-1*60*60) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]}
    
    # Loop to find the the wind direction and speed for every avaleble entry in the dataframe
    for(i in 2:nrow(All_Wind)){ 
      # Find the time difference to previus point
      Time_dif <- as.numeric(difftime(strptime(All_Wind[i, "UTC"], "%Y-%m-%d %H:%M:%S", tz = "UTC"), strptime(All_Wind[i-1, "UTC"], "%Y-%m-%d %H:%M:%S", tz = "UTC")))
      # find destination point of particle 
      Single_Point[i,2:3]  <- destPoint(Single_Point[(i-1),2:3],(All_Wind[i,"Wind_Direction"]+ rnorm(1, mean = 0, sd = sd_Direction)),(All_Wind[i,"Wind_Speed"]*60*Time_dif + rnorm(1, mean = 0, sd = sd_Speed))) ################## Check Wind direktion
      # Enter the Peak number in data frame
      Single_Point[i,4] <- j
      # Enter the time in data frame
      Single_Point[i,1] <- All_Wind[i, "UTC"]
      # enter iteration Number in data frame
      Single_Point[i,5] <- l
    }
    # Attach the data frame to Final Data frame
    Total_Peaks <- rbind(Total_Peaks,Single_Point)
  }
}

# Filter empty rows out of Data frame
Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , c("lon", "lat")]),]
# # Plot the Particals as Individual points on interactive map
# mv_points <- mapview(Total_Peaks, xcol = "lon", ycol = "lat", zcol = "Peak_No",cex = 0.5, alpha = 0.5, crs = 4326, map.types = "Stamen.Toner")
# # Save the Map as PNG
# mapshot(mv_points, file = "4_Data/OutputData/Plots/Maps/10_Emission_Points_with_Changing_Measured_Wind.png")


# # concert the data frame into a sf 'simple form' location object
# Total_Peaks_sf <- st_as_sf(Total_Peaks, coords = c("lon", "lat"),  crs = 4326)
# # Plot the Particals from sf object as Individual points on interactive map
# mapview(Total_Peaks_sf,  zcol = "Peak_No",cex = 0.5, alpha = 0.5, map.types = "Stamen.Toner") 


# Calculate density distribution of points
density_est <- kde2d(Total_Peaks$lon, Total_Peaks$lat, n = 300)

# Convert density into raster
density_raster <- raster(density_est)

# Select the color palette of the raster plot
colorPalette <- colorRampPalette(c("blue", "red", "white"))(255)
# Replaces very small values with NA, to make the rasta transperant in this section
values(density_raster)[values(density_raster) < 0.5] = NA
# Plot the raster on a map
mv_Raster <- mapview(density_raster, na.color = "transparent", alpha = 0.5,  col.regions = colorPalette, trans = "log") 
# Save the Map
mapshot(mv_Raster, file = "4_Data/OutputData/Plots/Maps/10_Emission_Distribution_with_Changing_Measured_Wind.png")