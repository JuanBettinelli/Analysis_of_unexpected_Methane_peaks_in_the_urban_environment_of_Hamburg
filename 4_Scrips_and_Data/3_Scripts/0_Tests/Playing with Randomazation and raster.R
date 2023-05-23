# script to model one wind direction with randomizing the wind direction and speed
# for mulipe traks
# plots points and heat map
# This script is now transfert to 'CH4_Transportmodel.r'

library(geosphere)
library(dplyr)
library(plotly)
library(rio)
library(pracma)
library(sf)
library(mapview)
library(MASS)
library(raster)


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
# SD for the wind
sd_Speed <- 0.5
sd_Direction <- 30
# number of minuts that are considered bekor the peak
Backwared_Minutes <- 60

# Number of Portical released
Released_Particals <- 100

# Geomatikum Location
Geomatikum <- cbind(9.973287, 53.568073)

# Create a data frame with CH4 concentration
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

# Find the CH4 Peaks
CH4_Peaks <- CH4_Peak_Finder(TotalData, FALSE)
# CH4_Peaks <- CH4_Peaks[1:5,] # Can be used to only that the largest peaks

# Create a data frame where all points will be placed
Total_Wind_Points <- data.frame()

# loop for all peaks
for(l in 1:nrow(CH4_Peaks)){
  # Set the Starting point for the wind calculation
  Start_point <- data.frame(UTC = CH4_Peaks[l, "UTC"], lon = Geomatikum[1], lat = Geomatikum[2], Point_No = 0, Speed = CH4_Peaks[l,"Speed"], Direction = CH4_Peaks[l,"Direction"], it_No = NA, Peak_No = l)
  
  # Loop trough the amount of particals released per Peak
  for (j in 1:Released_Particals){
    
    # Set up Starting for each Peak 
    Wind_points <- Start_point
    Wind_points$it_No <- j
    
    # Loop for the amounts of minutes that are backtracked
    for (i in 1:Backwared_Minutes){
      Wind_points[i+1, "Point_No"] <- i
      # Find the time one minute bevore
      Wind_points[i+1, "UTC"] <- Wind_points[1, "UTC"] - i*60
      # find the wind speed at this minute  an randomize it
      Wind_points[i+1, "Speed"] <-  Wind_points[1, "Speed"] + rnorm(1, mean = 0, sd = sd_Speed)
      # find the wind Direction at this minute  an randomize it
      Wind_points[i+1, "Direction"] <-  Wind_points[1, "Direction"] + rnorm(1, mean = 0, sd = sd_Direction)
      # find the destination point one minute before
      Wind_points[i+1, c("lon", "lat")]  <- destPoint(Wind_points[i, c("lon", "lat")],(Wind_points[i+1, "Direction"]),(Wind_points[i+1, "Speed"]*60*1))
      Wind_points[i+1, "it_No"] <- j
      Wind_points[i+1, "Peak_No"] <- l
    }
    # Attach the data frame
    Total_Wind_Points <- rbind(Total_Wind_Points, Wind_points)
  }
}

# find enteris were 'lon' and 'lat' is given
Total_Wind_Points <- Total_Wind_Points[complete.cases(Total_Wind_Points[ , c("lon", "lat")]),]

# # Plot the Particals as Individual points on interactive map
# mv_points <- mapview(Total_Wind_Points, xcol = "lon", ycol = "lat", zcol = "Peak_No",cex = 0.5, alpha = 0.5, crs = 4326, map.types = "Stamen.Toner")
# # Save the Map as PNG
# mapshot(mv_points, file = "4_Data/OutputData/Plots/Maps/10_Emission_Points_with_Changing_Measured_Wind.png")


# # concert the data frame into a sf 'simple form' location object
# Total_Peaks_sf <- st_as_sf(Total_Wind_Points, coords = c("lon", "lat"),  crs = 4326)
# # Plot the Particals from sf object as Individual points on interactive map
# mapview(Total_Peaks_sf,  zcol = "Peak_No",cex = 0.5, alpha = 0.5, map.types = "Stamen.Toner")



# Calculate density distribution of points
density_est <- kde2d(Total_Wind_Points$lon, Total_Wind_Points$lat, n = 300)

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


