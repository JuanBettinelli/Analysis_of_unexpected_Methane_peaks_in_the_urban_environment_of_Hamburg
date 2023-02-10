# script to model one wind direction with randomizing the wind direction and speed
# for mulipe traks
# plots points and heat map


library(MASS)
library(raster)


x <- c(1, 2, 3, 4, 5)
sd <- 0.5
x + rnorm(length(x), mean = 0, sd = sd)



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

# CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, npeaks = 1, threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,
# 
# names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
# CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
# CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
# CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]

CH4_Peaks <- CH4_Peak_Finder(TotalData, FALSE)
# CH4_Peaks <- CH4_Peaks[1:5,]

Geomatikum <- cbind(9.973287, 53.568073)
Total_Wind_Points <- data.frame()
for(l in 1:nrow(CH4_Peaks)){
  Start_point <- data.frame(UTC = CH4_Peaks[l, "UTC"], lon = Geomatikum[1], lat = Geomatikum[2], Point_No = 0, Speed = CH4_Peaks[l,"Speed"], Direction = CH4_Peaks[l,"Direction"], it_No = NA, Peak_No = l)
  
  sd_Speed <- 0.5
  sd_Direction <- 30
  

  for (j in 1:100){
    Wind_points <- Start_point
    Wind_points$it_No <- j
    for (i in 1:60){
      Wind_points[i+1, "Point_No"] <- i
      Wind_points[i+1, "UTC"] <- Wind_points[1, "UTC"] - i*60
      Wind_points[i+1, "Speed"] <-  Wind_points[1, "Speed"] + rnorm(1, mean = 0, sd = sd_Speed)
      Wind_points[i+1, "Direction"] <-  Wind_points[1, "Direction"] + rnorm(1, mean = 0, sd = sd_Direction)
      Wind_points[i+1, c("lon", "lat")]  <- destPoint(Wind_points[i, c("lon", "lat")],(Wind_points[i+1, "Direction"]),(Wind_points[i+1, "Speed"]*60*1))
      Wind_points[i+1, "it_No"] <- j
      Wind_points[i+1, "Peak_No"] <- l
    }
    Total_Wind_Points <- rbind(Total_Wind_Points, Wind_points)
  }
}


Total_Wind_Points_sf <- st_as_sf(Total_Wind_Points, coords = c("lon", "lat"),  crs = 4326)

# mapview(Wind_points, xcol = "lon", ycol = "lat", zcol = "Peak_No", crs = 4326, map.types = "Stamen.Toner")
mapview(Total_Wind_Points_sf, map.types = "Stamen.Toner", fun = "density", layer = "heatmap") 


# Calculate density of points
density_est <- kde2d(Total_Wind_Points$lon, Total_Wind_Points$lat, n = 100)

# Convert density estimate to raster
density_raster <- raster(density_est)

# Plot the raster
mapview(density_raster, alpha = 0.5)

