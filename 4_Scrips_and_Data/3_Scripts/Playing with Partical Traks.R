### Script not up to date "Playing with Poltting Partical Track.R" is more advanzed and working
### Playing with Randomazation and raster.R has olso more working elements
# 10.2.23


# if(!require('spatialEco')) {
#   install.packages('spatialEco')
#   library('spatialEco')
# }
# 
# pt <- cbind( x=9.973287 , y=53.568073)
# bearing.distance(pt[1], pt[2], 0.03887689, 190)
# 
# 

# 53°34'05.1"N 9°58'23.8"E
# 53.568073, 9.973287

# 53.534217, 9.961485

library(geosphere)
# Script to Plot the Data From the "CombineMeteorologicalData.csv" created by the script "Combine_All_Data_To_CSV_File.R"
# Author Juan Bettinelli
# Last change: 26.1.23

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



# pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
#                ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
#                stringr, tidyr) 

#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automaticaly access the data.
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

CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, npeaks = 3, threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,

names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]


Total_Peaks <- data.frame()
for(j in 1:nrow(CH4_Peaks)){
  Geomatikum <- cbind(9.973287, 53.568073) # (Lon, Lat)
  x <- data.frame(lon = Geomatikum[1], lat = Geomatikum[2], Peak_No = j, UTC = CH4_Peaks[j, "UTC"])

  All_Wind <- TotalData[TotalData$UTC >= CH4_Peaks[,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[,"UTC"], ]
  All_Wind <- All_Wind[complete.cases(All_Wind[ , c("Speed110m", "Direction110m")]), c("UTC", "Speed110m", "Direction110m")]
  
  for(i in 2:nrow(All_Wind)){ 
    x[i,1:2]  <- destPoint(x[(i-1),1:2],(All_Wind[i,"Direction110m"]),(All_Wind[i,"Speed110m"]*60*10))
    x[i,3] <- j
    x[i,4] <- All_Wind[i, "UTC"]
  }
  Total_Peaks <- rbind(Total_Peaks,x)
}



mapview(Total_Peaks, xcol = "lon", ycol = "lat", crs = 4269, grid = FALSE)

# dat <- structure(list(Blong = c(-75.58333, -76.08333, -81.08333, -94.25, 
#                                 -75.41667, -99.41667, -77.41667, -116.08333, -89.58333, -77.58333
# ), Blat = c(37.58333, 40.58333, 42.75, 41.91667, 38.25, 28.25, 
#             38.91667, 43.58333, 44.25, 38.91667), Elong = c(-65.91667, -75.75, 
#                                                             -80.58333, -95.41667, -73.58333, -89.41667, -77.58333, -116.41667, 
#                                                             -96.41667, -77.41667), Elat = c(45.91667, 40.58333, 42.75, 29.75, 
#                                                                                             45.58333, 48.25, 38.75, 43.58333, 34.08333, 38.91667), Flyway = structure(c(2L, 
#                                                                                                                                                                         2L, 2L, 1L, 2L, 2L, 2L, 3L, 2L, 2L), .Label = c("Central", "Eastern", 
#                                                                                                                                                                                                                         "West"), class = "factor")), .Names = c("Blong", "Blat", "Elong", 
#                                                                                                                                                                                                                                                                 "Elat", "Flyway"), row.names = c(NA, -10L), class = c("tbl_df", 
#                                                                                                                                                                                                                                                                                                                       "tbl", "data.frame"))
# 
# 
# b = dat[, c("Blong", "Blat")]
# names(b) = c("long", "lat")
# e = dat[, c("Elong", "Elat")]
# names(e) = c("long", "lat")

# test <- st_as_sf(Total_Peaks,
#                  coords = c("lon", "lat"),
#                  crs = 4269)

################
# t <- data.frame(matrix(0, ncol = 1, nrow = nrow(Total_Peaks)-1))
# names(t) <- c("geometry")
# 
# 
# t$geometry = do.call(
#   "c", 
#   lapply(seq(nrow(Total_Peaks)-1), function(i) {
#     st_sfc(
#       st_linestring(
#         as.matrix(
#           rbind(Total_Peaks[i, 1:2], Total_Peaks[i+1, 1:2])
#         )
#       ),
#       crs = 4326
#     )
#   }))
# 
# for(k in 1:nrow(Total_Peaks)-1){
#   t[k, "UTC"] <- Total_Peaks[(k + 1), "UTC"]
#   t[k, "Peak_No"] <- Total_Peaks[(k + 1), "Peak_No"]
# }
# 
# dat_sf = st_as_sf(t)
# 
# mapview(dat_sf$geometry, zcol = "Peak_No")
# 
# 


###############

# library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.1, PROJ 6.2.0

# can.df <- data.frame(
#   rbind(
#     c("NW",  "9V", 586518, 7077103),
#     c("NE", "13W", 645544, 7118728),
#     c("SW", "11T", 680262, 4865141),
#     c("SE", "14T", 314095, 497555)),
#   stringsAsFactors = F)
# colnames(can.df) <- c("Corner", "Zone", "Northing", "Easting")
# ## make xy numeric
# num.cols <- c("Northing", "Easting")
# can.df[num.cols] <- sapply(can.df[num.cols], as.numeric)
# can.df["Zone"] <- as.character(can.df["Zone"])
# test <- st_as_sf(can.df,
#                  coords = c("Easting", "Northing", "Zone"),
#                  crs = 2955)
# #> Warning in lapply(x[coords], as.numeric): NAs introduced by coercion
# #> Error in st_as_sf.data.frame(can.df, coords = c("Easting", "Northing", : missing values in coordinates not allowed

# test <- st_as_sf(Total_Peaks,
#                  coords = c("lon", "lat"),
#                  crs = 4269)
# test
# 
# mapview(test, xcol = "lon", ycol = "lat", zcol = "Peak_No", crs = 4269, grid = FALSE)


#> Simple feature collection with 4 features and 2 fields
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: 497555 ymin: 314095 xmax: 7118728 ymax: 680262
#> epsg (SRID):    2955
#> proj4string:    +proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#>   Corner                         Zone               geometry
#> 1     NW c("9V", "13W", "11T", "14T") POINT (7077103 586518)
#> 2     NE c("9V", "13W", "11T", "14T") POINT (7118728 645544)
#> 3     SW c("9V", "13W", "11T", "14T") POINT (4865141 680262)
#> 4     SE c("9V", "13W", "11T", "14T")  POINT (497555 314095)





# ggplot(x, aes(x = lon, y = lat)) +
#   coord_quickmap() +
#   geom_line()



# sbux_sf <- st_as_sf(starbucksNC, coords = c("Longitude", "Latitude"),  crs = 4326)







