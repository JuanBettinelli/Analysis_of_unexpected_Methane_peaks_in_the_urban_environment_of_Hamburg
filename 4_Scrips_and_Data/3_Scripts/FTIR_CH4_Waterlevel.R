# Script that plots methane concentration measured by a FTIR spretromenter and and water level data measured at S. Pauli.
# Author Juan Bettinelli
# Last change: 27.10.2023

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


#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automatically access the data.
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

# calling functions from diffret script
source("3_Scripts/Functions.R")
source("3_Scripts/CH4_Transportmodel.R")



#------------------------------------------------------------------------------------------------------------
# Plot for the 06.08.2021

StartTime <- as.POSIXct('2021-08-06 00:00:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2021-08-06 23:59:59', 
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

########### Read the CSV File #############

FTIRData <- import("4_Data/7_FTIR/HAM20210806test.csv")
FTIRData$year_day_hour <- as.POSIXct(as.character(FTIRData$year_day_hour), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")

FTIRData$me_xch4_sc <- as.numeric(FTIRData$me_xch4_sc)

FTIRData <- filter(FTIRData, FTIRData$year_day_hour > StartTime & FTIRData$year_day_hour < FinishTime, .preserve = FALSE)

FTIRData$year_day_hour <- as.POSIXct(FTIRData$year_day_hour, 
                            format = "%d-%m-%Y %H:%M:%S", 
                            tz = "utc")

FTIRData$V3 <- NULL
FTIRData$V4 <- NULL
FTIRData$V5 <- NULL
FTIRData$V6 <- NULL
FTIRData$V7 <- NULL

# Create Dataframe
TData <- data.frame()

# Mearge one by one, All data is merged by DateTime
TData <- merge( TotalData[ , c("UTC", "Water_Level")], FTIRData, 
                    by.x = "UTC",
                    by.y = "year_day_hour",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TData <- TData[!is.na(TData$UTC),]

png(file="4_Data/OutputData/Plots/15_FTIR/15_Basic_Plot_CH4_Wl_FTIR_20210806.png",
width=600, height=350)
par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
plot(TData$UTC, TData$Water_Level,
     type = "l",
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Elbe Water level [mm]",
     xlim = c(StartTime, FinishTime))

par(new = TRUE)
plot(FTIRData$year_day_hour, FTIRData$me_xch4_sc,
     main = "06.08.2021: Water level and FTIR CH4 concentation at Gematikum vs Time",
     type = "l",
     cex = 2,
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(c(StartTime, FinishTime)))

axis(side=4,
     col.axis="red",
     col="red")
mtext(expression("CH"[4]*" concentration [ppb]"),
      col="red",
      side=4,
      line=3)
dev.off() 
 



 
#------------------------------------------------------------------------------------------------------------
# Plot for the 11.08.2021
 
########### Read the CSV File #############
StartTime <- as.POSIXct('2021-08-11 00:00:00', 
                       format = "%Y-%m-%d %H:%M:%S", 
                       tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2021-08-11 23:59:59', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")


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

########### Read the CSV File #############

FTIRData <- import("4_Data/7_FTIR/HAM20210811test.csv")
FTIRData$year_day_hour <- as.POSIXct(as.character(FTIRData$year_day_hour), 
                                    format = "%Y-%m-%d %H:%M:%S", 
                                    tz = "UTC")

FTIRData$mc_xch4_sc <- as.numeric(FTIRData$mc_xch4_sc)

FTIRData <- filter(FTIRData, FTIRData$year_day_hour > StartTime & FTIRData$year_day_hour < FinishTime, .preserve = FALSE)

FTIRData$year_day_hour <- as.POSIXct(FTIRData$year_day_hour, 
                                    format = "%d-%m-%Y %H:%M:%S", 
                                    tz = "utc")

# Create Dataframe
TData <- data.frame()

# Mearge one by one, All data is merged by DateTime
TData <- merge( TotalData[ , c("UTC", "Water_Level")], FTIRData, 
               by.x = "UTC",
               by.y = "year_day_hour",
               all.x = TRUE,
               all.y = TRUE,
               sort = TRUE)
TData <- TData[!is.na(TData$UTC),]
 
# Plot the timeline
png(file="4_Data/OutputData/Plots/15_FTIR/15_Basic_Plot_CH4_Wl_FTIR_202108011.png",
   width=600, height=350)
par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
plot(TData$UTC, TData$Water_Level,
    type = "l",
    cex = 2,
    xlab = "Date/Time UTC",
    ylab = "Elbe Water level [mm]",
    xlim = c(StartTime, FinishTime))

par(new = TRUE)
plot(FTIRData$year_day_hour, FTIRData$mc_xch4_sc,
    main = "11.08.2021: Water level and FTIR CH4 concentation at Gematikum vs Time",
    type = "l",
    cex = 2,
    col="red",
    axes = FALSE,
    bty = "n",
    xlab = "",
    ylab = "",
    xlim = c(c(StartTime, FinishTime)))

axis(side=4,
    col.axis="red",
    col="red")
mtext(expression("CH"[4]*" concentration [ppb]"),
     col="red",
     side=4,
     line=3)
dev.off() 
 
 
 
#------------------------------------------------------------------------------------------------------------
# Plot for the 31.08.2021
 
########### Read the CSV File #############
StartTime <- as.POSIXct('2021-08-31 00:00:00', 
                       format = "%Y-%m-%d %H:%M:%S", 
                       tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2021-08-31 23:59:59', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")

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

########### Read the CSV File #############

FTIRData <- import("4_Data/7_FTIR/HAM20210831test.csv")
FTIRData$year_day_hour <- as.POSIXct(as.character(FTIRData$year_day_hour), 
                                    format = "%Y-%m-%d %H:%M:%S", 
                                    tz = "UTC")

FTIRData$mc_xch4_sc <- as.numeric(FTIRData$mc_xch4_sc)
FTIRData$md_xch4_sc <- as.numeric(FTIRData$md_xch4_sc)

FTIRData <- filter(FTIRData, FTIRData$year_day_hour > StartTime & FTIRData$year_day_hour < FinishTime, .preserve = FALSE)

FTIRData$year_day_hour <- as.POSIXct(FTIRData$year_day_hour, 
                                    format = "%d-%m-%Y %H:%M:%S", 
                                    tz = "utc")




# Create Dataframe
TData <- data.frame()

# Mearge one by one, All data is merged by DateTime
TData <- merge( TotalData[ , c("UTC", "Water_Level")], FTIRData, 
               by.x = "UTC",
               by.y = "year_day_hour",
               all.x = TRUE,
               all.y = TRUE,
               sort = TRUE)
TData <- TData[!is.na(TData$UTC),]

# Plot the timeline
png(file="4_Data/OutputData/Plots/15_FTIR/15_Basic_Plot_CH4_Wl_FTIR_202108031_mc.png",
   width=600, height=350)
par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
plot(TData$UTC, TData$Water_Level,
    type = "l",
    cex = 2,
    xlab = "Date/Time UTC",
    ylab = "Elbe Water level [mm]",
    xlim = c(StartTime, FinishTime))

par(new = TRUE)
plot(FTIRData$year_day_hour, FTIRData$mc_xch4_sc,
    main = "31.08.2021: Water level and FTIR CH4 concentation at Jork vs Time",
    type = "l",
    cex = 2,
    col="red",
    axes = FALSE,
    bty = "n",
    xlab = "",
    ylab = "",
    xlim = c(c(StartTime, FinishTime)))

axis(side=4,
    col.axis="red",
    col="red")
mtext(expression("CH"[4]*" concentration [ppb]"),
     col="red",
     side=4,
     line=3)
dev.off() 

png(file="4_Data/OutputData/Plots/15_FTIR/15_Basic_Plot_CH4_Wl_FTIR_202108031_md.png",
   width=600, height=350)
par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
plot(TData$UTC, TData$Water_Level,
    type = "l",
    cex = 2,
    xlab = "Date/Time UTC",
    ylab = "Elbe Water level [mm]",
    xlim = c(StartTime, FinishTime))

par(new = TRUE)
plot(FTIRData$year_day_hour, FTIRData$md_xch4_sc,
    main = "31.08.2021: Water level and FTIR CH4 concentation at Rosengarten vs Time",
    type = "l",
    cex = 2,
    col="red",
    axes = FALSE,
    bty = "n",
    xlab = "",
    ylab = "",
    xlim = c(c(StartTime, FinishTime)))

axis(side=4,
    col.axis="red",
    col="red")
mtext(expression("CH"[4]*" concentration [ppb]"),
     col="red",
     side=4,
     line=3)
dev.off() 


