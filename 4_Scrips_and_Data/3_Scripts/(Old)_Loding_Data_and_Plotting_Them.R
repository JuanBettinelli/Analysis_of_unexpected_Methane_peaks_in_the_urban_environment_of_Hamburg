library(pacman)
library(lubridate)
library(readr)
library("plyr")
library(tidyverse)
library("ggplot2")   
library("hexbin")
# install.packages('bit64')

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 


#Set Working Directory
#setwd("~/Documents/Isotrope Plots")
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


StartTime <- as.POSIXct('2021-08-01 00:00:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")

FinishTime <- as.POSIXct('2022-03-29 00:00:00', 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz ="utc")

######################### Water Level Complete Data ############################

# StPauli <- import("WaterlevelCompleate.csv")
# StPauli$V1 <- as.POSIXlt(as.character(StPauli$V1), 
#                                  format = "%Y%m%d%H%M%S", 
#                                  tz = "CET")

WSV_Waterlevel <- import("4_Data/4_Waterlevel/Water_Level_(20210701-20220505).csv")
colnames(WSV_Waterlevel) <- c("CET", "Water_Level")
WSV_Waterlevel$CET <- as.POSIXct(as.character(WSV_Waterlevel$CET), tz = "Etc/GMT-1",  format = "%Y%m%d%H%M%S")
WSV_Waterlevel$UTC <- with_tz(WSV_Waterlevel$CET, tzone = "UTC",  format = "%d-%m-%Y %H:%M:%S")

WSV_Waterlevel <- filter(WSV_Waterlevel, WSV_Waterlevel$UTC > StartTime & WSV_Waterlevel$UTC < FinishTime, .preserve = FALSE)

WSV_Waterlevel$CET      <- NULL

######################### Geomatikum Data ############################
#Loade the data from the csv file
Geomatikum_csv <- import("4_Data/1_Universität_Hamburg_Wind_Data/WindDataGeomatikumTotalTimeline.csv")

#Convert the datetime to the correct format
Geomatikum_csv$UTC <- as.POSIXct(as.character(Geomatikum_csv$UTC), format = "%d-%m-%Y %H:%M:%S", tz = "UTC")

Geomatikum_csv <- filter(Geomatikum_csv, Geomatikum_csv$UTC > StartTime & Geomatikum_csv$UTC < FinishTime, .preserve = FALSE)

######################## Mast Data ###################################
Mast_csv <- import("4_Data/1_Universität_Hamburg_Wind_Data/WindDataMastTotalTimeline.csv")

Mast_csv$UTC <- as.POSIXct(as.character(Mast_csv$UTC), format = "%d-%m-%Y %H:%M:%S", tz = "utc")

Mast_csv <- filter(Mast_csv, Mast_csv$UTC > StartTime & Mast_csv$UTC < FinishTime, .preserve = FALSE)


########################## DWD Data ##########################################

# # Load all the Met data into the script
# Wind_txt <- import("DWDMetroData/stundenwerte_FF_01975_akt (1)/produkt_ff_stunde_20200318_20210918_01975.txt")
# Niederschlag_txt <- import("DWDMetroData/stundenwerte_RR_01975_akt (1)/produkt_rr_stunde_20200318_20210918_01975.txt")
# TempFeuch_txt <- import("DWDMetroData/stundenwerte_TU_01975_akt (1)/produkt_tu_stunde_20200318_20210918_01975.txt")
# Erd_txt <- import("DWDMetroData/stundenwerte_EB_01975_akt (1)/produkt_eb_stunde_20200318_20210918_01975.txt")
# 
# #Select only the relevant data
# Wind_New <- Wind_txt[(Wind_txt$MESS_DATUM > 2021072600),]
# Niederschlag_New <- Niederschlag_txt[(Niederschlag_txt$MESS_DATUM > 2021072600),]
# TempFeuch_New <- TempFeuch_txt[(TempFeuch_txt$MESS_DATUM > 2021072600),]
# Erd_New <- Erd_txt[(Erd_txt$MESS_DATUM > 2021072600),]
# 
# #Convert the TimeDate in DataFrame
# Wind_New$UTCDateTime <- as.POSIXlt(as.character(Wind_New$MESS_DATUM), 
#                                    format = "%Y%m%d%H", 
#                                    tz = "UTC")
# 
# Niederschlag_New$UTCDateTime <- as.POSIXlt(as.character(Niederschlag_New$MESS_DATUM), 
#                                            format = "%Y%m%d%H", 
#                                            tz = "UTC")
# 
# TempFeuch_New$UTCDateTime <- as.POSIXlt(as.character(TempFeuch_New$MESS_DATUM), 
#                                         format = "%Y%m%d%H", 
#                                         tz = "UTC")
# Erd_New$UTCDateTime <- as.POSIXlt(as.character(Erd_New$MESS_DATUM), 
#                                   format = "%Y%m%d%H", 
#                                   tz = "UTC")

DWD_Data_10min <- import("4_Data/OutputData/DWDMeteorologicalData_10min.csv")
DWD_Data_1h <- import("4_Data/OutputData/DWDMeteorologicalData_1h.csv")

DWD_Data_10min$UTCDateTime <- as.POSIXct(as.character(DWD_Data_10min$UTCDateTime), format = "%Y-%m-%d %H:%M:%S", tz = "utc")
DWD_Data_1h$UTCDateTime <- as.POSIXct(as.character(DWD_Data_1h$UTCDateTime), format = "%Y-%m-%d %H:%M:%S", tz = "utc")

DWD_Data_10min <- filter(DWD_Data_10min, DWD_Data_10min$UTCDateTime > StartTime & DWD_Data_10min$UTCDateTime < FinishTime, .preserve = FALSE)
DWD_Data_1h <- filter(DWD_Data_1h, DWD_Data_1h$UTCDateTime > StartTime & DWD_Data_1h$UTCDateTime < FinishTime, .preserve = FALSE)



###########################Water Level data WSV#############################
# mydir = "Waterlevel1"
# myfiles = list.files(path=mydir, 
#                      pattern="*.csv", 
#                      full.names=TRUE)
# 
# WLTotal <- data.frame(stringsAsFactors=FALSE) 
# 
# 
# for (f in myfiles){
#   
#   #Read Concentration Data of the CSV Files
#   WL_csv<- import(f, ";", escape_double = FALSE, trim_ws = TRUE)
#   
#   WL_csv$UTC <- as.POSIXlt(WL_csv$UTC , 
#                            format="%d.%m.%y %H:%M", 
#                            tz = "UTC")
#   #WL_csv$UTCTimeDate <- as.character(WL_csv$UTC , format="%Y%m%d%H%M")
#   
#   WLTotal <- rbind(WLTotal, WL_csv)
# }


####################Isotrope Data###################

#Read Concentration Data of the CSV Files
# CH4_2H <-read.csv2("CH4 2H 20210816.csv",TRUE, ";")
# CH4_13C <-read.csv2("CH4 13C 20210816.csv",TRUE, ";")
# CH4_concentrations <-read.csv2("CH4 concentrations 20210816.csv",TRUE, ";")


#Convert the date into a readable format
# CH4_con_w_d <- CH4_concentrations
# CH4_con_w_d$fill.time.utc <- as.POSIXlt(CH4_con_w_d$fill.time.utc,
#                                         format = "%d.%m.%y %H:%M", 
#                                         tz = "utc")
# CH4_2H_w_d <- CH4_2H
# CH4_2H_w_d$fill.time.utc <- as.POSIXlt(CH4_2H_w_d$fill.time.utc,
#                                        format = "%d.%m.%y %H:%M", 
#                                        tz = "utc")
# CH4_13C_w_d <- CH4_13C
# CH4_13C_w_d$fill.time.utc <- as.POSIXlt(CH4_13C_w_d$fill.time.utc,
#                                         format = "%d.%m.%y %H:%M", 
#                                         tz = "utc")

CH4_concentrations <-read.csv2("4_Data/2_Geomatikum_CH4_Concentrations/3_CH4Concentration(1.8.2021-28.3.2022)/Hamburg Methan Measuments 01082021 - 28032022.csv")
CH4_concentrations$fill.time.utc <- as.POSIXct(as.character(CH4_concentrations$fill.time.utc), format = "%d.%m.%y %H:%M", tz = "utc")
CH4_concentrations$fill.time.utc <- as.POSIXct(CH4_concentrations$fill.time.utc, format = "%d-%m-%Y %H:%M:%S", tz = "utc")
CH4_concentrations$fill.time.utc.1 <- as.POSIXct(as.character(CH4_concentrations$fill.time.utc.1), format = "%d.%m.%y %H:%M", tz = "utc")
CH4_concentrations$fill.time.utc.1 <- as.POSIXct(CH4_concentrations$fill.time.utc.1, format = "%d-%m-%Y %H:%M:%S", tz = "utc")
CH4_concentrations$fill.time.utc.2 <- as.POSIXct(as.character(CH4_concentrations$fill.time.utc.2), format = "%d.%m.%y %H:%M", tz = "utc")
CH4_concentrations$fill.time.utc.2 <- as.POSIXct(CH4_concentrations$fill.time.utc.2, format = "%d-%m-%Y %H:%M:%S", tz = "utc")

CH4_concentrations$X      <- NULL
CH4_concentrations$X.1      <- NULL


################### Plotting the Data #######################


TotalData <- data.frame()


TotalData <- merge( Geomatikum_csv, Mast_csv, 
                    by.x = "UTC",
                    by.y = "UTC",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

TotalData <- merge( TotalData, WSV_Waterlevel[ , c("UTC", "Water_Level")], 
                    by.x = "UTC",
                    by.y = "UTC",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

TotalData <- merge( TotalData, CH4_concentrations[ , c("fill.time.utc", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C")], 
                    by.x = "UTC",
                    by.y = "fill.time.utc",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

TotalData <- merge( TotalData, CH4_concentrations[ , c("fill.time.utc.1", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H")], 
                    by.x = "UTC",
                    by.y = "fill.time.utc.1",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

TotalData <- merge( TotalData, CH4_concentrations[ , c("fill.time.utc.2", "X.CH4.")], 
                    by.x = "UTC",
                    by.y = "fill.time.utc.2",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

TotalData <- merge( TotalData, DWD_Data_10min, 
                    by.x = "UTC",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)



# TotalData <- merge( TotalData, Niederschlag_New, 
#                     by.x = "UTC",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# 
# TotalData <- merge( TotalData, TempFeuch_New, 
#                     by.x = "UTC",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# 
# TotalData <- merge( TotalData, Geomatikum_csv, 
#                     by.x = "UTC",
#                     by.y = "UTC",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# 
# TotalData <- merge( TotalData, Mast_csv, 
#                     by.x = "UTC",
#                     by.y = "UTC",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)



################## Filter the Data ##################


TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)

# TotalData$MEZ_Date      <- NULL
# TotalData$Mez_Time      <- NULL
# TotalData$MEZ           <- NULL
# TotalData$STATIONS_ID   <- NULL
# TotalData$STATIONS_ID.x <- NULL
# TotalData$STATIONS_ID.y <- NULL
# TotalData$MEZ           <- NULL
# TotalData$MESS_DATUM.x  <- NULL
# TotalData$MESS_DATUM.y  <- NULL
# TotalData$MESS_DATUM    <- NULL
# TotalData$eor.x         <- NULL
# TotalData$eor.y         <- NULL
# TotalData$eor           <- NULL
# TotalData$QN_3          <- NULL
# TotalData$QN_8          <- NULL
# TotalData$QN_9          <- NULL

# colnames(TotalData)[2]  <- "Water_Level"
TotalData$Water_Level <- as.numeric(TotalData$Water_Level)

# colnames(TotalData)[3]  <- "CH4"
TotalData$X.CH4..13C <- as.numeric(TotalData$X.CH4..13C)

# colnames(TotalData)[4]  <- "WindSpeed"
TotalData$sd..CH4. <- as.numeric(TotalData$sd..CH4.)

# colnames(TotalData)[5]  <- "WindDirction"
TotalData$sd.d13C <- as.numeric(TotalData$sd.d13C)

# colnames(TotalData)[6]  <- "RainQuantity"
TotalData$X.CH4..2H <- as.numeric(TotalData$X.CH4..2H)

# colnames(TotalData)[7]  <- "RainPresent"
TotalData$sd..CH4..1 <- as.numeric(TotalData$sd..CH4..1)

# colnames(TotalData)[8]  <- "RainType"
TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)

# # colnames(TotalData)[9]  <- "Temperature"
# TotalData$Temperature <- as.numeric(TotalData$Temperature)
# 
# # colnames(TotalData)[10]  <- "Humidity"
# TotalData$Humidity <- as.numeric(TotalData$Humidity)
# 
# # colnames(TotalData)[11]  <- "GeoWindDir"
# TotalData$GeoWindDir <- as.numeric(TotalData$GeoWindDir)
# 
# # colnames(TotalData)[12]  <- "GeoWindSpeed"
# TotalData$GeoWindSpeed <- as.numeric(TotalData$GeoWindSpeed)
# 
# # colnames(TotalData)[13]  <- "Mast50mDir"
# TotalData$Mast50mDir <- as.numeric(TotalData$Mast50mDir)
# 
# # colnames(TotalData)[14]  <- "Mast50mSpeed"
# TotalData$Mast50mSpeed <- as.numeric(TotalData$Mast50mSpeed)
# 
# # colnames(TotalData)[15]  <- "Mast110mDir"
# TotalData$Mast110mDir <- as.numeric(TotalData$Mast110mDir)
# 
# # colnames(TotalData)[16]  <- "Mast110mSpeed"
# TotalData$Mast110mSpeed <- as.numeric(TotalData$Mast110mSpeed)


# plot(TotalData$UTC,TotalData$X.CH4.)

# write.csv(TotalData,"4_Data/OutputData/CombineMeteorologicalData.csv", row.names = FALSE)

########## ?????????????? #########
# cor(TotalData$WindSpeed,TotalData$WindDirction)

# ######## Plot CH4/Water level#############
# par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
# # first plot
# plot(TotalData$UTC, TotalData$Water_Level,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Elbe Waterlevel, mm",
#      xlim = c(StartTime, FinishTime))
# 
# par(new = TRUE)
# plot(TotalData$UTC, TotalData$X.CH4.,
#      main = "WaterLevel(WSV)/CH4 Concentation Vs. Time",
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(c(StartTime, FinishTime)))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)

# ######## Plot Wind Direction (DWD)/Speed/CH4#############
# 
# par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$WindDirction,
#      main = "Wind Direction & Speed (DWD)/CH4 Concentation Vs. Time",
#      type = "p",
#      pch='.',
#      cex = 5,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Direction, °",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$WindSpeed,
#      type = "p",
#      pch='.',
#      cex = 5,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Speed, m/s",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mfrow=c(1,1))

# ######## Plot Wind Direction (Mast 110m)/Speed/CH4#############
# 
# par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$Mast110mDir,
#      main = "Wind Direction & Speed (MAST 110m)/CH4 Concentation Vs. Time",
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Direction, °",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$Mast110mSpeed,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Speed, m/s",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mfrow=c(1,1))
# 

# ######## Plot Wind Direction (Geomatikum)/CH4#############
# par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$GeoWindDir,
#      main = "Wind Direction (Geomatikum)/CH4 Concentation Vs. Time",
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Direction, °",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# 
# par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$GeoWindSpeed,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Speed, m/s",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mfrow=c(1,1))


# ######## Plot Rain/CH4#############
# par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$RainQ,
#      main = "Rain quantity (DWD)/CH4 Concentation Vs. Time",
#      type = "l",
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Rain Quantity, mm",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# ######## Plot Temp/CH4#############
# par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$Temp,
#      main = "Temperature (DWD)/CH4 Concentation Vs. Time",
#      type = "l",
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Temperature, °C",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# ######## Plot Humid/CH4#############
# par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$Humid,
#      main = "Relative humidity (DWD)/CH4 Concentation Vs. Time",
#      type = "l",
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Relative humidity, g/m3",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)


# ######## Plot CH4/Waterlevel/ Winddierction#############
# par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$level,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Elbe Waterlevel, mm",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
#      type = "l",
#      lwd = 1.5,
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# 
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$WindDirction,
#      type = "p",
#      pch='.',
#      cex = 2,
#      col="Blue",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# axis(side=4,
#      col.axis="blue",
#      col="blue") 
# mtext("CH4 Concentration & Wind Direction, °",
#       col="red",
#       side=4,
#       line=3)




# ######## Plot CH4/Water level/ Wind direction, Split into 10 Day intervals#############
# IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
# i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
# 
# ticks <- seq(from=min(TotalData$UTC), by='1 days', length=50)
# 
# for(j in IntervalDate){
#   par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
#   # first plot
#   plot(TotalData$UTC, TotalData$WaterLevel,
#        type = "l",
#        #pch='.',
#        lwd = 1,
#        bty = "n",
#        axes = FALSE,
#        # xlab = "Date/Time UTC",
#        # ylab = "Elbe Waterlevel, mm",
#        xlim = c(i,j))
#   axis(side=2,
#        col.axis="Black",
#        col="Black",
#        ylab = "Elbe Waterlevel, mm")
# 
#   par(new = TRUE)
#   plot(TotalData$UTC, TotalData$CH4,
#        main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
#        type = "p",
#        pch='.',
#        cex = 2,
#        col="red",
#        axes = FALSE,
#        xlab = "",
#        ylab = "",
#        xlim = c(i,j))
# 
#   axis(side=4,
#        col.axis="red",
#        col="red")
#   
#   axis.POSIXct(side=1,
#                at = ticks,
#                #labels=format(TotalData$UTC,"%Y-%m-%d"),
#                las=2)
# 
#   par(new = TRUE)
#   plot(TotalData$UTC, TotalData$WindDirction,
#        type = "p",
#        pch='.',
#        cex = 2,
#        col="Blue",
#        axes = FALSE,
#        xlab = "",
#        ylab = "",
#        xlim = c(i,j))
#   axis(side=4,
#        col.axis="blue",
#        col="blue")
#   mtext("CH4 Concentration & Wind Direction, °",
#         col="red",
#         side=4,
#         line=3)
# 
#   i <- j
# }

# ######## Plot CH4/Water level/ Wind Speed, Split into 10 Day intervals#############
# IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
# i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
# 
# for(j in IntervalDate){
#   par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
#   # first plot
#   plot(TotalData$datetime, TotalData$level,
#        type = "p",
#        pch='.',
#        cex = 2,
#        xlab = "Date/Time UTC",
#        ylab = "Elbe Waterlevel, mm",
#        xlim = c(i,j))
#   
#   par(new = TRUE)
#   plot(TotalData$datetime, TotalData$ch4,
#        main = "WaterLevel(WSV)/CH4 Concentation/Wind Speed (DWD) Vs. Time",
#        type = "l",
#        lwd = 1.5,
#        col="red",
#        axes = FALSE,
#        bty = "n",
#        xlab = "",
#        ylab = "",
#        xlim = c(i,j))
#   
#   axis(side=4,
#        col.axis="red",
#        col="red")
#   
#   
#   par(new = TRUE)
#   plot(TotalData$datetime, TotalData$WindSpeed,
#        type = "p",
#        pch='.',
#        cex = 4,
#        col="Blue",
#        axes = FALSE,
#        bty = "n",
#        xlab = "",
#        ylab = "",
#        xlim = c(i,j))
#   axis(side=4,
#        col.axis="blue",
#        col="blue") 
#   mtext("CH4 Concentration & Wind Speed",
#         col="red",
#         side=4,
#         line=3)
#   
#   i <- j
# }
# 
# 
