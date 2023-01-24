# Script to Plot the Data From the "CombineMeteorologicalData.csv" created by the script "Combine_All_Data_To_CSV_File.R"
# Author Juan Bettinelli
# Last change: 19.1.23

library(pacman)
library(lubridate)
library(readr)
library(plyr)
library(tidyverse)
library(ggplot2)   
library(hexbin)
library(gridExtra)
library(reshape2)

StartTime <- as.POSIXct('2021-08-01 22:00:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
FinishTime <- as.POSIXct('2022-03-29 00:00:00', 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz ="utc")


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


########### Plot CH4 Concentration Timeseries ##############
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]

CH4_TimeLine <- ggplot(CH4Data, aes(x = UTC, y = X.CH4.)) +
  geom_line() +
  labs(x = "Fill Time [UTC]", y ="CH4 mole fraction [ppb]", title = "CH4 mole fraction vs. Time") +
  scale_x_datetime(date_breaks = "20 day", date_labels = "%d-%m-%Y", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
  theme(axis.text.x=element_text(angle=60, hjust=1))
CH4_TimeLine



########## ?????????????? #########
# cor(TotalData$WindSpeed,TotalData$WindDirction)

######## Plot CH4/Water level#############
TotalData_CH4 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
TotalData_CH4 <- TotalData_CH4[,c("UTC", "X.CH4.", "Water_Level")]
WL_CH4_Data <- melt(TotalData_CH4, id.var="UTC")


par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Water_Level,
     type = "p",
     pch='.',
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Elbe Waterlevel, mm",
     xlim = c(StartTime, FinishTime))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     main = "WaterLevel(WSV)/CH4 Concentation Vs. Time",
     type = "p",
     pch='.',
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
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

CH4_WL_TimeLine <- ggplot(data=WL_CH4_Data, aes(UTC, value,colour=variable)) +
  geom_line() +
  scale_colour_manual(values=c("red","blue"))+
  labs(x="games",y="variance")
  # geom_line() +
  # labs(x = "Fill Time [UTC]", y ="Elbe Waterlevel, mm", title = "Elbe Waterlevel, mm vs. Time") +
  # scale_x_datetime(date_breaks = "20 day", date_labels = "%d-%m-%Y", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
  # theme(axis.text.x=element_text(angle=60, hjust=1))
CH4_WL_TimeLine
# grid.arrange(CH4_TimeLine, CH4_WL_TimeLine)


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
