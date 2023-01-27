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

FinishTime <- as.POSIXct('2021-09-06 00:00:00', 
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



########### Plot CH4 Concentration Timeseries ##############
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]

CH4_TimeLine <- ggplot(CH4Data, aes(x = UTC, y = X.CH4.)) +
  geom_line() +
  labs(x = "Fill Time [UTC]", y ="CH4 mole fraction [ppb]", title = "CH4 mole fraction vs. Time") +
  scale_x_datetime(date_breaks = "20 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
  theme(axis.text.x=element_text(angle=60, hjust=1))
CH4_TimeLine



########## ?????????????? #########
# cor(TotalData$WindSpeed,TotalData$WindDirction)

######## Plot CH4/Water level#############


###Basic Plot #####
# par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
# plot(TotalData_CH4$UTC, TotalData_CH4$Water_Level,
#      type = "l",
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Elbe Waterlevel, mm",
#      xlim = c(StartTime, FinishTime))
# 
# par(new = TRUE)
# plot(TotalData_CH4$UTC, TotalData_CH4$X.CH4.,
#      main = "WaterLevel(WSV)/CH4 Concentation Vs. Time",
#      type = "l",
#      cex = 2,
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

TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]

n <- 4
TotalData_CH4_WL <- TotalData_CH4_WL %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData_CH4_WL))*n))

CH4_TimeLine <- ggplot(TotalData_CH4_WL) +
  geom_line(aes(x = UTC,
                y = X.CH4.),
            col = "red") +
  labs(x = "Fill Time [UTC]",
       y ="CH4 Concentration [ppb]",
       title = "CH4 Concentration & Elbe Waterlevel vs. Time") +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.title.y = element_text(color = "red",
                                    size=13),
        axis.text.y = element_text(color = "red"),
        axis.title.y.right = element_text(color = "blue",
                                          size=13),
        axis.text.y.right = element_text(color = "blue"),
        strip.text.x = element_blank()) +
  geom_line(aes(x = UTC,
                y = Water_Level*5),
            col = "blue") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./5,
                                         name="Water Level, mm"))+
  facet_wrap(~panel, scales = 'free', nrow = n)
CH4_TimeLine

ggsave("CH4_WL.png", CH4_TimeLine, path = "4_Data/OutputData", width = 10, height = 5)




TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Wind_Direction", "Wind_Speed")]),]
n <- 4
TotalData_Wind <- TotalData_Wind %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData_Wind))*n))


Wind_TimeLine <- ggplot(TotalData_Wind) +
  geom_line(aes(x = UTC,
                y = Wind_Direction),
            col = "black") +
  labs(x = "Fill Time [UTC]",
       y ="Wind Direction, °",
       title = "Wind Speed & Wind Direction vs. Time") +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%d-%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.title.y = element_text(color = "black",
                                    size=13),
        axis.text.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "green",
                                          size=13),
        axis.text.y.right = element_text(color = "green"),
        strip.text.x = element_blank()) +
  geom_line(aes(x = UTC,
                y = Wind_Speed*70),
            col = "green") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./70,
                                         name="Wind Speed, m/s"))+
  facet_wrap(~panel, scales = 'free', nrow = n)
Wind_TimeLine

ggsave("Wind_D_S.png", Wind_TimeLine, path = "4_Data/OutputData", width = 10, height = 5)



# TotalData_CH4 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
# TotalData_CH4 <- TotalData_CH4[,c("UTC", "X.CH4.", "Water_Level")]
# WL_CH4_Data <- melt(TotalData_CH4, id.var="UTC")



######## Plot Wind Direction (DWD)/Speed/CH4#############

par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Wind_Direction,
     main = "Wind Direction & Speed (DWD)/CH4 Concentation Vs. Time",
     type = "p",
     pch='.',
     cex = 5,
     xlab = "Date/Time UTC",
     ylab = "Wind Direction, °",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Wind_Speed,
     type = "p",
     pch='.',
     cex = 5,
     xlab = "Date/Time UTC",
     ylab = "Wind Speed, m/s",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

par(mfrow=c(1,1))

######## Plot Wind Direction (Mast 110m)/Speed/CH4#############

par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Direction110m,
     main = "Wind Direction & Speed (MAST 110m)/CH4 Concentation Vs. Time",
     type = "p",
     pch='.',
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Wind Direction, °",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Speed110m,
     type = "p",
     pch='.',
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Wind Speed, m/s",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

par(mfrow=c(1,1))


######## Plot Wind Direction (Geomatikum)/CH4#############

par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Direction,
     main = "Wind Direction (Geomatikum)/CH4 Concentation Vs. Time",
     type = "p",
     pch='.',
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Wind Direction, °",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)


par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Speed,
     type = "p",
     pch='.',
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Wind Speed, m/s",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

par(mfrow=c(1,1))


######## Plot Rain/CH4#############
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$precipitation_height,
     main = "Rain quantity (DWD)/CH4 Concentation Vs. Time",
     type = "p",
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Rain Quantity, mm",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

######## Plot Temp/CH4#############
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$temperature_air_mean_200,
     main = "Temperature (DWD)/CH4 Concentation Vs. Time",
     type = "p",
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Temperature, °C",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "p",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

######## Plot Humid/CH4#############
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$humidity,
     main = "Relative humidity (DWD)/CH4 Concentation Vs. Time",
     type = "p",
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Relative humidity, g/m3",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     type = "l",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

######## Plot CH4/Waterlevel/ Winddierction#############
par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
# first plot
plot(TotalData$UTC, TotalData$Water_Level,
     type = "p",
     pch='.',
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Elbe Waterlevel, mm",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$UTC, TotalData$X.CH4.,
     main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
     type = "p",
     lwd = 1.5,
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")


par(new = TRUE)
plot(TotalData$UTC, TotalData$Wind_Direction,
     type = "p",
     pch='.',
     cex = 2,
     col="Blue",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
axis(side=4,
     col.axis="blue",
     col="blue")
mtext("CH4 Concentration & Wind Direction, °",
      col="red",
      side=4,
      line=3)




######## Plot CH4/Water level/ Wind direction, Split into 10 Day intervals#############
IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")

ticks <- seq(from=min(TotalData$UTC), by='1 days', length=50)

par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
for(j in IntervalDate){
  # first plot
  plot(TotalData$UTC, TotalData$Water_Level,
       type = "p",
       pch='.',
       cex = 1,
       # lwd = 1,
       bty = "n",
       axes = FALSE,
       # xlab = "Date/Time UTC",
       # ylab = "Elbe Waterlevel, mm",
       xlim = c(i,j))
  axis(side=2,
       col.axis="Black",
       col="Black",
       las = 1,
       ylab = "Elbe Waterlevel, mm")

  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       # main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
       type = "p",
       pch='.',
       cex = 3,
       col="red",
       axes = FALSE,
       xlab = "",
       ylab = "",
       xlim = c(i,j))

  axis(side=4,
       col.axis="red",
       col="red",
       las = 1)

  axis.POSIXct(side=1,
               at = ticks,
               #labels=format(TotalData$UTC,"%Y-%m-%d"),
               las=2)

  par(new = TRUE)
  plot(TotalData$UTC, TotalData$Wind_Direction,
       type = "p",
       pch='.',
       cex = 3,
       col="Blue",
       axes = FALSE,
       xlab = "",
       ylab = "",
       xlim = c(i,j))
  axis(side=4,
       col.axis="blue",
       col="blue",
       las = 1)
  mtext("CH4 Concentration & Wind Direction, °",
        col="red",
        side=4,
        line=3)
  mtext("WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time", side = 3, line = - 2, outer = TRUE)
  i <- j
}

######## Plot CH4/Water level/ Wind Speed, Split into 10 Day intervals#############
IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")

ticks <- seq(from=min(TotalData$UTC), by='1 days', length=50)


for(j in IntervalDate){
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Water_Level,
       type = "p",
       pch='.',
       cex = 2,
       # lwd = 1,
       bty = "n",
       axes = FALSE,
       # xlab = "Date/Time UTC",
       # ylab = "Elbe Waterlevel, mm",
       xlim = c(i,j))
  axis(side=2,
       col.axis="Black",
       col="Black",
       ylab = "Elbe Waterlevel, mm")

  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       main = "WaterLevel(WSV)/CH4 Concentation/Wind Speed (DWD) Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       # lwd = 1.5,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(i,j))

  axis(side=4,
       col.axis="red",
       col="red")
  axis.POSIXct(side=1,
               at = ticks,
               #labels=format(TotalData$UTC,"%Y-%m-%d"),
               las=2)


  par(new = TRUE)
  plot(TotalData$UTC, TotalData$Wind_Speed,
       type = "p",
       pch='.',
       cex = 2,
       col="Blue",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(i,j))
  axis(side=4,
       col.axis="blue",
       col="blue")
  mtext("CH4 Concentration & Wind Speed",
        col="red",
        side=4,
        line=3)

  i <- j
}


