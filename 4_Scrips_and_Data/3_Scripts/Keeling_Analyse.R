
# Author:
# Juan Bettinelli,
# Script to be used in the Paper "Quantification of methane emissions in Hamburg using a network of FTIR spectrometers and an inverse modeling approach"
# Data from the Hamburg campaign 2021-2022.
# This Script is used for a Keeling analysed of the data collected in Hamburg Geomatikum in 2021.
# The script produces a timeline of the total Methan concentration

# Declare librarys used
library(pacman)
library(lubridate)
library(readr)
library(plyr)
library(tidyverse)
library(ggplot2)   
library(hexbin)
library(gridExtra)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 


# Set Working Directory
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")



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

# Calculate 1/Mole Fraction
TotalData$c13C <- 1/TotalData$X.CH4..13C
TotalData$c2H <- 1/TotalData$X.CH4..2H

# Remove Empty Cells
TotalData <- TotalData[!is.na(TotalData$UTC),]

# Save the Data
# write.csv(TotalData,
#           "4_Data/OutputData/CombineCH4Data(1.8.2021-17.09.2021).csv",
#           row.names = FALSE)



# ######### not needed ##########
# # Plot CH4 Concentration Timeline
# # Total CH4 Concentration Timeline (Without a Gap)
# Timeline_Total_CH4 <- ggplot(TotalData[!is.na(TotalData$X.CH4..13C), ], aes(x = UTC, y = X.CH4.)) +
#         geom_line() + 
#         labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Compleate Timeline) Stationary in-Situ Measurement") +
#         scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#         theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 13C Measurement
# Timeline_13C_CH4 <- ggplot(TotalData[!is.na(TotalData$X.CH4..13C), ], aes(x = UTC, y = X.CH4..13C)) +
#       geom_line() + 
#       labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 13C Timeline) Stationary in-Situ Measurement") +
#       scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#       theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 2H Measurement
# Timeline_2H_CH4 <- ggplot(TotalData[!is.na(TotalData$X.CH4..2H), ], aes(x = UTC, y = X.CH4..2H)) +
#       geom_line() + 
#       labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 2H Timeline) Stationary in-Situ Measurement") +
#       scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#       theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Combine The Three timelines in one Plot
# grid.arrange(Timeline_Total_CH4,Timeline_13C_CH4, Timeline_2H_CH4, nrow = 3)
# 
# 
# 
# 
# 
# # Total CH4 Concentration Timeline (With a Gap)
# #introduce a NA value to produce a gap
# TotalData_13C_CH4 <- TotalData[!is.na(TotalData$X.CH4..13C), ]
# test <- TotalData_13C_CH4[0, ]
# test[1, 1] <- as.POSIXct("2021-08-26 00:00:00")
# TotalData_13C_CH4 <- rbind(TotalData_13C_CH4, test)
# TotalData_13C_CH4 <- TotalData_13C_CH4[order(TotalData_13C_CH4$UTC),]
# 
# TotalData_2HC_CH4 <- TotalData[!is.na(TotalData$X.CH4..2H), ]
# test <- TotalData_2HC_CH4[0, ]
# test[1, 1] <- as.POSIXct("2021-08-26 00:00:00")
# TotalData_2HC_CH4 <- rbind(TotalData_2HC_CH4, test)
# TotalData_2HC_CH4 <- TotalData_2HC_CH4[order(TotalData_2HC_CH4$UTC),]
# 
# 
# Timeline_Total_CH4 <- ggplot(TotalData, aes(x = UTC, y = X.CH4.)) +
#   geom_line() + 
#   labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Compleate Timeline) Stationary in-Situ Measurement") +
#   scale_x_datetime(date_breaks = "10 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#   theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 13C Measurement
# Timeline_13C_CH4 <- ggplot(TotalData_13C_CH4, aes(x = UTC, y = X.CH4..13C)) +
#   geom_line() + 
#   labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 13C Timeline) Stationary in-Situ Measurement") +
#   scale_x_datetime(date_breaks = "10 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#   theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 2H Measurement
# Timeline_2H_CH4 <- ggplot(TotalData_2HC_CH4, aes(x = UTC, y = X.CH4..2H)) +
#   geom_line() + 
#   labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 2H Timeline) Stationary in-Situ Measurement") +
#   scale_x_datetime(date_breaks = "10 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#   theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Combine The Three timelines in one Plot
# grid.arrange(Timeline_Total_CH4,Timeline_13C_CH4, Timeline_2H_CH4, nrow = 3)
# 
# 





# Keeling Plot
q <- ggplot(TotalData, aes(x = c13C, y = d13C.VPDB)) +
        geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
        expand_limits(x = 0) +
        geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
        labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = " Keeling Plot, 12C, δ(13)C \n (mean = -59.2‰ ± 0.15‰ s.e)") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      
k <- ggplot(TotalData, aes(x = c2H, y = d2H.VPDB)) +
      expand_limits(x = 0) +
      geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
      geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
      labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = " Keeling Plot, 2H, δ(2)H \n (mean = -304.4‰ ± 1.5‰ s.e.)") +
      theme(axis.text.x=element_text(angle=60, hjust=1))
grid.arrange(q,k, ncol = 2)

# Combine The Three timelines in one Plot
# grid.arrange(q,Timeline_13C_CH4, nrow = 2)
# grid.arrange(k,Timeline_2H_CH4, nrow = 2)

# # not needed
# # Select Only dayes during campaign
# TotalData_c <- TotalData[1:1874,]

# Keeling Analyse for total champagne Time series
c13C_Line <- lm(d13C.VPDB ~ c13C, TotalData_c )
c13C_coef <- coef(summary(c13C_Line))[, "Estimate"]
c13C_se <- coef(summary(c13C_Line))[, "Std. Error"] 

c2H_Line <- lm(d2H.VPDB ~ c2H, TotalData_c )
c2H_coef <- coef(summary(c2H_Line))[, "Estimate"]
c2H_se <- coef(summary(c2H_Line))[, "Std. Error"] 







#Select the Data from Dataframe with CH4 Concentration
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

# Find the Peaks in the timeline
CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE)) # "[+]{1,}[0]{1,2}[-]{1,}" peakpat = NULL,

# Format the Peak Dataframe
names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]

# Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
# get all Coloum Names
Heads <- colnames(TotalData)
Heads <- Heads[-1]
Heads <- Heads[-16]
# Peaks......
p <- data.frame()
for(i in 1:nrow(CH4_Peaks)) {       # for-loop over rows
  u <- TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], ]
  p <- rbind(p,u)
}

p <- p[complete.cases(p[ , "X.CH4."]),]




# Keeling for just Peaks in a continuous series (Manualy sorted)
# The peaks:
# 1<- 69:72
# 2<- 326:332
# 3<- 448:455
# 4<- 567:575
# 5<- 626:639
# 6<- 775:778
# 7<- 805:808
# 8<- 1042:1046
# p <- TotalData[c(69:72, 326:332, 448:455, 567:575, 626:639, 775:778, 805:808, 1042:1046),]
p_c13C_Line <- lm(d13C.VPDB ~ c13C, p )
p_c13C_coef <- coef(summary(p_c13C_Line))[, "Estimate"]
p_c13C_se <- coef(summary(p_c13C_Line))[, "Std. Error"]

p_c2H_Line <- lm(d2H.VPDB ~ c2H, p )
p_c2H_coef <- coef(summary(p_c2H_Line))[, "Estimate"]
p_c2H_se <- coef(summary(p_c2H_Line))[, "Std. Error"] 



# # expluding peaks
# r <- data.frame()
# 
# for(i in 1:nrow(CH4_Peaks)) {       # for-loop over rows
#   a <- TotalData[TotalData$UTC <= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC >= CH4_Peaks[i,"UTC_Ending"], ] ####### not working correctly
#   r <- rbind(p,a)
# }

r <- subset(TotalData, UTC = p$UTC) ###### check if it works!!!!
r <- r[complete.cases(r[ , "X.CH4."]),]



# Total Time series excluding the peaks (manualy sorted)
# r <- TotalData_c[-c(69:72, 326:332, 448:455, 567:575, 626:639, 775:778, 805:808, 1042:1046),]
r_c13C_Line <- lm(d13C.VPDB ~ c13C, r )
r_c13C_coef <- coef(summary(r_c13C_Line))[, "Estimate"]
r_c13C_se <- coef(summary(r_c13C_Line))[, "Std. Error"] 
r_c2H_Line <- lm(d2H.VPDB ~ c2H, r )
r_c2H_coef <- coef(summary(r_c2H_Line))[, "Estimate"]
r_c2H_se <- coef(summary(r_c2H_Line))[, "Std. Error"] 

# Plot Keeling Plot Only Peaks
KP_13C_Peaks <- ggplot(p, aes(x = c13C, y = d13C.VPDB)) +
  geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = "(Only Peaks) Keeling Plot, 12C, δ(13)C \n (mean = -61.5‰ ± 0.2‰ s.e)") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

KP_2H_Peaks <- ggplot(p, aes(x = c2H, y = d2H.VPDB)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = "(Only Peaks) Keeling Plot, 2H, δ(2)H \n (mean =-319.6 ‰ ± 2.5‰ s.e.)") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

grid.arrange(KP_13C_Peaks,KP_2H_Peaks, ncol = 2)

# Plot Keeling Plot No Peaks
KP_13C_NoPeaks <- ggplot(r, aes(x = c13C, y = d13C.VPDB)) +
  geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = "(No Peaks) Keeling Plot, 12C, δ(13)C \n (mean = -55.6‰ ± 0.3‰ s.e)") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

KP_2H_NoPeaks <- ggplot(r, aes(x = c2H, y = d2H.VPDB)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = "(No Peaks) Keeling Plot, 2H, δ(2)H \n (mean = -291.0‰ ± 2.1‰ s.e.)") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

grid.arrange(KP_13C_NoPeaks,KP_2H_NoPeaks, ncol = 2)

##### Show Keely analyse Data in output Console ######
message("\n \nTotal timeseries: \n 12C, δ(13)C (mean = ", c13C_coef[[1]],"‰ ± ", c13C_se[[1]],"‰ s.e; n = 1)","\n 2H, δ(2)H  (mean =", c2H_coef[[1]],"‰ ±", c2H_se[[1]],"‰ s.e; n = 1)")
message("Just the peaks: \n 12C, δ(13)C (mean = ", p_c13C_coef[[1]],"‰ ± ", p_c13C_se[[1]],"‰ s.e; n = 1)", " \n 2H, δ(2)H  (mean =", p_c2H_coef[[1]],"‰ ±", p_c2H_se[[1]],"‰ s.e; n = 1)")
message("Excluding the peaks: \n 12C, δ(13)C (mean =", r_c13C_coef[[1]],"‰ ±", r_c13C_se[[1]],"‰ s.e; n = 1)","\n 2H, δ(2)H (mean =", r_c2H_coef[[1]],"‰ ±", r_c2H_se[[1]],"‰ s.e; n = 1)")
