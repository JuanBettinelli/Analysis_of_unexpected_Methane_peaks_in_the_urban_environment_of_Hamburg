
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
library(dplyr)
library(GGally)
library(ggthemes)
library(ggvis)
library(httr)
library(lubridate)
library(plotly)
library(rio)
library(rmarkdown)
library(shiny)
library(stringr)
library(tidyr)
library(gridExtra)
library(grid)
library(PlaneGeometry)


##### Characterisation of methane sources in Lutjewad, The Netherlands, using quasi-continuous isotopic composition measurements

# Extraction and distribution of fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
FF <- Ellipse$new(center = c(-40.0, -198.5), rmajor = 17.75, rminor = 12, alpha = 0)
# ellipse as a path
FFpath <- FF$path()
# the path is not closed; close it
FFpath <- rbind(FFpath, FFpath[1,])

# Agriculture 12C 68.0 [70.6; 46.0] 2H 319 [361; 295]
AG <- Ellipse$new(center = c(-68.0, -319), rmajor = 33 , rminor = 12.3, alpha = 90)
# ellipse as a path
AGpath <- AG$path()
# the path is not closed; close it
AGpath <- rbind(AGpath, AGpath[1,])

# Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
WA <- Ellipse$new(center = c(-55, -293), rmajor =  28.4, rminor = 9.5 , alpha = 0)
# ellipse as a path
WApath <- WA$path()
# the path is not closed; close it
WApath <- rbind(WApath, WApath[1,])

# Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
OA <- Ellipse$new(center = c(-35, -175), rmajor =  28.4, rminor = 9.5 , alpha = 0)
# ellipse as a path
OApath <- OA$path()
# the path is not closed; close it
OApath <- rbind(OApath, OApath[1,])

# Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
WL <- Ellipse$new(center = c(-69, -330), rmajor =  56 , rminor = 18.7 , alpha = 90)
# ellipse as a path
WLpath <- WL$path()
# the path is not closed; close it
WLpath <- rbind(WLpath, WLpath[1,])

TH <- data.frame(x = c(-75, -40, -15, -40, -60), y = c(-350, -100, -150, -300, -350))
A <- data.frame(x = c(-50, -50, -25, -10, -10), y = c(-450, -300, -50, -50, -450))







# Set Working Directory
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


# Set Starting and Finish Time
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

# Read the CSV File
TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
# format the Date 'UTC'
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
# Convert the format of 'X.CH4.' to numeric
TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)
# Filter out all the dated that are outside the selected Strating and Finish time of the campaign
TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)
# Remove Empty Cells n data frame
TotalData <- TotalData[!is.na(TotalData$UTC),]

# Calculate 1/Mole Fraction for C13 & H2
TotalData$c13C <- 1/TotalData$X.CH4..13C
TotalData$c2H <- 1/TotalData$X.CH4..2H


CH4Data <- TotalData[, c("UTC", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H", "X.CH4.", "c13C", "c2H")]
CH4Data <- CH4Data[complete.cases(CH4Data[ , "X.CH4."]),]

CH4Data[,"Speed"] <- NA
CH4Data[,"Direction"] <- NA


for(i in 1:nrow(CH4Data)) {       # for-loop over rows
  # Find the mean Values during the Peak
  CH4Data[i ,"Speed"] <- mean(TotalData[TotalData$UTC >= (CH4Data[i,"UTC"] - 10*60) & TotalData$UTC <= (CH4Data[i,"UTC"] + 10*60), "Speed" ], na.rm = TRUE)
  CH4Data[i ,"Direction"] <- mean(TotalData[TotalData$UTC >= (CH4Data[i,"UTC"] - 10*60) & TotalData$UTC <= (CH4Data[i,"UTC"] + 10*60), "Direction" ], na.rm = TRUE)
}

CH4Data$Direction <- round(CH4Data$Direction, digits = -1)
CH4Data$Direction[CH4Data$Direction == 360] <- 0 


Keeling_output <- data_frame()

for (i in seq(0, 350, by = 10)){
  # Keeling Analyse for total data of the champagne Time series
  # For C13
  v_c13C_Line <- lm(d13C.VPDB ~ c13C, CH4Data[CH4Data$Direction == i,])
  v_c13C_coef <- coef(summary(v_c13C_Line))[, "Estimate"]
  v_c13C_se <- coef(summary(v_c13C_Line))[, "Std. Error"] 
  # For H2
  v_c2H_Line <- lm(d2H.VPDB ~ c2H, CH4Data[CH4Data$Direction == i,] )
  v_c2H_coef <- coef(summary(v_c2H_Line))[, "Estimate"]
  v_c2H_se <- coef(summary(v_c2H_Line))[, "Std. Error"] 
  
  # Keeling_List <- data_frame(c(i, c13C_coef, c13C_se, c2H_coef, c2H_se))
  # Keeling_output <- rbind(Keeling_output, Keeling_List)
  Keeling_output[nrow(Keeling_output) + 1, 1] <- as.integer(i)
  Keeling_output[nrow(Keeling_output) , 2] <- v_c13C_coef[[1]]
  Keeling_output[nrow(Keeling_output) , 3] <- v_c13C_se[[1]]
  Keeling_output[nrow(Keeling_output) , 4] <- v_c2H_coef[[1]]
  Keeling_output[nrow(Keeling_output) , 5] <- v_c2H_se[[1]]
}
colnames(Keeling_output) <- c("Direction", "c13C_coef", "c13C_se", "c2H_coef", "c2H_se")
Keeling_output <- Keeling_output[complete.cases(Keeling_output[ , ]),]


write.csv(Keeling_output, "4_Data/OutputData/Keeling_By_Wind_Total.csv", row.names=FALSE)


# plot(Keeling_output$Direction, Keeling_output$c13C_coef)
# plot(Keeling_output$Direction, Keeling_output$c2H_coef)
# 
# plot(Keeling_output$c13C_coef, Keeling_output$c2H_coef, col=Keeling_output$Direction)

Total_Plot <- ggplot() +
  geom_point(data=Keeling_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction), width=.1) + #, position=pd
  # geom_path(aes(x = x, y = y), as.data.frame(FFpath), color = "red")+
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  # geom_path(aes(x = x, y = y), as.data.frame(AGpath), color = "green")+
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  # geom_path(aes(x = x, y = y), as.data.frame(WApath), color = "yellow")+
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "yellow")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  # geom_path(aes(x = x, y = y), as.data.frame(OApath), color = "blue")+
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  # geom_path(aes(x = x, y = y), as.data.frame(WLpath), color = "black")+
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="purple")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.1, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="orange")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures")+
  scale_color_gradient2(midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )
  # scale_color_gradientn(colours = rainbow(5))
ggsave("12_Keeling_Total_Wind.png", Total_Plot, path = "4_Data/OutputData/Plots/12_Keeling_with_Wind", width = 10, height = 5)


# ################ Keeling analyse ##############
# 
# # Keeling Analyse for total data of the champagne Time series
# # For C13
# c13C_Line <- lm(d13C.VPDB ~ c13C, TotalData )
# c13C_coef <- coef(summary(c13C_Line))[, "Estimate"]
# c13C_se <- coef(summary(c13C_Line))[, "Std. Error"] 
# # For H2
# c2H_Line <- lm(d2H.VPDB ~ c2H, TotalData )
# c2H_coef <- coef(summary(c2H_Line))[, "Estimate"]
# c2H_se <- coef(summary(c2H_Line))[, "Std. Error"] 


# 
# ############## Keepling Plots ############
# 
# # Complete Timeline including peaks and base measurements
# q <- ggplot(TotalData, aes(x = c13C, y = d13C.VPDB)) +
#   geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
#   expand_limits(x = 0) +
#   geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
#   labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("12C, δ(13)C \n (mean = ", round(c13C_coef[[1]], digits = 1),"‰ ± ", round(c13C_se[[1]], digits = 1),"‰ s.e)")) +
#   theme(axis.text.x=element_text(angle=60, hjust=1),
#         plot.title = element_text(size=10))
# 
# k <- ggplot(TotalData, aes(x = c2H, y = d2H.VPDB)) +
#   expand_limits(x = 0) +
#   geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
#   geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
#   labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("2H, δ(2)H \n (mean = ", round(c2H_coef[[1]], digits = 1),"‰ ±", round(c2H_se[[1]], digits = 1),"‰ s.e)")) +
#   theme(axis.text.x=element_text(angle=60, hjust=1),
#         plot.title = element_text(size=10))
# KP_Total <- grid.arrange(q,k, ncol = 2,  top = textGrob("Keeling Plot of compleate measument campaign",gp=gpar(fontsize=15,font=3)))
# 
# ggsave("11_Keeling_Plot_Total.png", KP_Total, path = "4_Data/OutputData/Plots", width = 10, height = 5)
# 
# 
# 
# 
# ##### Show Keeling analyse Data in output Console ######
# message("\n \nTotal timeseries: \n 12C, δ(13)C (mean = ", c13C_coef[[1]],"‰ ± ", c13C_se[[1]],"‰ s.e; n = 1)","\n 2H, δ(2)H  (mean =", c2H_coef[[1]],"‰ ±", c2H_se[[1]],"‰ s.e; n = 1)")
# 
# 









############# Find the Peaks and the  Create a dataframe with only the Peaks ###########

#Select the Data from Data frame with CH4 Concentration
CH4Data2 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]
# Find the Peaks in the Remaining timeline
CH4_Peaks <- as.data.frame(findpeaks(CH4Data2$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE))

# Format the Peak Data frame 'CH4_Peaks'
# Rename the Columns
names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
# Replace the Index with Timestemps 'UTC'
CH4_Peaks$UTC_Beginning <- CH4Data2[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data2[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data2[CH4_Peaks$UTC,"UTC"]

# Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
# Get all Columns Names from 'TotalData
Heads <- colnames(TotalData)
# Remove empty Columns
Heads <- Heads[-1]
Heads <- Heads[-16]

# Create Data frame with only peaks
Total_Peaks <- data.frame()
# for-loop over rows
for(i in 1:nrow(CH4_Peaks)) {
  Single_Peak <- TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], ]
  Total_Peaks <- rbind(Total_Peaks,Single_Peak)
}

Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "X.CH4."]),]



Total_Peaks <- Total_Peaks[, c("UTC", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H", "X.CH4.", "c13C", "c2H")]
Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "X.CH4."]),]

Total_Peaks[,"Speed"] <- NA
Total_Peaks[,"Direction"] <- NA


for(i in 1:nrow(Total_Peaks)) {       # for-loop over rows
  # Find the mean Values during the Peak
  Total_Peaks[i ,"Speed"] <- mean(TotalData[TotalData$UTC >= (Total_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (Total_Peaks[i,"UTC"] + 10*60), "Speed" ], na.rm = TRUE)
  Total_Peaks[i ,"Direction"] <- mean(TotalData[TotalData$UTC >= (Total_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (Total_Peaks[i,"UTC"] + 10*60), "Direction" ], na.rm = TRUE)
}

Total_Peaks$Direction <- round(Total_Peaks$Direction, digits = -1)
Total_Peaks$Direction[Total_Peaks$Direction == 360] <- 0 


Keeling_Peaks_output <- data_frame()

for (i in seq(0, 350, by = 10)){
  if (length(which(Total_Peaks$Direction == i)) >= 4){
    # Keeling Analyse for total data of the champagne Time series
    # For C13
    p_c13C_Line <- lm(d13C.VPDB ~ c13C, Total_Peaks[Total_Peaks$Direction == i,])
    p_c13C_coef <- coef(summary(p_c13C_Line))[, "Estimate"]
    p_c13C_se <- coef(summary(p_c13C_Line))[, "Std. Error"] 
    # For H2
    p_c2H_Line <- lm(d2H.VPDB ~ c2H, Total_Peaks[Total_Peaks$Direction == i,] )
    p_c2H_coef <- coef(summary(p_c2H_Line))[, "Estimate"]
    p_c2H_se <- coef(summary(p_c2H_Line))[, "Std. Error"] 
    
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) + 1, 1] <- as.integer(i)
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 2] <- p_c13C_coef[[1]]
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 3] <- p_c13C_se[[1]]
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 4] <- p_c2H_coef[[1]]
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 5] <- p_c2H_se[[1]]
  }
}
colnames(Keeling_Peaks_output) <- c("Direction", "c13C_coef", "c13C_se", "c2H_coef", "c2H_se")
Keeling_Peaks_output <- Keeling_Peaks_output[complete.cases(Keeling_Peaks_output[ , ]),]

write.csv(Keeling_Peaks_output, "4_Data/OutputData/Keeling_By_Wind_Peaks.csv", row.names=FALSE)


# plot(Keeling_Peaks_output$Direction, Keeling_Peaks_output$c13C_coef)
# plot(Keeling_Peaks_output$Direction, Keeling_Peaks_output$c2H_coef)
# 
# plot(Keeling_Peaks_output$c13C_coef, Keeling_Peaks_output$c2H_coef, col=Keeling_Peaks_output$Direction)


Peaks_Plot <- ggplot() +
  geom_point(data=Keeling_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction), width=.1) + #, position=pd
  # geom_path(aes(x = x, y = y), as.data.frame(FFpath), color = "red")+
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  # geom_path(aes(x = x, y = y), as.data.frame(AGpath), color = "green")+
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  # geom_path(aes(x = x, y = y), as.data.frame(WApath), color = "yellow")+
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "yellow")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  # geom_path(aes(x = x, y = y), as.data.frame(OApath), color = "blue")+
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  # geom_path(aes(x = x, y = y), as.data.frame(WLpath), color = "black")+
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="purple")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.1, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="orange")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures, only Peaks")+
  scale_color_gradient2(midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )
  # scale_color_gradientn(colours = rainbow(5))
ggsave("12_Keeling_Peaks_Wind.png", Peaks_Plot, path = "4_Data/OutputData/Plots/12_Keeling_with_Wind", width = 10, height = 5)


No_Peaks <- subset(TotalData, UTC = Total_Peaks$UTC) ###### check if it works!!!!
No_Peaks <- No_Peaks[complete.cases(No_Peaks[ , "X.CH4."]),]




No_Peaks <- No_Peaks[, c("UTC", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H", "X.CH4.", "c13C", "c2H")]
No_Peaks <- No_Peaks[complete.cases(No_Peaks[ , "X.CH4."]),]

No_Peaks[,"Speed"] <- NA
No_Peaks[,"Direction"] <- NA


for(i in 1:nrow(No_Peaks)) {       # for-loop over rows
  # Find the mean Values during the Peak
  No_Peaks[i ,"Speed"] <- mean(TotalData[TotalData$UTC >= (No_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (No_Peaks[i,"UTC"] + 10*60), "Speed" ], na.rm = TRUE)
  No_Peaks[i ,"Direction"] <- mean(TotalData[TotalData$UTC >= (No_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (No_Peaks[i,"UTC"] + 10*60), "Direction" ], na.rm = TRUE)
}

No_Peaks$Direction <- round(No_Peaks$Direction, digits = -1)
No_Peaks$Direction[No_Peaks$Direction == 360] <- 0 


Keeling_No_Peaks_output <- data_frame()

for (i in seq(0, 350, by = 10)){
  if (length(which(No_Peaks$Direction == i)) >= 10){
    # Keeling Analyse for total data of the champagne Time series
    # For C13
    n_c13C_Line <- lm(d13C.VPDB ~ c13C, No_Peaks[No_Peaks$Direction == i,])
    n_c13C_coef <- coef(summary(n_c13C_Line))[, "Estimate"]
    n_c13C_se <- coef(summary(n_c13C_Line))[, "Std. Error"]
    # For H2
    n_c2H_Line <- lm(d2H.VPDB ~ c2H, No_Peaks[No_Peaks$Direction == i,] )
    n_c2H_coef <- coef(summary(n_c2H_Line))[, "Estimate"]
    n_c2H_se <- coef(summary(n_c2H_Line))[, "Std. Error"]
    
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) + 1, 1] <- as.integer(i)
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 2] <- n_c13C_coef[[1]]
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 3] <- n_c13C_se[[1]]
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 4] <- n_c2H_coef[[1]]
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 5] <- n_c2H_se[[1]]
  }
}
colnames(Keeling_No_Peaks_output) <- c("Direction", "c13C_coef", "c13C_se", "c2H_coef", "c2H_se")
Keeling_No_Peaks_output <- Keeling_No_Peaks_output[complete.cases(Keeling_No_Peaks_output[ , ]),]

write.csv(Keeling_No_Peaks_output, "4_Data/OutputData/Keeling_By_Wind_No_Peaks.csv", row.names=FALSE)


# plot(Keeling_No_Peaks_output$Direction, Keeling_No_Peaks_output$c13C_coef)
# plot(Keeling_No_Peaks_output$Direction, Keeling_No_Peaks_output$c2H_coef)
# 
# plot(Keeling_No_Peaks_output$c13C_coef, Keeling_No_Peaks_output$c2H_coef, col=Keeling_No_Peaks_output$Direction)

No_Peaks_Plot <- ggplot() +
  geom_point(data=Keeling_No_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_No_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se , colour=Direction), width=.02, alpha=0.5) + #, position=pd
  # geom_path(aes(x = x, y = y), as.data.frame(FFpath), color = "red")+
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  # geom_path(aes(x = x, y = y), as.data.frame(AGpath), color = "green")+
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  # geom_path(aes(x = x, y = y), as.data.frame(WApath), color = "yellow")+
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "yellow")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  # geom_path(aes(x = x, y = y), as.data.frame(OApath), color = "blue")+
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  # geom_path(aes(x = x, y = y), as.data.frame(WLpath), color = "black")+
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="purple")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.1, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="orange")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures, exluding Peaks")+
  scale_color_gradient2(midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )
  # scale_color_gradientn(colours = rainbow(5))
  
ggsave("12_Keeling_No_Peaks_Wind.png", No_Peaks_Plot, path = "4_Data/OutputData/Plots/12_Keeling_with_Wind", width = 10, height = 5)


# ggplot() +
#   geom_point(data=Keeling_No_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
#   # geom_path(aes(x = x, y = y), as.data.frame(FFpath), color = "red")+
#   geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
#   # geom_path(aes(x = x, y = y), as.data.frame(AGpath), color = "green")+
#   geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
#   # geom_path(aes(x = x, y = y), as.data.frame(WApath), color = "yellow")+
#   geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "yellow")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
#   # geom_path(aes(x = x, y = y), as.data.frame(OApath), color = "blue")+
#   geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
#   # geom_path(aes(x = x, y = y), as.data.frame(WLpath), color = "black")+
#   geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
#   geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="purple")+ # MC: Microbial CO2 reduction
#   geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.1, fill="pink")+ # MF: Microbial Fermentation
#   geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="orange")+ #TH: Thermogenic
#   geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue") # A: Abiotic
  
  # scale_colour_manual(name = 'Emitters', values =c('black'='black','red'='red', 'yellow' = 'yellow', 'blue' = 'blue', 'green' = 'green'), labels = c('WL','FF', 'WA', 'OA', 'AG'))+
  # scale_fill_identity(name = 'Typs', guide = 'legend',labels = c('MC', 'MF', 'TH', 'A'))
  
  # scale_colour_manual("", 
  #                     breaks = c("TempMax", "TempMedia", "TempMin", "tsdjs", "jxaax"),
  #                     values = c("red", "green", "blue", 'yellow', 'black')) 
  # 
  
  

# # Keeling analyse for Peaks
# # Peaks selected with Peak Finder
# # For C13
# p_c13C_Line <- lm(d13C.VPDB ~ c13C, Total_Peaks )
# p_c13C_coef <- coef(summary(p_c13C_Line))[, "Estimate"]
# p_c13C_se <- coef(summary(p_c13C_Line))[, "Std. Error"]
# # For H2
# p_c2H_Line <- lm(d2H.VPDB ~ c2H, Total_Peaks )
# p_c2H_coef <- coef(summary(p_c2H_Line))[, "Estimate"]
# p_c2H_se <- coef(summary(p_c2H_Line))[, "Std. Error"] 
# 
# # Keeling analyse excluding the peaks 
# # Peaks selected with Peak Finder
# # For C13
# r_c13C_Line <- lm(d13C.VPDB ~ c13C, No_Peaks )
# r_c13C_coef <- coef(summary(r_c13C_Line))[, "Estimate"]
# r_c13C_se <- coef(summary(r_c13C_Line))[, "Std. Error"] 
# # For H2
# r_c2H_Line <- lm(d2H.VPDB ~ c2H, No_Peaks )
# r_c2H_coef <- coef(summary(r_c2H_Line))[, "Estimate"]
# r_c2H_se <- coef(summary(r_c2H_Line))[, "Std. Error"] 


# # Plot Keeling Plot Only Peaks
# KP_13C_Peaks <- ggplot(Total_Peaks, aes(x = c13C, y = d13C.VPDB)) +
#   geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
#   expand_limits(x = 0) +
#   geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
#   labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("12C, δ(13)C \n (mean = ", round(p_c13C_coef[[1]], digits = 1),"‰ ± ", round(p_c13C_se[[1]], digits = 1),"‰ s.e)")) + 
#   theme(axis.text.x=element_text(angle=60, hjust=1),
#         plot.title = element_text(size=10))
# 
# KP_2H_Peaks <- ggplot(Total_Peaks, aes(x = c2H, y = d2H.VPDB)) +
#   expand_limits(x = 0) +
#   geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
#   geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
#   labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("2H, δ(2)H \n (mean = ", round( p_c2H_coef[[1]], digits = 1),"‰ ± ", round(p_c2H_se[[1]], digits = 1),"‰ s.e)")) +
#   theme(axis.text.x=element_text(angle=60, hjust=1),
#         plot.title = element_text(size=10))
# 
# KP_Peaks <- grid.arrange(KP_13C_Peaks,KP_2H_Peaks, ncol = 2,  top = textGrob("Keeling Plot of only the Peaks",gp=gpar(fontsize=15,font=3)))
# 
# ggsave("11_Keeling_Plot_Peaks.png", KP_Peaks, path = "4_Data/OutputData/Plots", width = 10, height = 5)
# 
# # Plot Keeling Plot No Peaks
# KP_13C_NoPeaks <- ggplot(No_Peaks, aes(x = c13C, y = d13C.VPDB)) +
#   geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
#   expand_limits(x = 0) +
#   geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
#   labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("12C, δ(13)C \n (mean = ", round(r_c13C_coef[[1]], digits = 1),"‰ ± ", round( r_c13C_se[[1]], digits = 1),"‰ s.e)")) +
#   theme(axis.text.x=element_text(angle=60, hjust=1),
#         plot.title = element_text(size=10))
# 
# KP_2H_NoPeaks <- ggplot(No_Peaks, aes(x = c2H, y = d2H.VPDB)) +
#   expand_limits(x = 0) +
#   geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
#   geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
#   labs(x = expression('(c'[CH4]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("2H, δ(2)H \n (mean = ", round(r_c2H_coef[[1]], digits = 1),"‰ ± ", round(r_c2H_se[[1]], digits = 1),"‰ s.e)")) +
#   theme(axis.text.x=element_text(angle=60, hjust=1),
#         plot.title = element_text(size=10))
# 
# KP_Ex_Peaks <- grid.arrange(KP_13C_NoPeaks,KP_2H_NoPeaks, ncol = 2,  top = textGrob("Keeling Plot of compleate measument campaign excluding the Peaks",gp=gpar(fontsize=15,font=3)))
# 
# ggsave("11_Keeling_Plot_Ex_Peaks.png", KP_Ex_Peaks, path = "4_Data/OutputData/Plots", width = 10, height = 5)

# message("Just the peaks: \n 12C, δ(13)C (mean = ", p_c13C_coef[[1]],"‰ ± ", p_c13C_se[[1]],"‰ s.e; n = 1)", " \n 2H, δ(2)H  (mean =", p_c2H_coef[[1]],"‰ ±", p_c2H_se[[1]],"‰ s.e; n = 1)")
# message("Excluding the peaks: \n 12C, δ(13)C (mean =", r_c13C_coef[[1]],"‰ ±", r_c13C_se[[1]],"‰ s.e; n = 1)","\n 2H, δ(2)H (mean =", r_c2H_coef[[1]],"‰ ±", r_c2H_se[[1]],"‰ s.e; n = 1)")




