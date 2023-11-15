
# Author:
# Juan Bettinelli,
# Script to be used in the Paper "Quantification of methane emissions in Hamburg using a network of FTIR spectrometers and an inverse modeling approach"
# Data from the Hamburg campaign 2021-2022.
# This Script is used for a Keeling analysed of the data collected in Hamburg Geomatikum in 2021.
# The script produces a timeline of the total Methan concentration
#Last edit: 22.05.2023

# Declare librarys used

library(ggplot2)   
library(hexbin)
library(gridExtra)
library(dplyr)
library(plotly)
library(rio)
library(gridExtra)
library(grid)
library(pracma)
library(patchwork)
# library(ggplot2)   
# library(hexbin)
# library(dplyr)
# library(lubridate)
library(plotly)
library(rio)
library(cowplot)

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






EMPA_csv <- import("4_Data/9_EMPA/ts_sim_concDelta_HH_GEO_IFS_FP-C1.csv")
# format the Date 'UTC'
EMPA_csv$dtm.end <- as.POSIXct(as.character(EMPA_csv$dtm.end), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
# Convert the format of 'X.CH4.' to numeric
EMPA_csv$CH4 <- as.numeric(EMPA_csv$CH4)
# Filter out all the dated that are outside the selected Strating and Finish time of the campaign
EMPA_csv <- filter(EMPA_csv, EMPA_csv$dtm.end > StartTime & EMPA_csv$dtm.end < FinishTime, .preserve = FALSE)
# Remove Empty Cells n data frame
EMPA_csv <- EMPA_csv[!is.na(EMPA_csv$dtm.end),]

# Calculate 1/Mole Fraction for C13 & H2
EMPA_csv$c13C <- 1/EMPA_csv$CH4
EMPA_csv$c2H <- 1/EMPA_csv$CH4




############# Find the Peaks and the  Create a dataframe with only the Peaks ###########

# #Select the Data from Data frame with CH4 Concentration
# CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

##### Find Loweres 15%
#Select the Data from Dataframe with CH4 Concentration
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

# Sort the dataset in ascending order
sorted_data <- sort(CH4Data$X.CH4.)

# Determine the number of observations corresponding to the lowest 15% of the dataset
n_lowest <- round(length(sorted_data) * 0.15)

# Use the head() function to extract the lowest 15% of the dataset
lowest_15_percent <- max(head(sorted_data, n_lowest))
######


# Find the Peaks in the timeline
# The Peaks criteria can be selected hire, The comments give some usefull ones
CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4., minpeakheight = 2250, minpeakdistance = 30, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)


# Format the Peak Dataframe
names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]

# Find all the values in the TotalData Dataframe 12h before and 12 after the peak. The Time can be changed hire as needed
# than it findes the lowest value (troth) in that timeline
for (k in 1:nrow(CH4_Peaks)){
  testDFUp <- filter(TotalData, TotalData$UTC > (CH4_Peaks[k,2]) & TotalData$UTC < (CH4_Peaks[k,2]+12*60*60), .preserve = FALSE)
  testDFUp <- testDFUp[complete.cases(testDFUp[ , "X.CH4."]),]
  CH4_Up <- as.data.frame(findpeaks(-testDFUp$X.CH4., npeaks = 1, sortstr=TRUE))
  if (nrow(CH4_Up) == 0){
    CH4_Up <- data.frame(NA, tail(testDFUp$UTC, n = 1), NA, NA )
    names(CH4_Up) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
  }
  else{
    names(CH4_Up) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
    CH4_Up$UTC <- testDFUp[CH4_Up$UTC,"UTC"]
  }
  CH4_Peaks[k,"UTC_Ending"] <- CH4_Up[1, "UTC"]
  
  
  testDFDown <- filter(TotalData, TotalData$UTC > (CH4_Peaks[k,2]-12*60*60) & TotalData$UTC < (CH4_Peaks[k,2]), .preserve = FALSE)
  testDFDown <- testDFDown[complete.cases(testDFDown[ , "X.CH4."]),]
  CH4_Down <- as.data.frame(findpeaks(-testDFDown$X.CH4., npeaks = 1, sortstr=TRUE))
  if (nrow(CH4_Down) == 0){
    CH4_Down <- data.frame(NA, tail(testDFDown$UTC, n = 1), NA,NA )
    names(CH4_Down) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
  }
  else {
    names(CH4_Down) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
    CH4_Down$UTC <- testDFDown[CH4_Down$UTC,"UTC"]
  }
  CH4_Peaks[k,"UTC_Beginning"] <- CH4_Down[1, "UTC"]
}

# ##### Find Loweres 15%
# #Select the Data from Dataframe with CH4 Concentration
# CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]
# 
# # Sort the dataset in ascending order
# sorted_data <- sort(CH4Data$X.CH4.)
# 
# # Determine the number of observations corresponding to the lowest 15% of the dataset
# n_lowest <- round(length(sorted_data) * 0.15)
# 
# # Use the head() function to extract the lowest 15% of the dataset
# lowest_15_percent <- max(head(sorted_data, n_lowest))
# ######
# 
# # Find the Peaks in the Remaining timeline
# CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)
# 
# # Format the Peak Data frame 'CH4_Peaks'
# # Rename the Columns
# names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
# # Replace the Index with Timestemps 'UTC'
# CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
# CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
# CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]

# # Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
# # Get all Columns Names from 'TotalData
# Heads <- colnames(TotalData)
# # Remove empty Columns
# Heads <- Heads[-1]
# Heads <- Heads[-16]

# Create Data frame with only peaks
Total_Peaks <- data.frame()
# for-loop over rows
for(i in 1:nrow(CH4_Peaks)) {
  Single_Peak <- TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], ]
  Total_Peaks <- rbind(Total_Peaks,Single_Peak)
}

Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "X.CH4."]),]


No_Peaks <- subset(TotalData, UTC = Total_Peaks$UTC) ###### check if it works!!!!
No_Peaks <- No_Peaks[complete.cases(No_Peaks[ , "X.CH4."]),]


################ Keeling analyse ##############

# Keeling Analyse for total data of the champagne Time series
# For C13
c13C_Line <- lm(d13C.VPDB ~ c13C, TotalData )
c13C_coef <- coef(summary(c13C_Line))[, "Estimate"]
c13C_se <- coef(summary(c13C_Line))[, "Std. Error"] 
# For H2
c2H_Line <- lm(d2H.VPDB ~ c2H, TotalData )
c2H_coef <- coef(summary(c2H_Line))[, "Estimate"]
c2H_se <- coef(summary(c2H_Line))[, "Std. Error"] 

# Keeling analyse for Peaks
# Peaks selected with Peak Finder
# For C13
p_c13C_Line <- lm(d13C.VPDB ~ c13C, Total_Peaks )
p_c13C_coef <- coef(summary(p_c13C_Line))[, "Estimate"]
p_c13C_se <- coef(summary(p_c13C_Line))[, "Std. Error"]
# For H2
p_c2H_Line <- lm(d2H.VPDB ~ c2H, Total_Peaks )
p_c2H_coef <- coef(summary(p_c2H_Line))[, "Estimate"]
p_c2H_se <- coef(summary(p_c2H_Line))[, "Std. Error"] 

# Keeling analyse excluding the peaks 
# Peaks selected with Peak Finder
# For C13
r_c13C_Line <- lm(d13C.VPDB ~ c13C, No_Peaks )
r_c13C_coef <- coef(summary(r_c13C_Line))[, "Estimate"]
r_c13C_se <- coef(summary(r_c13C_Line))[, "Std. Error"] 
# For H2
r_c2H_Line <- lm(d2H.VPDB ~ c2H, No_Peaks )
r_c2H_coef <- coef(summary(r_c2H_Line))[, "Estimate"]
r_c2H_se <- coef(summary(r_c2H_Line))[, "Std. Error"] 

############## Keeling Plots ############

# Complete Timeline including peaks and base measurements
q <- ggplot(TotalData, aes(x = c13C, y = d13C.VPDB)) +
  geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(c13C_coef[[1]], digits = 1),"‰ ± ", round(c13C_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

k <- ggplot(TotalData, aes(x = c2H, y = d2H.VPDB)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("2H, δ(2)H \n (mean = ", round(c2H_coef[[1]], digits = 1),"‰ ±", round(c2H_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))
KP_Total <- grid.arrange(q,k, ncol = 2,  top = textGrob("Keeling plot of compleate measument campaign",gp=gpar(fontsize=15,font=3)))

ggsave("24_Keeling_Plot_Total.png", KP_Total, path = "4_Data/OutputData/Plots/24_EMPA", width = 10, height = 5)


# Plot Keeling Plot Only Peaks
KP_13C_Peaks <- ggplot(Total_Peaks, aes(x = c13C, y = d13C.VPDB)) +
  geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(p_c13C_coef[[1]], digits = 1),"‰ ± ", round(p_c13C_se[[1]], digits = 1),"‰ s.e)")) + 
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_2H_Peaks <- ggplot(Total_Peaks, aes(x = c2H, y = d2H.VPDB)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("D, δD \n (mean = ", round( p_c2H_coef[[1]], digits = 1),"‰ ± ", round(p_c2H_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_Peaks <- grid.arrange(KP_13C_Peaks,KP_2H_Peaks, ncol = 2,  top = textGrob("Keeling plot of only the Peaks",gp=gpar(fontsize=15,font=3)))

ggsave("24_Keeling_Plot_Peaks.png", KP_Peaks, path = "4_Data/OutputData/Plots/24_EMPA", width = 10, height = 5)

# Plot Keeling Plot No Peaks
KP_13C_NoPeaks <- ggplot(No_Peaks, aes(x = c13C, y = d13C.VPDB)) +
  geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(r_c13C_coef[[1]], digits = 1),"‰ ± ", round( r_c13C_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_2H_NoPeaks <- ggplot(No_Peaks, aes(x = c2H, y = d2H.VPDB)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("D, δD \n (mean = ", round(r_c2H_coef[[1]], digits = 1),"‰ ± ", round(r_c2H_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_Ex_Peaks <- grid.arrange(KP_13C_NoPeaks,KP_2H_NoPeaks, ncol = 2,  top = textGrob("Keeling plot of compleate measument campaign excluding methane peaks",gp=gpar(fontsize=15,font=3)))

ggsave("24_Keeling_Plot_Ex_Peaks.png", KP_Ex_Peaks, path = "4_Data/OutputData/Plots/24_EMPA", width = 10, height = 5)

##### Show Keeling analyse Data in output Console ######
message("\n \nTotal timeseries: \n 12C, δ(13)C (mean = ", c13C_coef[[1]],"‰ ± ", c13C_se[[1]],"‰ s.e; n = 1)","\n D, δD  (mean =", c2H_coef[[1]],"‰ ±", c2H_se[[1]],"‰ s.e; n = 1)")
message("Just the peaks: \n 12C, δ(13)C (mean = ", p_c13C_coef[[1]],"‰ ± ", p_c13C_se[[1]],"‰ s.e; n = 1)", " \n D, δD  (mean =", p_c2H_coef[[1]],"‰ ±", p_c2H_se[[1]],"‰ s.e; n = 1)")
message("Excluding the peaks: \n 12C, δ(13)C (mean =", r_c13C_coef[[1]],"‰ ±", r_c13C_se[[1]],"‰ s.e; n = 1)","\n D, δD (mean =", r_c2H_coef[[1]],"‰ ±", r_c2H_se[[1]],"‰ s.e; n = 1)")



#------------------------------------------------------------------------------------------------------------------------

##### Find Loweres 15%
#Select the Data from Data frame with CH4 Concentration
CH4DataEMPA <- EMPA_csv[complete.cases(EMPA_csv[ , "CH4"]),c("dtm.end", "CH4")]

##### Find Loweres 15%
#Select the Data from Dataframe with CH4 Concentration
CH4DataEMPA <- EMPA_csv[complete.cases(EMPA_csv[ , "CH4"]),c("dtm.end", "CH4")]

# Sort the dataset in ascending order
sorted_data_EMPA <- sort(CH4DataEMPA$CH4)

# Determine the number of observations corresponding to the lowest 15% of the dataset
n_lowest_EMPA <- round(length(sorted_data_EMPA) * 0.15)

# Use the head() function to extract the lowest 15% of the dataset
lowest_15_percent_EMPA <- max(head(sorted_data_EMPA, n_lowest_EMPA))
######


# Find the Peaks in the timeline
# The Peaks criteria can be selected hire, The comments give some usefull ones
CH4_Peaks_EMPA <- as.data.frame(findpeaks(CH4DataEMPA$CH4, minpeakheight = 2250, minpeakdistance = 30, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)


# Format the Peak Data frame 'CH4_Peaks_EMPA'
# Rename the Columns
names(CH4_Peaks_EMPA) <- c("CH4", "dtm.end", "UTC_Beginning", "UTC_Ending")
# Replace the Index with Timestemps 'dtm.end'
CH4_Peaks_EMPA$UTC_Beginning <- CH4DataEMPA[CH4_Peaks_EMPA$UTC_Beginning,"dtm.end"]
CH4_Peaks_EMPA$UTC_Ending <- CH4DataEMPA[CH4_Peaks_EMPA$UTC_Ending,"dtm.end"]
CH4_Peaks_EMPA$dtm.end <- CH4DataEMPA[CH4_Peaks_EMPA$dtm.end,"dtm.end"]

# Find all the values in the TotalData Dataframe 12h before and 12 after the peak. The Time can be changed hire as needed
# than it findes the lowest value (troth) in that timeline
for (k in 1:nrow(CH4_Peaks_EMPA)){
  testDFUp <- filter(EMPA_csv, EMPA_csv$dtm.end > (CH4_Peaks_EMPA[k,2]) & EMPA_csv$dtm.end < (CH4_Peaks_EMPA[k,2]+12*60*60), .preserve = FALSE)
  testDFUp <- testDFUp[complete.cases(testDFUp[ , "CH4"]),]
  CH4_Up <- as.data.frame(findpeaks(-testDFUp$CH4, npeaks = 1, sortstr=TRUE))
  if (nrow(CH4_Up) == 0){
    CH4_Up <- data.frame(NA, tail(testDFUp$UTC, n = 1), NA, NA )
    names(CH4_Up) <- c("CH4", "dtm.end", "UTC_Beginning", "UTC_Ending")
  }
  else{
    names(CH4_Up) <- c("CH4", "dtm.end", "UTC_Beginning", "UTC_Ending")
    CH4_Up$dtm.end <- testDFUp[CH4_Up$dtm.end,"dtm.end"]
  }
  CH4_Peaks_EMPA[k,"UTC_Ending"] <- CH4_Up[1, "dtm.end"]
  
  
  testDFDown <- filter(EMPA_csv, EMPA_csv$dtm.end > (CH4_Peaks_EMPA[k,2]-12*60*60) & EMPA_csv$dtm.end < (CH4_Peaks_EMPA[k,2]), .preserve = FALSE)
  testDFDown <- testDFDown[complete.cases(testDFDown[ , "CH4"]),]
  CH4_Down <- as.data.frame(findpeaks(-testDFDown$CH4, npeaks = 1, sortstr=TRUE))
  if (nrow(CH4_Down) == 0){
    CH4_Down <- data.frame(NA, tail(testDFDown$dtm.end, n = 1), NA,NA )
    names(CH4_Down) <- c("CH4", "dtm.end", "UTC_Beginning", "UTC_Ending")
  }
  else {
    names(CH4_Down) <- c("CH4", "dtm.end", "UTC_Beginning", "UTC_Ending")
    CH4_Down$dtm.end <- testDFDown[CH4_Down$dtm.end,"dtm.end"]
  }
  CH4_Peaks_EMPA[k,"UTC_Beginning"] <- CH4_Down[1, "dtm.end"]
}


#--------
# ############# Find the Peaks and the  Create a dataframe with only the Peaks ###########
# 
# #Select the Data from Data frame with CH4 Concentration
# CH4DataEMPA <- EMPA_csv[complete.cases(EMPA_csv[ , "CH4"]),c("dtm.end", "CH4")]
# 
# ##### Find Loweres 15%
# #Select the Data from Dataframe with CH4 Concentration
# CH4DataEMPA <- EMPA_csv[complete.cases(EMPA_csv[ , "CH4"]),c("dtm.end", "CH4")]
# 
# # Sort the dataset in ascending order
# sorted_data_EMPA <- sort(CH4DataEMPA$CH4)
# 
# # Determine the number of observations corresponding to the lowest 15% of the dataset
# n_lowest_EMPA <- round(length(sorted_data_EMPA) * 0.15)
# 
# # Use the head() function to extract the lowest 15% of the dataset
# lowest_15_percent_EMPA <- max(head(sorted_data_EMPA, n_lowest_EMPA))
# ######
# 
# # Find the Peaks in the Remaining timeline
# CH4_Peaks_EMPA <- as.data.frame(findpeaks(CH4DataEMPA$CH4, minpeakheight = lowest_15_percent_EMPA, minpeakdistance = 5, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)

# # Format the Peak Data frame 'CH4_Peaks'
# # Rename the Columns
# names(CH4_Peaks_EMPA) <- c("CH4", "dtm.end", "UTC_Beginning", "UTC_Ending")
# # Replace the Index with Timestemps 'dtm.end'
# CH4_Peaks_EMPA$UTC_Beginning <- CH4DataEMPA[CH4_Peaks_EMPA$UTC_Beginning,"dtm.end"]
# CH4_Peaks_EMPA$UTC_Ending <- CH4DataEMPA[CH4_Peaks_EMPA$UTC_Ending,"dtm.end"]
# CH4_Peaks_EMPA$dtm.end <- CH4DataEMPA[CH4_Peaks_EMPA$dtm.end,"dtm.end"]

# # Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
# # Get all Columns Names from 'TotalData
# Heads <- colnames(TotalData)
# # Remove empty Columns
# Heads <- Heads[-1]
# Heads <- Heads[-16]

# Create Data frame with only peaks
Total_Peaks_EMPA <- data.frame()
# for-loop over rows
for(i in 1:nrow(CH4_Peaks_EMPA)) {
  Single_Peak_EMPA <- EMPA_csv[EMPA_csv$dtm.end >= CH4_Peaks_EMPA[i,"UTC_Beginning"] & EMPA_csv$dtm.end <= CH4_Peaks_EMPA[i,"UTC_Ending"], ]
  Total_Peaks_EMPA <- rbind(Total_Peaks_EMPA,Single_Peak_EMPA)
}

Total_Peaks_EMPA <- Total_Peaks_EMPA[complete.cases(Total_Peaks_EMPA[ , "CH4"]),]


No_Peaks_EMPA <- subset(EMPA_csv, dtm.end = Total_Peaks_EMPA$dtm.end) ###### check if it works!!!!
No_Peaks_EMPA <- No_Peaks_EMPA[complete.cases(No_Peaks_EMPA[ , "CH4"]),]


################ Keeling analyse ##############

# Keeling Analyse for total data of the champagne Time series
# For C13
c13C_Line_EMPA <- lm(d13C_CH4 ~ c13C, EMPA_csv )
c13C_coef_EMPA <- coef(summary(c13C_Line_EMPA))[, "Estimate"]
c13C_se_EMPA <- coef(summary(c13C_Line_EMPA))[, "Std. Error"] 

# For H2
c2H_Line_EMPA <- lm(dD_CH4 ~ c2H, EMPA_csv)
c2H_coef_EMPA <- coef(summary(c2H_Line_EMPA))[, "Estimate"]
c2H_se_EMPA <- coef(summary(c2H_Line_EMPA))[, "Std. Error"] 

# Keeling analyse for Peaks
# Peaks selected with Peak Finder
# For C13
p_c13C_Line_EMPA <- lm(d13C_CH4 ~ c13C, Total_Peaks_EMPA )
p_c13C_coef_EMPA <- coef(summary(p_c13C_Line_EMPA))[, "Estimate"]
p_c13C_se_EMPA <- coef(summary(p_c13C_Line_EMPA))[, "Std. Error"]
# For H2
p_c2H_Line_EMPA <- lm(dD_CH4 ~ c2H, Total_Peaks_EMPA )
p_c2H_coef_EMPA <- coef(summary(p_c2H_Line_EMPA))[, "Estimate"]
p_c2H_se_EMPA <- coef(summary(p_c2H_Line_EMPA))[, "Std. Error"] 

# Keeling analyse excluding the peaks 
# Peaks selected with Peak Finder
# For C13
r_c13C_Line_EMPA <- lm(d13C_CH4 ~ c13C, No_Peaks_EMPA )
r_c13C_coef_EMPA <- coef(summary(r_c13C_Line_EMPA))[, "Estimate"]
r_c13C_se_EMPA <- coef(summary(r_c13C_Line_EMPA))[, "Std. Error"] 
# For H2
r_c2H_Line_EMPA <- lm(dD_CH4 ~ c2H, No_Peaks_EMPA )
r_c2H_coef_EMPA <- coef(summary(r_c2H_Line_EMPA))[, "Estimate"]
r_c2H_se_EMPA <- coef(summary(r_c2H_Line_EMPA))[, "Std. Error"] 

############## Keeling Plots ############

# Complete Timeline including peaks and base measurements
q <- ggplot(EMPA_csv, aes(x = c13C, y = d13C_CH4)) +
  geom_point(aes(x = c13C, y = d13C_CH4), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(c13C_coef_EMPA[[1]], digits = 1),"‰ ± ", round(c13C_se_EMPA[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

k <- ggplot(EMPA_csv, aes(x = c2H, y = dD_CH4)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = dD_CH4), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("2H, δ(2)H \n (mean = ", round(c2H_coef_EMPA[[1]], digits = 1),"‰ ±", round(c2H_se_EMPA[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))
KP_Total <- grid.arrange(q,k, ncol = 2,  top = textGrob("EMPA Model Keeling plot of compleate measument campaign",gp=gpar(fontsize=15,font=3)))

ggsave("24_Keeling_Plot_Total_EMPA.png", KP_Total, path = "4_Data/OutputData/Plots/24_EMPA", width = 10, height = 5)


# Plot Keeling Plot Only Peaks
KP_13C_Peaks <- ggplot(Total_Peaks_EMPA, aes(x = c13C, y = d13C_CH4)) +
  geom_point(aes(x = c13C, y = d13C_CH4), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(p_c13C_coef_EMPA[[1]], digits = 1),"‰ ± ", round(p_c13C_se_EMPA[[1]], digits = 1),"‰ s.e)")) + 
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_2H_Peaks <- ggplot(Total_Peaks_EMPA, aes(x = c2H, y = dD_CH4)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = dD_CH4), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("D, δD \n (mean = ", round( p_c2H_coef_EMPA[[1]], digits = 1),"‰ ± ", round(p_c2H_se_EMPA[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_Peaks <- grid.arrange(KP_13C_Peaks,KP_2H_Peaks, ncol = 2,  top = textGrob("EMPA Model Keeling plot of only the Peaks",gp=gpar(fontsize=15,font=3)))

ggsave("24_Keeling_Plot_Peaks_EMPA.png", KP_Peaks, path = "4_Data/OutputData/Plots/24_EMPA", width = 10, height = 5)

# Plot Keeling Plot No Peaks
KP_13C_NoPeaks <- ggplot(No_Peaks_EMPA, aes(x = c13C, y = d13C_CH4)) +
  geom_point(aes(x = c13C, y = d13C_CH4), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(r_c13C_coef_EMPA[[1]], digits = 1),"‰ ± ", round( r_c13C_se_EMPA[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_2H_NoPeaks <- ggplot(No_Peaks_EMPA, aes(x = c2H, y = dD_CH4)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = dD_CH4), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("D, δD \n (mean = ", round(r_c2H_coef_EMPA[[1]], digits = 1),"‰ ± ", round(r_c2H_se_EMPA[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_Ex_Peaks <- grid.arrange(KP_13C_NoPeaks,KP_2H_NoPeaks, ncol = 2,  top = textGrob("EMPA Model  Keeling plot of compleate measument campaign excluding methane peaks",gp=gpar(fontsize=15,font=3)))

ggsave("24_Keeling_Plot_Ex_Peaks_EMPA.png", KP_Ex_Peaks, path = "4_Data/OutputData/Plots/24_EMPA", width = 10, height = 5)

##### Show Keeling analyse Data in output Console ######
message("\n \nTotal timeseries for EMPA Model: \n 12C, δ(13)C (mean = ", c13C_coef_EMPA[[1]],"‰ ± ", c13C_se_EMPA[[1]],"‰ s.e; n = 1)","\n D, δD  (mean =", c2H_coef_EMPA[[1]],"‰ ±", c2H_se_EMPA[[1]],"‰ s.e; n = 1)")
message("Just the peaks for EMPA Model: \n 12C, δ(13)C (mean = ", p_c13C_coef_EMPA[[1]],"‰ ± ", p_c13C_se_EMPA[[1]],"‰ s.e; n = 1)", " \n D, δD  (mean =", p_c2H_coef_EMPA[[1]],"‰ ±", p_c2H_se_EMPA[[1]],"‰ s.e; n = 1)")
message("Excluding the peaks for EMPA Model: \n 12C, δ(13)C (mean =", r_c13C_coef_EMPA[[1]],"‰ ±", r_c13C_se_EMPA[[1]],"‰ s.e; n = 1)","\n D, δD (mean =", r_c2H_coef_EMPA[[1]],"‰ ±", r_c2H_se_EMPA[[1]],"‰ s.e; n = 1)")

#--------------------------------------------------------------------------------------------------------------

Keeling_EMPA <- data.frame(
  x = c13C_coef_EMPA[[1]],
  y = c2H_coef_EMPA[[1]],
  x_error = c13C_se_EMPA[[1]],
  y_error = c2H_se_EMPA[[1]]
)

Keeling_Measurment <- data.frame(
  x = c13C_coef[[1]],
  y = c2H_coef[[1]],
  x_error = c13C_se[[1]],
  y_error = c2H_se[[1]]
)


# Plotting the results in a combine isotope plot to identify emmittor type.
# regions are highlightet for the emmition type

## Create Ploygon to be used in the Plots
# Thermogenic
TH <- data.frame(x = c(-75, -40, -15, -40, -60), y = c(-350, -100, -150, -300, -350))
# Abiotic
A <- data.frame(x = c(-50, -50, -25, -10, -10), y = c(-450, -300, -50, -50, -450))


p0 <- ggplot() +
  geom_point(data=Keeling_EMPA, aes(x = x, y = y, color = "EMPA"), color = "blue")+
  geom_errorbar(data=Keeling_EMPA, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="EMPA"), width=.02, alpha=0.5, color = "blue") + #, position=pd
  geom_point(data=Keeling_Measurment, aes(x = x, y = y, color = "Measurement"), color = "red")+
  geom_errorbar(data=Keeling_Measurment, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="Measurement"), width=.02, alpha=0.5, color = "red") + #, position=pd
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "purple")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="yellow")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="#6B8E23")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures")+
  # scale_color_gradient2(midpoint=180, low="blue", mid="red",
                        # high="blue", space ="Lab" )+
  theme(legend.position = "none")


# fill only
p1 <- ggplot() +
  geom_point(data=Keeling_EMPA, aes(x = x, y = y, color = "EMPA"))+
  geom_errorbar(data=Keeling_EMPA, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="EMPA"), width=.02, alpha=0.5) + #, position=pd
  geom_point(data=Keeling_Measurment, aes(x = x, y = y, color = "Measurement"))+
  geom_errorbar(data=Keeling_Measurment, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="Measurement"), width=.02, alpha=0.5) + #, position=pd
  # scale_color_gradient2(name = "Wind Direction, °", midpoint=180, low="blue", mid="red",
  #                       high="blue", space ="Lab" )+
  scale_color_manual(name = "Keeling", values = c("EMPA" = "blue", "Measurement" = "red"))+
  theme(legend.direction = "vertical", legend.box = "vertical")


# color only
p2 <- ggplot() +
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175, color = "Fossil fuels & nonindustrial combustion"), alpha=0.1) + #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295, color = "Agriculture"),alpha=0.1) + # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293, color = "Waste"),alpha=0.1)+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81, color = "Other anthropogenic sources"),alpha=0.1)+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246, color = "Natural wetlands"), alpha=0.1) +# Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  scale_color_manual('Type',
                     breaks=c("Fossil fuels & nonindustrial combustion", "Agriculture", "Waste", "Other anthropogenic sources","Natural wetlands"),
                     values = c("Fossil fuels & nonindustrial combustion" = "red", "Agriculture" = "green", "Waste" = "purple", "Other anthropogenic sources" = "blue", "Natural wetlands" = "black"),  
                     guide = guide_legend(override.aes = list(alpha = 1)))
# theme(legend.direction = "vertical", legend.box = "vertical")

p3 <- ggplot() +
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250, fill="Microbial CO2 reduction"),alpha=0.1)+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125, fill="Microbial Fermentation"),alpha=0.5)+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y, fill="Thermogenic"), alpha=0.1)+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y, fill="Abiotic"), alpha=0.4)+ # A: Abiotic
  scale_fill_manual('Methode',
                    breaks=c("Microbial CO2 reduction", "Microbial Fermentation", "Thermogenic", "Abiotic"),
                    values = c("Microbial CO2 reduction" = 'yellow', "Microbial Fermentation" = "pink", "Thermogenic" = "#6B8E23", "Abiotic" = "lightblue"),  
                    guide = guide_legend(override.aes = list(alpha = 1)))

leg1 <- get_legend(p1)
leg2 <- get_legend(p2)
leg3 <- get_legend(p3)

# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()


# combine all legends
leg123 <- plot_grid(leg1, leg2, leg3,
                    align = "hv",
                    # axis = "tb",
                    nrow = 3
)

Total_Plot <- plot_grid(p0,
                        leg123,
                        nrow = 1,
                        align = "h",
                        axis = "t",
                        rel_widths = c(1, 0.3)
)

# Save and export the plot
ggsave("24_Dual_Isotopes_Total.png", Total_Plot, path = "4_Data/OutputData/Plots/24_EMPA", width = 12, height = 5.2)




#--------------------------------------------------------------------------------------------------------------


Keeling_Peaks_EMPA <- data.frame(
  x = p_c13C_coef_EMPA[[1]],
  y = p_c2H_coef_EMPA[[1]],
  x_error = p_c13C_se_EMPA[[1]],
  y_error = p_c2H_se_EMPA[[1]]
)

Keeling_Peaks_Measurment <- data.frame(
  x = p_c13C_coef[[1]],
  y = p_c2H_coef[[1]],
  x_error = p_c13C_se[[1]],
  y_error = p_c2H_se[[1]]
)


# Plotting the results in a combine isotope plot to identify emmittor type.
# regions are highlightet for the emmition type

## Create Ploygon to be used in the Plots
# Thermogenic
TH <- data.frame(x = c(-75, -40, -15, -40, -60), y = c(-350, -100, -150, -300, -350))
# Abiotic
A <- data.frame(x = c(-50, -50, -25, -10, -10), y = c(-450, -300, -50, -50, -450))


p0 <- ggplot() +
  geom_point(data=Keeling_Peaks_EMPA, aes(x = x, y = y, color = "EMPA"), color = "blue")+
  geom_errorbar(data=Keeling_Peaks_EMPA, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="EMPA"), width=.02, alpha=0.5, color = "blue") + #, position=pd
  geom_point(data=Keeling_Peaks_Measurment, aes(x = x, y = y, color = "Measurement"), color = "red")+
  geom_errorbar(data=Keeling_Peaks_Measurment, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="Measurement"), width=.02, alpha=0.5, color = "red") + #, position=pd
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "purple")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="yellow")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="#6B8E23")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures")+
  # scale_color_gradient2(midpoint=180, low="blue", mid="red",
  # high="blue", space ="Lab" )+
  theme(legend.position = "none")


# fill only
p1 <- ggplot() +
  geom_point(data=Keeling_EMPA, aes(x = x, y = y, color = "EMPA"))+
  geom_errorbar(data=Keeling_EMPA, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="EMPA"), width=.02, alpha=0.5) + #, position=pd
  geom_point(data=Keeling_Measurment, aes(x = x, y = y, color = "Measurement"))+
  geom_errorbar(data=Keeling_Measurment, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="Measurement"), width=.02, alpha=0.5) + #, position=pd
  # scale_color_gradient2(name = "Wind Direction, °", midpoint=180, low="blue", mid="red",
  #                       high="blue", space ="Lab" )+
  scale_color_manual(name = "Keeling", values = c("EMPA" = "blue", "Measurement" = "red"))+
  theme(legend.direction = "vertical", legend.box = "vertical")


# color only
p2 <- ggplot() +
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175, color = "Fossil fuels & nonindustrial combustion"), alpha=0.1) + #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295, color = "Agriculture"),alpha=0.1) + # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293, color = "Waste"),alpha=0.1)+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81, color = "Other anthropogenic sources"),alpha=0.1)+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246, color = "Natural wetlands"), alpha=0.1) +# Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  scale_color_manual('Type',
                     breaks=c("Fossil fuels & nonindustrial combustion", "Agriculture", "Waste", "Other anthropogenic sources","Natural wetlands"),
                     values = c("Fossil fuels & nonindustrial combustion" = "red", "Agriculture" = "green", "Waste" = "purple", "Other anthropogenic sources" = "blue", "Natural wetlands" = "black"),  
                     guide = guide_legend(override.aes = list(alpha = 1)))
# theme(legend.direction = "vertical", legend.box = "vertical")

p3 <- ggplot() +
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250, fill="Microbial CO2 reduction"),alpha=0.1)+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125, fill="Microbial Fermentation"),alpha=0.5)+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y, fill="Thermogenic"), alpha=0.1)+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y, fill="Abiotic"), alpha=0.4)+ # A: Abiotic
  scale_fill_manual('Methode',
                    breaks=c("Microbial CO2 reduction", "Microbial Fermentation", "Thermogenic", "Abiotic"),
                    values = c("Microbial CO2 reduction" = 'yellow', "Microbial Fermentation" = "pink", "Thermogenic" = "#6B8E23", "Abiotic" = "lightblue"),  
                    guide = guide_legend(override.aes = list(alpha = 1)))

leg1 <- get_legend(p1)
leg2 <- get_legend(p2)
leg3 <- get_legend(p3)

# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()


# combine all legends
leg123 <- plot_grid(leg1, leg2, leg3,
                    align = "hv",
                    # axis = "tb",
                    nrow = 3
)

Total_Plot <- plot_grid(p0,
                        leg123,
                        nrow = 1,
                        align = "h",
                        axis = "t",
                        rel_widths = c(1, 0.3)
)

# Save and export the plot
ggsave("24_Dual_Isotopes_Peaks.png", Total_Plot, path = "4_Data/OutputData/Plots/24_EMPA", width = 12, height = 5.2)


#--------------------------------------------------------------------------------------------------------------


Keeling_Peaks_EMPA <- data.frame(
  x = r_c13C_coef_EMPA[[1]],
  y = r_c2H_coef_EMPA[[1]],
  x_error = r_c13C_se_EMPA[[1]],
  y_error = r_c2H_se_EMPA[[1]]
)

Keeling_Peaks_Measurment <- data.frame(
  x = r_c13C_coef[[1]],
  y = r_c2H_coef[[1]],
  x_error = r_c13C_se[[1]],
  y_error = r_c2H_se[[1]]
)


# Plotting the results in a combine isotope plot to identify emmittor type.
# regions are highlightet for the emmition type

## Create Ploygon to be used in the Plots
# Thermogenic
TH <- data.frame(x = c(-75, -40, -15, -40, -60), y = c(-350, -100, -150, -300, -350))
# Abiotic
A <- data.frame(x = c(-50, -50, -25, -10, -10), y = c(-450, -300, -50, -50, -450))


p0 <- ggplot() +
  geom_point(data=Keeling_Peaks_EMPA, aes(x = x, y = y, color = "EMPA"), color = "blue")+
  geom_errorbar(data=Keeling_Peaks_EMPA, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="EMPA"), width=.02, alpha=0.5, color = "blue") + #, position=pd
  geom_point(data=Keeling_Peaks_Measurment, aes(x = x, y = y, color = "Measurement"), color = "red")+
  geom_errorbar(data=Keeling_Peaks_Measurment, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="Measurement"), width=.02, alpha=0.5, color = "red") + #, position=pd
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "purple")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="yellow")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="#6B8E23")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures")+
  # scale_color_gradient2(midpoint=180, low="blue", mid="red",
  # high="blue", space ="Lab" )+
  theme(legend.position = "none")


# fill only
p1 <- ggplot() +
  geom_point(data=Keeling_EMPA, aes(x = x, y = y, color = "EMPA"))+
  geom_errorbar(data=Keeling_EMPA, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="EMPA"), width=.02, alpha=0.5) + #, position=pd
  geom_point(data=Keeling_Measurment, aes(x = x, y = y, color = "Measurement"))+
  geom_errorbar(data=Keeling_Measurment, aes(x = x, xmin=x - x_error, xmax= x + x_error, y = y, ymin=y-y_error, ymax=y+y_error, colour="Measurement"), width=.02, alpha=0.5) + #, position=pd
  # scale_color_gradient2(name = "Wind Direction, °", midpoint=180, low="blue", mid="red",
  #                       high="blue", space ="Lab" )+
  scale_color_manual(name = "Keeling", values = c("EMPA" = "blue", "Measurement" = "red"))+
  theme(legend.direction = "vertical", legend.box = "vertical")


# color only
p2 <- ggplot() +
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175, color = "Fossil fuels & nonindustrial combustion"), alpha=0.1) + #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295, color = "Agriculture"),alpha=0.1) + # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293, color = "Waste"),alpha=0.1)+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81, color = "Other anthropogenic sources"),alpha=0.1)+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246, color = "Natural wetlands"), alpha=0.1) +# Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  scale_color_manual('Type',
                     breaks=c("Fossil fuels & nonindustrial combustion", "Agriculture", "Waste", "Other anthropogenic sources","Natural wetlands"),
                     values = c("Fossil fuels & nonindustrial combustion" = "red", "Agriculture" = "green", "Waste" = "purple", "Other anthropogenic sources" = "blue", "Natural wetlands" = "black"),  
                     guide = guide_legend(override.aes = list(alpha = 1)))
# theme(legend.direction = "vertical", legend.box = "vertical")

p3 <- ggplot() +
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250, fill="Microbial CO2 reduction"),alpha=0.1)+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125, fill="Microbial Fermentation"),alpha=0.5)+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y, fill="Thermogenic"), alpha=0.1)+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y, fill="Abiotic"), alpha=0.4)+ # A: Abiotic
  scale_fill_manual('Methode',
                    breaks=c("Microbial CO2 reduction", "Microbial Fermentation", "Thermogenic", "Abiotic"),
                    values = c("Microbial CO2 reduction" = 'yellow', "Microbial Fermentation" = "pink", "Thermogenic" = "#6B8E23", "Abiotic" = "lightblue"),  
                    guide = guide_legend(override.aes = list(alpha = 1)))

leg1 <- get_legend(p1)
leg2 <- get_legend(p2)
leg3 <- get_legend(p3)

# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()


# combine all legends
leg123 <- plot_grid(leg1, leg2, leg3,
                    align = "hv",
                    # axis = "tb",
                    nrow = 3
)

Total_Plot <- plot_grid(p0,
                        leg123,
                        nrow = 1,
                        align = "h",
                        axis = "t",
                        rel_widths = c(1, 0.3)
)

# Save and export the plot
ggsave("24_Dual_Isotopes_No_Peaks.png", Total_Plot, path = "4_Data/OutputData/Plots/24_EMPA", width = 12, height = 5.2)




