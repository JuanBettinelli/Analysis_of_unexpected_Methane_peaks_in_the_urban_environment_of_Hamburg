
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



#----------------------------------------------------------------



# Function to calculate the Yamamoto average wind direction
yamamoto_average <- function(directions_degrees) {
  radians <- directions_degrees * (pi / 180)
  sin_sum <- sum(sin(radians))
  cos_sum <- sum(cos(radians))
  average_rad <- atan2(sin_sum, cos_sum)
  average_deg <- average_rad * (180 / pi)
  if (average_deg < 0) {
    average_deg <- 360 + average_deg
  }
  return(average_deg)
}


#----------------------------------------------------------------
# Function to calculate circular variance
circular_variance <- function(directions_degrees) {
  radians <- directions_degrees * (pi / 180)
  sin_sum <- sum(sin(2 * radians))
  cos_sum <- sum(cos(2 * radians))
  circular_variance_value <- 1 - sqrt(sin_sum^2 + cos_sum^2) / length(directions_degrees)
  return(circular_variance_value)
}

#----------------------------------------------------------------


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

TotalData$Direction[TotalData$Direction > 361] <- NA
TotalData$Speed[TotalData$Speed > 99] <- NA
TotalData$Direction50m[TotalData$Direction50m > 361] <- NA
TotalData$Speed50m[TotalData$Speed50m > 99] <- NA
TotalData$Wind_Direction[TotalData$Wind_Direction > 361] <- NA
TotalData$Wind_Speed[TotalData$Wind_Speed > 99] <- NA
TotalData$Direction110m[TotalData$Direction110m > 361] <- NA
TotalData$Speed110m[TotalData$Speed110m > 99] <- NA


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

# Create Data frame with only peaks
Total_Peaks <- data.frame()
# for-loop over rows
for(i in 1:nrow(CH4_Peaks)) {
  Single_Peak <- TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], ]
  Total_Peaks <- rbind(Total_Peaks,Single_Peak)
}

Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "X.CH4."]),]



# ==============================================================================================================
# Strict peaks

############# Find the Peaks and the  Create a dataframe with only the Peaks ###########

# Find the Peaks in the timeline
# The Peaks criteria can be selected hire, The comments give some usefull ones
CH4_Large_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)


# Format the Peak Data frame 'CH4_Large_Peaks'
# Rename the Columns
names(CH4_Large_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
# Replace the Index with Timestemps 'UTC'
CH4_Large_Peaks$UTC_Beginning <- CH4Data[CH4_Large_Peaks$UTC_Beginning,"UTC"]
CH4_Large_Peaks$UTC_Ending <- CH4Data[CH4_Large_Peaks$UTC_Ending,"UTC"]
CH4_Large_Peaks$UTC <- CH4Data[CH4_Large_Peaks$UTC,"UTC"]

# Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
# Get all Columns Names from 'TotalData
Heads <- colnames(TotalData)
# Remove empty Columns
Heads <- Heads[-1]
Heads <- Heads[-16]

# Create Data frame with only peaks
Total_Large_Peaks <- data.frame()
# for-loop over rows
for(i in 1:nrow(CH4_Large_Peaks)) {
  Single_Peak <- TotalData[TotalData$UTC >= CH4_Large_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Large_Peaks[i,"UTC_Ending"], ]
  Total_Large_Peaks <- rbind(Total_Large_Peaks,Single_Peak)
}

Total_Large_Peaks <- Total_Large_Peaks[complete.cases(Total_Large_Peaks[ , "X.CH4."]),]




Geomatikum <- TotalData[complete.cases(TotalData[ , "Direction"]),c("UTC", "Direction", "Speed")]
# Calculate the Yamamoto average wind direction
Geomatikum_average <- data_frame()
Geomatikum_average[1, "Direction"] <- yamamoto_average(Geomatikum$Direction)
# Calculate circular standard deviation 
Geomatikum_average[1, "Direction_csd"] <- sqrt(-2 * log(1 - circular_variance(Geomatikum$Direction)))
Geomatikum_average[1, "Speed"] <- mean(Geomatikum$Speed)
Geomatikum_average[1, "Speed_sd"] <- sd(Geomatikum$Speed)

Mast_50 <- TotalData[complete.cases(TotalData[ , "Direction50m"]),c("UTC", "Direction50m", "Speed50m")]
# Calculate the Yamamoto average wind direction
Mast_50_average <- data_frame()
Mast_50_average[1, "Direction"] <- yamamoto_average(Mast_50$Direction50m)
# Calculate circular standard deviation 
Mast_50_average[1, "Direction_csd"] <- sqrt(-2 * log(1 - circular_variance(Mast_50$Direction50m)))
Mast_50_average[1, "Speed"] <- mean(Mast_50$Speed50m)
Mast_50_average[1, "Speed_sd"] <- sd(Mast_50$Speed50m)

Mast_110 <- TotalData[complete.cases(TotalData[ , "Direction110m"]),c("UTC", "Direction110m", "Speed110m")]
# Calculate the Yamamoto average wind direction
Mast_110_average <- data_frame()
Mast_110_average[1, "Direction"] <- yamamoto_average(Mast_110$Direction110m)
# Calculate circular standard deviation 
Mast_110_average[1, "Direction_csd"] <- sqrt(-2 * log(1 - circular_variance(Mast_110$Direction110m)))
Mast_110_average[1, "Speed"] <- mean(Mast_110$Speed110m)
Mast_110_average[1, "Speed_sd"] <- sd(Mast_110$Speed110m)


DWD <- TotalData[complete.cases(TotalData[ , "Wind_Direction"]),c("UTC", "Wind_Direction", "Wind_Speed")]
# Calculate the Yamamoto average wind direction
DWD_average <- data_frame()
DWD_average[1, "Direction"] <- yamamoto_average(DWD$Wind_Direction)
# Calculate circular standard deviation 
DWD_average[1, "Direction_csd"] <- sqrt(-2 * log(1 - circular_variance(DWD$Wind_Direction)))
DWD_average[1, "Speed"] <- mean(DWD$Wind_Speed)
DWD_average[1, "Speed_sd"] <- sd(DWD$Wind_Speed)

Small_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "Direction"]),c("UTC", "Direction", "Speed")]
# Calculate the Yamamoto average wind direction
Small_Peaks_average <- data_frame()
Small_Peaks_average[1, "Direction"] <- yamamoto_average(Small_Peaks$Direction)
# Calculate circular standard deviation 
Small_Peaks_average[1, "Direction_csd"] <- sqrt(-2 * log(1 - circular_variance(Small_Peaks$Direction)))
Small_Peaks_average[1, "Speed"] <- mean(Small_Peaks$Speed)
Small_Peaks_average[1, "Speed_sd"] <- sd(Small_Peaks$Speed)


Large_Peaks <- Total_Large_Peaks[complete.cases(Total_Large_Peaks[ , "Direction"]),c("UTC", "Direction", "Speed")]
# Calculate the Yamamoto average wind direction
Large_Peaks_average <- data_frame()
Large_Peaks_average[1, "Direction"] <- yamamoto_average(Large_Peaks$Direction)
# Calculate circular standard deviation 
Large_Peaks_average[1, "Direction_csd"] <- sqrt(-2 * log(1 - circular_variance(Large_Peaks$Direction)))
Large_Peaks_average[1, "Speed"] <- mean(Large_Peaks$Speed)
Large_Peaks_average[1, "Speed_sd"] <- sd(Large_Peaks$Speed)

cat("Geomatikum Yamamoto Average Wind Direction:", Geomatikum_average$Direction, "±", Geomatikum_average$Direction_csd , "degrees\n", "and Wind Speed", Geomatikum_average$Speed, "±" , Geomatikum_average$Speed_sd , "m/s\n",
"Weather mast 50 m Yamamoto Average Wind Direction:", Mast_50_average$Direction, "±", Mast_50_average$Direction_csd ,"degrees\n", "and Wind Speed", Mast_50_average$Speed, "±" , Mast_50_average$Speed_sd , "m/s\n",
"Weather mast 110 m Yamamoto Average Wind Direction:", Mast_110_average$Direction, "±", Mast_110_average$Direction_csd , "degrees\n", "and Wind Speed", Mast_110_average$Speed, "±" , Mast_110_average$Speed_sd , "m/s\n",
"DWD Yamamoto Average Wind Direction:", DWD_average$Direction, "±", DWD_average$Direction_csd , "degrees\n", "and Wind Speed", DWD_average$Speed, "±" , DWD_average$Speed_sd , "m/s\n",
"Geomatikum Small Peaks Yamamoto Average Wind Direction:", Small_Peaks_average$Direction, "±", Small_Peaks_average$Direction_csd , "degrees\n", "and Wind Speed", Small_Peaks_average$Speed, "±" , Small_Peaks_average$Speed_sd , "m/s\n",
"Geomatikum Large Peaks Yamamoto Average Wind Direction:", Large_Peaks_average$Direction, "±", Large_Peaks_average$Direction_csd , "degrees\n", "and Wind Speed", Large_Peaks_average$Speed, "±" , Large_Peaks_average$Speed_sd , "m/s\n")




