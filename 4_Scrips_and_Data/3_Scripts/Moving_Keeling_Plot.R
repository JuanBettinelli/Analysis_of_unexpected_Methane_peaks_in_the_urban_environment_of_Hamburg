
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
library(lubridate)
library(plotly)
library(rio)
library(cowplot)

#------------------------------------------------------------------
# Function to perform calculations on selected rows
Keeling_Caluclation <- function(TotalData, start_time) {
  # Select rows within the first 12 hours
  # selected_rows <- TotalData %>%
  #   filter(between(DateTime, start_time, start_time + hours(12)))
  TotalData <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "d13C.VPDB", "c13C", "d2H.VPDB", "c2H")]
  
  SelectedData <- filter(TotalData, TotalData$UTC > start_time & TotalData$UTC < start_time + hours(12), .preserve = FALSE)

  if ((nrow(SelectedData[complete.cases(SelectedData[ , "d13C.VPDB"]),]) > 5) & (nrow(SelectedData[complete.cases(SelectedData[ , "d2H.VPDB"]),]) > 5)){
    # Keeling Analyse for total data of the champagne Time series
    # For C13
    c13C_Line <- lm(d13C.VPDB ~ c13C, SelectedData )
    c13C_coef <- coef(summary(c13C_Line))[, "Estimate"]
    c13C_se <- coef(summary(c13C_Line))[, "Std. Error"] 
    # For H2
    c2H_Line <- lm(d2H.VPDB ~ c2H, SelectedData )
    c2H_coef <- coef(summary(c2H_Line))[, "Estimate"]
    c2H_se <- coef(summary(c2H_Line))[, "Std. Error"]
    
    result <- data.frame(UTC = start_time, c13C_coef = c13C_coef[[1]], c2H_coef = c2H_coef[[1]])
    
    return(result)
  }
  else{
    result <- data.frame(UTC = start_time, c13C_coef = NA, c2H_coef = NA)
    return(result)
  }
  
  
}
#------------------------------------------------------------------

# Function to perform calculations on selected rows
Keeling_Caluclation_Empa <- function(EMPA_csv, start_time) {
  # Select rows within the first 12 hours
  # selected_rows <- TotalData %>%
  #   filter(between(DateTime, start_time, start_time + hours(12)))
  EMPA_csv <- EMPA_csv[complete.cases(EMPA_csv[ , "CH4"]),c("dtm.end", "CH4", "d13C_CH4", "c13C", "dD_CH4", "c2H")]
  
  SelectedData <- filter(EMPA_csv, EMPA_csv$dtm.end > start_time & EMPA_csv$dtm.end < start_time + hours(12), .preserve = FALSE)
  
  if ((nrow(SelectedData[complete.cases(SelectedData[ , "d13C_CH4"]),]) > 5) & (nrow(SelectedData[complete.cases(SelectedData[ , "dD_CH4"]),]) > 5)){
    # Keeling Analyse for total data of the champagne Time series
    # For C13
    c13C_Line_EMPA <- lm(d13C_CH4 ~ c13C, SelectedData )
    c13C_coef_EMPA <- coef(summary(c13C_Line_EMPA))[, "Estimate"]
    c13C_se_EMPA <- coef(summary(c13C_Line_EMPA))[, "Std. Error"] 
    
    # For H2
    c2H_Line_EMPA <- lm(dD_CH4 ~ c2H, SelectedData)
    c2H_coef_EMPA <- coef(summary(c2H_Line_EMPA))[, "Estimate"]
    c2H_se_EMPA <- coef(summary(c2H_Line_EMPA))[, "Std. Error"] 
    
    result <- data.frame(dtm.end = start_time, c13C_coef_EMPA = c13C_coef_EMPA[[1]], c2H_coef_EMPA = c2H_coef_EMPA[[1]])
    
    return(result)
  }
  else{
    result <- data.frame(dtm.end = start_time, c13C_coef_EMPA = NA, c2H_coef_EMPA = NA)
    return(result)
  }
  
  
}
#------------------------------------------------------------------


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


################ Keeling analyse ##############

result_df <- data.frame(UTC = as.POSIXct(character()), c13C_coef = numeric(), c2H_coef = numeric())
CurrentTime <- StartTime

while (CurrentTime < FinishTime) {
  # Call the function to perform calculations and append to the result dataframe
  result <- Keeling_Caluclation(TotalData, CurrentTime)
  result_df <- bind_rows(result_df, result)
  
  # Move 1 hour forward
  CurrentTime <- CurrentTime + hours(1)
}

hist(result_df$c13C_coef, main =  expression('Histogram of '*delta^13*'C in ‰'), xlab = expression(delta^13*'C in ‰'), ylab = "Number  with MKP", col = "lightblue", border = "black")
hist(result_df$c2H_coef, main =  expression('Histogram of '*delta^2*'H in ‰'), xlab = expression(delta^2*'D in ‰'), ylab = "Number  with MKP", col = "red", border = "black")




result_df_EMPA <- data.frame(dtm.end = as.POSIXct(character()), c13C_coef_EMPA = numeric(), c2H_coef_EMPA = numeric())
CurrentTime <- StartTime

while (CurrentTime < FinishTime) {
  # Call the function to perform calculations and append to the result dataframe
  result <- Keeling_Caluclation_Empa(EMPA_csv, CurrentTime)
  result_df_EMPA <- bind_rows(result_df_EMPA, result)
  
  # Move 1 hour forward
  CurrentTime <- CurrentTime + hours(1)
}

hist(result_df_EMPA$c13C_coef_EMPA, main =  expression('Histogram of '*delta^13*'C in ‰'), xlab = expression(delta^13*'C in ‰'), ylab = "Number  with MKP", col = "lightblue", border = "black")
hist(result_df_EMPA$c2H_coef_EMPA, main =  expression('Histogram of '*delta^2*'H in ‰'), xlab = expression(delta^2*'D in ‰'), ylab = "Number  with MKP", col = "red", border = "black")



# Combine data into a data frame
df_C13 <- data.frame(value = c(result_df$c13C_coef, result_df_EMPA$c13C_coef_EMPA),
                 group = rep(c("Measument", "EMPA")))

# Combine data into a data frame
df_2H <- data.frame(value = c(result_df$c2H_coef, result_df_EMPA$c2H_coef_EMPA),
                    group = rep(c("Measument", "EMPA")))

# Create a grouped histogram with a different number of bins
Plot_13C <- ggplot(df_C13, aes(x = value, fill = group)) +
                    geom_histogram(position = "dodge", bins = 10, color = "black", alpha = 0.7) +
                    labs(title = expression('Moving Keeling Plot Histogram of '*delta^13*'C in ‰'), x = expression(delta^13*'C in ‰'), y = "Frequency") +
                    scale_fill_manual(values = c("Measument" = "lightblue", "EMPA" = "lightgreen"))


# Create a grouped histogram with a different number of bins
Plot_2H <- ggplot(df_2H, aes(x = value, fill = group)) +
                  geom_histogram(position = "dodge", bins = 10, color = "black", alpha = 0.7) +
                  labs(title = expression('Moving Keeling Plot Histogram of '*delta^2*'H in ‰'), x = expression(delta^2*'H in ‰'), y = "Frequency") +
                  scale_fill_manual(values = c("Measument" = "lightblue", "EMPA" = "lightgreen"))


KeelingHistograms <- Plot_13C / Plot_2H

# Save and export the plot
ggsave("24_Moving_Keeling_Plot_Histograms.png", KeelingHistograms, path = "4_Data/OutputData/Plots/24_EMPA", width = 7, height = 12)
