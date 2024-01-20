
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
# Calculate R-squared value
r_squared <- function(Total_CH4_Data, start_time, H = 12) {
  
  
  SelectedData <- filter(Total_CH4_Data, Total_CH4_Data$UTC >= start_time & Total_CH4_Data$UTC < start_time + hours(H), .preserve = FALSE)
  
  
  if ((nrow(SelectedData[complete.cases(SelectedData[ , "X.CH4."]),]) > (H-round(H*0.1))) & (nrow(SelectedData[complete.cases(SelectedData[ , "CH4"]),]) > (H-round(H*0.1)))){
    correlation <- cor(SelectedData$X.CH4., SelectedData$CH4)
    r_squared_value <- correlation^2
    
    result <- data.frame(UTC = start_time, r_squared_value = r_squared_value)
    
    return(result)
  }
  else{
    result <- data.frame(UTC = start_time, r_squared_value = NA)

    return(result)
  }
  
}

#------------------------------------------------------------------

# Set Working Directory
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


# Set Starting and Finish Time
StartTime <- as.POSIXct('2021-08-01 23:00:00', 
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

Contribution_Averages <- colMeans(EMPA_csv[, c("CH4_energy", "CH4_residential", "CH4_industrial", "CH4_natural_gas", "CH4_transport", "CH4_waste", "CH4_agriculture", "CH4_bg")])
Contribution_Summery <- data.frame(AverageContributions = Contribution_Averages)
Contribution_Summery$ContributionPercent <- (Contribution_Summery$AverageContributions / sum(Contribution_Summery$AverageContributions)) * 100
Contribution_Summery <- round(Contribution_Summery, digits = 2)
write.csv(Contribution_Summery, file = "4_Data/OutputData/Plots/24_EMPA/24_EMPA_CH4_Contribution_Summery.csv", row.names = TRUE)

Contribution_Averages_no_bg <- colMeans(EMPA_csv[, c("CH4_energy", "CH4_residential", "CH4_industrial", "CH4_natural_gas", "CH4_transport", "CH4_waste", "CH4_agriculture")])
Contribution_Summery_no_bg <- data.frame(AverageContributions = Contribution_Averages_no_bg)
Contribution_Summery_no_bg$ContributionPercent <- (Contribution_Summery_no_bg$AverageContributions / sum(Contribution_Summery_no_bg$AverageContributions)) * 100
Contribution_Summery_no_bg <- round(Contribution_Summery_no_bg, digits = 2)
write.csv(Contribution_Summery_no_bg, file = "4_Data/OutputData/Plots/24_EMPA/24_EMPA_CH4_Contribution_Summery_no_bg.csv", row.names = TRUE)

################ Keeling analyse ##############

TotalData <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "d13C.VPDB", "c13C", "d2H.VPDB", "c2H")]

TotalData$UTC <- cut(TotalData$UTC, breaks="hour") 
TotalData_CH4 <- aggregate(X.CH4. ~ UTC, TotalData, mean)


EMPA_csv <- EMPA_csv[complete.cases(EMPA_csv[ , "CH4"]),c("dtm.end", "CH4", "d13C_CH4", "c13C", "dD_CH4", "c2H")]

EMPA_csv$dtm.end <- cut(EMPA_csv$dtm.end, breaks="hour") 
EMPA_CH4 <- aggregate(CH4 ~ dtm.end, EMPA_csv, mean)

Total_CH4_Data <- merge( TotalData_CH4[ , ], EMPA_CH4[ , ],
                         by.x = "UTC",
                         by.y = "dtm.end",
                         all.x = TRUE,
                         all.y = TRUE,
                         sort = TRUE)

Total_CH4_Data$UTC <- as.POSIXct(as.character(Total_CH4_Data$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")

result_df <- data.frame(UTC = as.POSIXct(character()), r_squared_value = numeric())
CurrentTime <- StartTime
H <- 12

while (CurrentTime < FinishTime) {
  # Call the function to perform calculations and append to the result dataframe
  result <- r_squared(Total_CH4_Data, CurrentTime, H)
  result_df <- bind_rows(result_df, result)

  # Move 1 hour forward
  CurrentTime <- CurrentTime + hours(1)
}


Total_CH4_Data <- merge( Total_CH4_Data[ , ], result_df[ , ],
                         by.x = "UTC",
                         by.y = "UTC",
                         all.x = TRUE,
                         all.y = TRUE,
                         sort = TRUE)

R2 <- 0.5

for (l in 1:nrow(Total_CH4_Data)){
  if (!is.na(Total_CH4_Data[l,"r_squared_value"]) & Total_CH4_Data[l,"r_squared_value"] > R2){
    Total_CH4_Data[l,"Condition1"] <- 1
  }
  else{
    Total_CH4_Data[l,"Condition1"] <- 0
  }
}

for (l in 1:nrow(Total_CH4_Data)){
  if (is.na(Total_CH4_Data[l,"r_squared_value"])){
    Total_CH4_Data[l,"Condition2"] <- 1
  }
  else{
    Total_CH4_Data[l,"Condition2"] <- 0
  }
}

for (l in 1:nrow(Total_CH4_Data)){
  if (!is.na(Total_CH4_Data[l,"r_squared_value"]) & Total_CH4_Data[l,"r_squared_value"] < R2){
    Total_CH4_Data[l,"Condition3"] <- 1
  }
  else{
    Total_CH4_Data[l,"Condition3"] <- 0
  }
}

# Set up custom colors for conditions
condition_colors <- c("green", "grey", "red")

# Plot CH4, Waterlevel Vs Time
CH4_TimeLine <- ggplot(Total_CH4_Data) +
  geom_line(aes(x = UTC, y = CH4), col = "red", alpha = 0.5) +
  labs(
    x = "Time [UTC]",
    y = expression("EMPA CH"[4] * " concentration [ppb]"),
    title = "Methane concentration EMPA Model & IRMS Measurement"
  ) +
  scale_x_datetime(date_breaks = "20 day", 
                   date_labels = "%d-%b", #) +
                   limits = c(as.POSIXct('2021-08-02 00:00:00',
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz ="utc"), 
                              as.POSIXct('2021-08-14 00:00:00',
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz ="utc"))) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title.y = element_text(color = "red", size = 13),
    axis.text.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue", size = 13),
    axis.text.y.right = element_text(color = "blue"),
    strip.text.x = element_blank()
  ) +
  geom_line(
    data = Total_CH4_Data,
    aes(x = UTC, y = X.CH4. * 1),
    col = "blue",
    alpha = 0.5
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~./1, name = "IRMS")
  ) +
  geom_rect(
    data = Total_CH4_Data[Total_CH4_Data$Condition1 == 1, ],
    aes(xmin = UTC, xmax = (UTC + hours(1)), ymin = -Inf, ymax = Inf, fill = "Condition 1"),
    alpha = 0.3
  ) +
  geom_rect(
    data = Total_CH4_Data[Total_CH4_Data$Condition2 == 1, ],
    aes(xmin = UTC, xmax = (UTC + hours(1)), ymin = -Inf, ymax = Inf, fill = "Condition 2"),
    alpha = 0.3
  ) +
  geom_rect(
    data = Total_CH4_Data[Total_CH4_Data$Condition3 == 1, ],
    aes(xmin = UTC, xmax = (UTC + hours(1)), ymin = -Inf, ymax = Inf, fill = "Condition 3"),
    alpha = 0.3
  ) +
  scale_fill_manual(
    name = paste('R2 for', H, ' Hours'),
    values = setNames(condition_colors, c("Condition 1", "Condition 2", "Condition 3")),
    labels = c(paste('R2 > ', R2), "R2 = NA", paste('R2 > ', R2))
  )



CH4_R2 <- ggplot(result_df) +
  geom_line(aes(x = UTC,
                y = r_squared_value),
            col = "red") +
  labs(x = "Time [UTC]",
       y =expression('R'^2),
       title = expression('R'^2*' Methane concentration EMPA Model & IRMS Measument')) +
  scale_x_datetime(date_breaks = "20 day",
                   date_labels = "%d-%b", #) +
                   limits = c(as.POSIXct('2021-08-02 00:00:00',
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz ="utc"), 
                              as.POSIXct('2021-08-14 00:00:00',
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz ="utc"))) +
  theme(axis.text.x=element_text(angle=60, hjust=1))

CH4_Timeline_Plot <- CH4_TimeLine / CH4_R2

# Save the plot
ggsave(paste0("4_CH4_R2_Timeline.png"),
       CH4_R2,
       path = "4_Data/OutputData/Plots/24_EMPA",
       width = 15,
       height = 7)


ggsave(paste0("4_CH4_R2_Timeline_Highlighted.png"),
       CH4_TimeLine,
       path = "4_Data/OutputData/Plots/24_EMPA",
       width = 15,
       height = 7)

ggsave(paste0("4_CH4_R2_Timeline_Combine.png"),
       CH4_Timeline_Plot,
       path = "4_Data/OutputData/Plots/24_EMPA",
       width = 15,
       height = 10)


