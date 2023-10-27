# Script to Plot the Data From the "CombineMeteorologicalData.csv" created by the script "Combine_All_Data_To_CSV_File.R"
# Author Juan Bettinelli
# Last change: 26.1.23

# library(plyr)
# library(dplyr)
# library(plotly)
# library(rio)
# 
# library(plyr)
# library(ggplot2)   
# library(hexbin)
# library(reshape2)
# library(openair)
# library(cowplot)
# library(patchwork)
# library(dplyr)
# library(GGally)
# library(ggvis)
# library(httr)
# library(plotly)
# library(stringr)
# library(tidyr)
# library(pracma)




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
library(zoo)
library(dplyr)

#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automatically access the data.
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


StartTime <- as.POSIXct('2021-08-01 22:03:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2022-03-29 00:00:00', 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz ="utc")

# LIDAR 2021-09-05 22:30:00
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

TotalData <- fill(TotalData, starts_with(colnames(TotalData[,2:27])), .direction = "up")
# TotalData <- na.omit(TotalData)


for (l in 1:nrow(TotalData)){
  if (TotalData[l,"Water_Level"] < 500 & TotalData[l,"Direction"] > 160 & TotalData[l,"Direction"] < 280 & TotalData[l,"Speed"]>0 & TotalData[l,"Speed"]<6 ){ #   & TotalData[l,"LIDAR_BLH"]<700
    TotalData[l,"Condition1"] <- 1
  }
  else{
    TotalData[l,"Condition1"] <- 0
  }
}
# for (l in 1:nrow(TotalData)){
#   if (TotalData[l,"Water_Level"] < 500 & TotalData[l,"Direction"] > 90 & TotalData[l,"Direction"] < 180 & TotalData[l,"Speed"]>0 & TotalData[l,"Speed"]<6 ){ #  & TotalData[l,"LIDAR_BLH"]<700
#     TotalData[l,"Condition2"] <- 0
#   }
#   else{
#     TotalData[l,"Condition2"] <- 0
#   }
# }
# for (l in 1:nrow(TotalData)){
#   if (TotalData[l,"Water_Level"] < 500 & TotalData[l,"Direction"] > 180 & TotalData[l,"Direction"] < 270 & TotalData[l,"Speed"]>0 & TotalData[l,"Speed"]<6 ){ #   & TotalData[l,"LIDAR_BLH"]<700
#     TotalData[l,"Condition3"] <- 0
#   }
#   else{
#     TotalData[l,"Condition3"] <- 0
#   }
# }
# for (l in 1:nrow(TotalData)){
#   if (TotalData[l,"Water_Level"] < 500 & TotalData[l,"Direction"] > 270 & TotalData[l,"Direction"] < 360 & TotalData[l,"Speed"]>0 & TotalData[l,"Speed"]<6 ){ #   & TotalData[l,"LIDAR_BLH"]<700
#     TotalData[l,"Condition4"] <- 0
#   }
#   else{
#     TotalData[l,"Condition4"] <- 0
#   }
# }


#------------------------------------------------------------------------------------------------------------
# Function to split the Timeline into separate Plots/Panels

panel_function <- function(TotalData, n){
  # library("dplyr")
  
  # 0 is used in Campain paper, here Equal Sized Plots are produced regadles of the compleatness of the data
  if (n == 0){
    TotalData$panel[TotalData$UTC <= "2021-08-10 23:59:00"] <- 0
    TotalData$panel[TotalData$UTC >= "2021-08-11 00:00:00" & TotalData$UTC <= "2021-08-18 23:59:00"] <- 1
    TotalData$panel[TotalData$UTC >= "2021-08-19 00:00:00" & TotalData$UTC <= "2021-08-28 23:59:00"] <- 2
    TotalData$panel[TotalData$UTC >= "2021-08-29 00:00:00"] <- 3
    return(TotalData)
  } else{ # Automaticaly splits the timeline into panels/Plotts, n is the number of Panels
    TotalData <- TotalData %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData))*n))
    return(TotalData)
  }
}

#------------------------------------------------------------------------------------------------------------
# function that checks Fixed panel sizes are uesd and changes n if that is the case 
panel_No_function <- function(n){
  if (n == 0){
    m <- 4
    return(m)
  }
  else{
    m <- n
    return(m)
  }
}


#------------------------------------------------------------------------------------------------------------
# Function to Find CH4 Peaks in Timeline
CH4_Peak_Finder <- function(TotalData = TotalData, Export_CSV = TRUE){
  
  #Select the Data from Dataframe with CH4 Concentration
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]
  
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
  CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4., minpeakheight = 2100, minpeakdistance = 30, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)
  
  for (k in 1:nrow(CH4_Peaks)){
    if ((CH4_Peaks[k,4] - CH4_Peaks[k, 3]) < 6 ){
      CH4_Peaks[k, 3] <- CH4_Peaks[k, 3] - 5
      CH4_Peaks[k, 4] <- CH4_Peaks[k, 4] + 5
    }
  }
  
  
  
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
  for (j in Heads){
    # Create new Coloums with same Names
    CH4_Peaks[,j] <- NA
    for(i in 1:nrow(CH4_Peaks)) {       # for-loop over rows
      # Find the mean Values during the Peak
      CH4_Peaks[i, j] <- mean(TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], j], na.rm = TRUE)
    }
  }
  
  # Remove the "Peaks" at where no measurements were taken (12h)
  CH4_Peaks <- subset(CH4_Peaks, (UTC_Ending - UTC_Beginning) < 12*60 )
  
  # Checks if the Data Should be returend to the Script ode exported into a CSV File
  if (Export_CSV){
    write.csv(CH4_Peaks, "4_Data/OutputData/SecondPaper/Peak/CH4_Peaks.csv", row.names=TRUE)
    write.csv(colMeans(CH4_Peaks[,c(1,5:29)], na.rm = TRUE), "4_Data/OutputData/SecondPaper/Peak/CH4_PeaksMean.csv", row.names=TRUE)
    write.csv(colMeans(TotalData[,2:27], na.rm = TRUE), "4_Data/OutputData/SecondPaper/Peak/CH4_TotalMean.csv", row.names=TRUE)
  }
  else {
    return(CH4_Peaks)
  }
}

#------------------------------------------------------------------------------------------------------------

# Function to Plot a CH4 Timeline with A Peak detection
CH4_TimeLine <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime, n = 10, Panel_Plot = FALSE){
  
  # calling funktions to splite timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  #Select the Data from dataframe with CH4 Concentration
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "panel", "Condition1")] #, "Condition2", "Condition3", "Condition4")]
  
  # # Find the Methan Peaks
  # CH4_Peaks <- CH4_Peak_Finder(TotalData, FALSE)
  # CH4_Peaks$panel <- CH4Data[match(CH4_Peaks$UTC, CH4Data$UTC),"panel"]
  # CH4_min <- min(CH4Data$X.CH4.)
  # CH4_max <- max(CH4Data$X.CH4.)
  
  
  # Select Plot in Panels or in separate files
  if (Panel_Plot == FALSE) {
    
    # Loop throw individual panels
    for (i in (0:(m-1))){
      # Create the Timeline plot
      CH4_TimeLine <- ggplot(CH4Data[CH4Data$panel == i, ], aes(x = UTC, y = X.CH4.)) +
        geom_line() +
        labs(x = "Fill Time [UTC]",
             y = expression("CH"[4]*" concentration [ppb]"),
             title = "Methane concentration vs. Time Highlightes good Conditions Wind Speed & Direction, Water Level, Boundary Layer Hight") +
        scale_x_datetime(date_breaks = "2 day",
                         date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
        theme(axis.text.x=element_text(angle=60,
                                       hjust=1),
              strip.text.x = element_blank())+ #,
        #legend.position="none")+
        # geom_rect(data=CH4_Peaks[CH4_Peaks$panel == i, ], inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
        #                                                                          ymax=CH4_max, fill=Direction), alpha=0.3)+
        # scale_fill_continuous(name = "Mean Wind Direction", limits = c(0, 360), breaks = seq(0, 360, by = 30), type = "viridis")+  # Add legend scale
        # geom_point(data=CH4_Peaks[CH4_Peaks$panel == i, ], aes(x = UTC, y = X.CH4., col = "red"))
        geom_rect(data = CH4Data[CH4Data$Condition1 == 1 & CH4Data$panel == i, ], aes(xmin = UTC - 60, xmax = UTC + 60, ymin = -Inf, ymax = Inf),
                  ,fill = "blue", alpha = 0.2) #+
        # geom_rect(data = CH4Data[CH4Data$Condition2 == 1 & CH4Data$panel == i, ], aes(xmin = UTC - 60, xmax = UTC + 60, ymin = -Inf, ymax = Inf, fill = "green"),
        #           alpha = 0.2)+
        # geom_rect(data = CH4Data[CH4Data$Condition3 == 1 & CH4Data$panel == i, ], aes(xmin = UTC - 60, xmax = UTC + 60, ymin = -Inf, ymax = Inf, fill = "yellow"),
        #           alpha = 0.2)+
        # geom_rect(data = CH4Data[CH4Data$Condition4 == 1 & CH4Data$panel == i, ], aes(xmin = UTC - 60, xmax = UTC + 60, ymin = -Inf, ymax = Inf, fill = "red"),
        #           alpha = 0.2)+
        # scale_fill_manual(values = c("blue", "green","yellow", "red"),
        #                   labels = c("Good Conditions", "90-180°", "180-270°", "270-360°")) +  # Change the legend labels
        # guides(fill = guide_legend(title = "Wind Direction"))
      
      # Save the Plot
      ggsave(paste0("4_CH4_Timeline_Condition",i,".png"),
             CH4_TimeLine,
             path = "4_Data/OutputData/SecondPaper/Peak/CH4_Timeline",
             width = 10,
             height = 5)
    }
  }
  
  # Select a plot with seperate panels
  else if (Panel_Plot == TRUE){
    
    # Create the Plot
    CH4_TimeLine <- ggplot(CH4Data, aes(x = UTC, y = X.CH4.)) +
      geom_line() +
      labs(x = "Fill Time [UTC]",
           y = expression("CH"[4]*" concentration [ppb]"),
           title = "Methane concentration vs. Time") +
      scale_x_datetime(date_breaks = "2 day",
                       date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
      theme(axis.text.x=element_text(angle=60,
                                     hjust=1),
            strip.text.x = element_blank(),
            legend.position="none")+
      # geom_rect(data=CH4_Peaks, inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
      #                                                  ymax=CH4_max, fill=Direction), alpha=0.3)+ #, group=group
      # scale_fill_continuous(name = "Mean Wind Direction", limits = c(0, 360), breaks = seq(0, 360, by = 30), type = "viridis")+  # Add legend scale
      # geom_point(data=CH4_Peaks, aes(x = UTC, y = X.CH4., col = "red"))
      geom_rect(data = CH4Data[CH4Data$Condition == 1, ], aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.2)
    facet_wrap(~panel,
               scales = 'free',
               nrow = m)
    
    # Save the plot
    ggsave(paste0("4_CH4_Timeline_Panels_Condition.png"),
           CH4_TimeLine,
           path = "4_Data/OutputData/SecondPaper/Peak/CH4_Timeline",
           width = 10,
           height = 5)
  }
  
  
}



######## Finding the Peaks, The Average Meteorological Data during Peak, Saving csv File #########


CH4_TimeLine(TotalData, StartTime, FinishTime, 10, FALSE) #CH4_TimeLine(ImputDataFrame = , StartTime = , FinishTime =, MumberOfPanels = (0=FixedPanelForPaper), TURE = OnePlotMultiplePanels FALSE = MultipePlotsOnePanel)
