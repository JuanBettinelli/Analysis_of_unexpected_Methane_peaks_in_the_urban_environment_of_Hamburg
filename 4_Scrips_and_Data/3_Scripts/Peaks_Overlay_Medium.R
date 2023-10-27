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
  
  
  
  
  # for (k in 1:nrow(CH4_Peaks)){
  #   if ((CH4_Peaks[k,4] - CH4_Peaks[k, 3]) < 3*6 ){
  #     CH4_Peaks[k, 3] <- CH4_Peaks[k, 3] - 2*6
  #     CH4_Peaks[k, 4] <- CH4_Peaks[k, 4] + 2*6
  #   }
  # }
  
  
  
  # Format the Peak Dataframe
  names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
  CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
  CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
  CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]
  
  
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
    write.csv(CH4_Peaks, "4_Data/OutputData/SecondPaper/Peak/CH4_Peaks_Large.csv", row.names=TRUE)
    write.csv(colMeans(CH4_Peaks[,c(1,5:29)], na.rm = TRUE), "4_Data/OutputData/SecondPaper/Peak/CH4_PeaksMean_Large.csv", row.names=TRUE)
    write.csv(colMeans(TotalData[,2:27], na.rm = TRUE), "4_Data/OutputData/SecondPaper/Peak/CH4_TotalMean_Large.csv", row.names=TRUE)
  }
  else {
    return(CH4_Peaks)
  }
}

#------------------------------------------------------------------------------------------------------------

# Function to Plot a CH4 Timeline with A Peak detection
CH4_TimeLine <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime, n = 10, Panel_Plot = FALSE){
  n <- 1
  # calling funktions to splite timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  #Select the Data from dataframe with CH4 Concentration
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "panel")]
  
  # Find the Methan Peaks
  CH4_Peaks <- CH4_Peak_Finder(TotalData, FALSE)
  CH4_Peaks$panel <- CH4Data[match(CH4_Peaks$UTC, CH4Data$UTC),"panel"]
  CH4_min <- min(CH4Data$X.CH4.)
  CH4_max <- max(CH4Data$X.CH4.)
  
  
  Indifidual_Peaks <- data.frame(
    Peak_No = numeric(0),
    CH4 = numeric(0),
    Time = character(0))
  u <- 1
  for (u in (1:nrow(CH4_Peaks))){
    testCH4  <- TotalData[(TotalData$UTC >= CH4_Peaks[u, "UTC"] - 4*60*60) & (TotalData$UTC <= CH4_Peaks[u, "UTC"] + 4*60*60) , "X.CH4." ]
    testTime <- as.numeric(difftime(TotalData[(TotalData$UTC >= CH4_Peaks[u, "UTC"] - 4*60*60) & (TotalData$UTC <= CH4_Peaks[u, "UTC"] + 4*60*60) ,"UTC" ], CH4_Peaks[u, "UTC"], units = "hours"))
    Indifidual_Peaks_single <- data.frame(
      Peak_No = u,
      CH4 = testCH4,
      Time = testTime
    )
    Indifidual_Peaks_single <- Indifidual_Peaks_single[complete.cases(Indifidual_Peaks_single[ , "CH4"]),c("Peak_No", "CH4", "Time")]
    Indifidual_Peaks <- rbind(Indifidual_Peaks, Indifidual_Peaks_single)
  }
  
  
  CH4_Overlay <- ggplot(Indifidual_Peaks, aes(Time, CH4, group=Peak_No, colour=Peak_No)) +
    geom_line()+
    labs(x = "Hours from Peak center",
         y = expression("CH"[4]*" concentration [ppb]"),
         title = "Methane concentration vs. Time")+
    scale_color_gradient(low="red", high="blue") #+
  #stat_summary(fun = mean, geom = "smooth", aes(group = 1), color = "black", size = 2)
  

  ggsave(paste0("4_CH4_Peaks_Overlay_Medium",".png"),
         CH4_Overlay,
         path = "4_Data/OutputData/SecondPaper/Overlay_Peaks",
         width = 20,
         height = 10)
}




######## Finding the Peaks, The Average Meteorological Data during Peak, Saving csv File #########
#CH4_Peak_Finder(TotalData, TRUE)

CH4_TimeLine(TotalData, StartTime, FinishTime, 1, FALSE) #CH4_TimeLine(ImputDataFrame = , StartTime = , FinishTime =, MumberOfPanels = (0=FixedPanelForPaper), TURE = OnePlotMultiplePanels FALSE = MultipePlotsOnePanel)

