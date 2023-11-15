# Script to Combine all Data (Metro, Water, CH4) into one CSV File.
# Some Filtering is done 
# Author: Juan Bettinelli
# Last Change: 19.1.23


library(lubridate)
library(dplyr)
library(plotly)
library(rio)
# library(lubridate)
# library(readr)
# library(plyr)
# library(tidyverse)
library(ggplot2)   
library(hexbin)
library(gridExtra)
# library(reshape2)
# library(openair)
library(cowplot)
library(patchwork)
# library(dplyr)
library(GGally)
library(ggthemes)
library(ggvis)
# library(httr)
# library(plotly)
# library(rio)
# library(rmarkdown)
# library(shiny)
# library(stringr)
# library(tidyr)
# library(pracma)
library(areaplot)


#------------------------------------------------------------------------------------------------------------
# Function to split the Timeline into separate Plots/Panels

panel_function <- function(TotalData, n){
  # library("dplyr")
  
  # 0 is used in Campain paper, here Equal Sized Plots are produced regadles of the compleatness of the data
  if (n == 0){
    TotalData$panel[TotalData$dtm.end <= "2021-08-10 23:59:00"] <- 0
    TotalData$panel[TotalData$dtm.end >= "2021-08-11 00:00:00" & TotalData$dtm.end <= "2021-08-18 23:59:00"] <- 1
    TotalData$panel[TotalData$dtm.end >= "2021-08-19 00:00:00" & TotalData$dtm.end <= "2021-08-28 23:59:00"] <- 2
    TotalData$panel[TotalData$dtm.end >= "2021-08-29 00:00:00"] <- 3
    return(TotalData)
  } else{ # Automaticaly splits the timeline into panels/Plotts, n is the number of Panels
    TotalData <- TotalData %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData))*n))
    return(TotalData)
  }
}

#------------------------------------------------------------------------------------------------------------
# Function to split the Timeline into separate Plots/Panels

panel_function_TotalData <- function(TotalData, n){
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




# Function to Plot a CH4 Timeline with A Peak detection
EMPA_TimeLine_Concentration <- function(EMPA_csv = EMPA_csv, TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime, n) { #, n = 10, Panel_Plot = FALSE)
  
  EMPA_csv <- filter(EMPA_csv, EMPA_csv$dtm.end > StartTime & EMPA_csv$dtm.end < FinishTime, .preserve = FALSE)
  
  EMPA_csv$dtm.end <- as.POSIXct(EMPA_csv$dtm.end, 
                                 format = "%d-%m-%Y %H:%M:%S", 
                                 tz = "utc")
  
  #Split Timeline into Panels
  EMPA_Data <- panel_function(EMPA_csv, n)
  m <- panel_No_function(n)
  
  TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)
  
  TotalData$UTC <- as.POSIXct(TotalData$UTC, 
                              format = "%d-%m-%Y %H:%M:%S", 
                              tz = "utc")
  
  
  
  TotalData <- panel_function_TotalData(TotalData, n)
  m <- panel_No_function(n)
  
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  
  # Plot CH4, Waterlevel Vs Time
  CH4_TimeLine <- ggplot(EMPA_Data) +
    geom_line(aes(x = dtm.end,
                  y = CH4),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y =expression("EMPA CH"[4]*" concentration [ppb]"),
         title = "Methane concentration EMPS & IRMS  vs. time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    # limits = c(as.POSIXct('2021-08-01 00:00:00',
    #                       format = "%Y-%m-%d %H:%M:%S",
    #                       tz ="utc"), as.POSIXct('2021-08-18 00:00:00',
    #                                              format = "%Y-%m-%d %H:%M:%S",
    #                                              tz ="utc"))) +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(data = TotalData_CH4, aes(x = UTC,
                                        y = X.CH4.*1),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./1,
                                           name="IRMS"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  CH4_TimeLine
  
  #Export the plot to PNG file
  ggsave("24_EMPA_CH4.png", CH4_TimeLine, path = "4_Data/OutputData/Plots/24_EMPA", width = 10, height = 5)
  
}


#------------------------------------------------------------------------------------------------------------



# Function to Plot a CH4 Timeline with A Peak detection
CH4_TimeLine_total <- function(EMPA_csv = EMPA_csv, TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime) { #, n = 10, Panel_Plot = FALSE)
  
  # # calling funktions to splite timeline into Panels
  # TotalData <- panel_function(TotalData, n)
  # m <- panel_No_function(n)
  
  
  EMPA_csv <- filter(EMPA_csv, EMPA_csv$dtm.end > StartTime & EMPA_csv$dtm.end < FinishTime, .preserve = FALSE)
  
  EMPA_csv$dtm.end <- as.POSIXct(EMPA_csv$dtm.end, 
                                 format = "%d-%m-%Y %H:%M:%S", 
                                 tz = "utc")
  
  # #Split Timeline into Panels
  # EMPA_Data <- panel_function(EMPA_csv, n)
  # m <- panel_No_function(n)
  
  TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)
  
  TotalData$UTC <- as.POSIXct(TotalData$UTC, 
                              format = "%d-%m-%Y %H:%M:%S", 
                              tz = "utc")
  
  
  
  # TotalData <- panel_function_TotalData(TotalData, n)
  # m <- panel_No_function(n)
  
  
  
  #Select the Data from dataframe with CH4 Concentration
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]
  C13Data <- TotalData[complete.cases(TotalData[ , "d13C.VPDB"]),c("UTC", "d13C.VPDB")]
  DData <- TotalData[complete.cases(TotalData[ , "d2H.VPDB"]),c("UTC", "d2H.VPDB")]
  
  # Create the Timeline plot
  CH4_TL <- ggplot(CH4Data[, ], aes(x = UTC, y = X.CH4.)) +
    geom_line() +
    labs(
      y = expression("CH"[4]*" con. [ppb]")) +
    scale_x_datetime(date_breaks = "15 day",
                     date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
    geom_line(data = EMPA_csv, aes(x = dtm.end,
                                        y = CH4*1),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./1,
                                           name="EMPA"))+
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x = element_blank(),
              strip.text.x = element_blank(),
              legend.position="none",
              axis.title.y = element_text(color = "black",
                                          size=13),
              axis.text.y = element_text(color = "black"),
              axis.title.y.right = element_text(color = "blue",
                                                size=13),
              axis.text.y.right = element_text(color = "blue")) 

  
  CH4_13C_TimeLine <- ggplot(C13Data[, ], aes(x = UTC, y = d13C.VPDB)) +
    geom_line() +
    labs(
      y = expression("δ"^13*"C VPDB [‰]")) +
    scale_x_datetime(date_breaks = "15 day",
                     date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
    geom_line(data = EMPA_csv, aes(x = dtm.end,
                                   y = d13C_CH4*1),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./1,
                                           name="EMPA"))+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x = element_blank(),
          strip.text.x = element_blank(),
          legend.position="none",
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"))
  
  CH4_2H_TimeLine <- ggplot(DData[, ], aes(x = UTC, y = d2H.VPDB)) +
    geom_line() +
    labs(x = "Fill Time [UTC]",
         y = expression("δD VSMOW [‰]")) +
    scale_x_datetime(date_breaks = "15 day",
                     date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
    geom_line(data = EMPA_csv, aes(x = dtm.end,
                                   y = dD_CH4*1),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./1,
                                           name="EMPA"))+
    theme(axis.text.x=element_text(angle=60,
                                   hjust=1),
          axis.title.x=element_blank(),
          strip.text.x = element_blank(),
          legend.position="none",
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"))
    
  
  
  Total_Timeline <- CH4_TL + CH4_13C_TimeLine + CH4_2H_TimeLine +
    plot_layout(ncol = 1, guides = "collect")+
    plot_annotation(
      title = expression("CF-IRMS time series for methane concentration, δ"^13*"C and δD"))
  
  Total_Timeline
  
  # Save the Plot
  ggsave(paste0("24_EMPA_CH4_Isotope_Signature.png"),
         Total_Timeline,
         path = "4_Data/OutputData/Plots/24_EMPA",
         width = 10,
         height = 5)
}


#------------------------------------------------------------------------------------------------------------

# Function to Plot a CH4 Timeline with A Peak detection
EMPA_TimeLine_Sorces <- function(EMPA_csv = EMPA_csv, TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime, n) { #, n = 10, Panel_Plot = FALSE)
  
  EMPA_csv <- filter(EMPA_csv, EMPA_csv$dtm.end > StartTime & EMPA_csv$dtm.end < FinishTime, .preserve = FALSE)
  
  EMPA_csv$dtm.end <- as.POSIXct(EMPA_csv$dtm.end, 
                                 format = "%d-%m-%Y %H:%M:%S", 
                                 tz = "utc")
  
  #Split Timeline into Panels
  EMPA_Data <- panel_function(EMPA_csv, n)
  m <- panel_No_function(n)
  
  TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)
  
  TotalData$UTC <- as.POSIXct(TotalData$UTC, 
                              format = "%d-%m-%Y %H:%M:%S", 
                              tz = "utc")
  
  
  
  TotalData <- panel_function_TotalData(TotalData, n)
  m <- panel_No_function(n)
  
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  
  
  
  
  CH4_TimeLine <- ggplot(EMPA_Data) +
    geom_line(aes(x = dtm.end,
                  y = CH4_energy,
                  color = "CH4_energy")) +
    geom_line(aes(x = dtm.end,
                  y = CH4_residential,
                  color = "CH4_residential")) +
    geom_line(aes(x = dtm.end,
                  y = CH4_industrial,
                  color = "CH4_industrial")) +
    geom_line(aes(x = dtm.end,
                  y = CH4_natural_gas,
                  color = "CH4_natural_gas")) +
    geom_line(aes(x = dtm.end,
                  y = CH4_transport,
                  color = "CH4_transport")) +
    geom_line(aes(x = dtm.end,
                  y = CH4_waste,
                  color = "CH4_waste")) +
    geom_line(aes(x = dtm.end,
                  y = CH4_agriculture,
                  color = "CH4_agriculture")) +
    geom_line(aes(x = dtm.end,
                  y = CH4_bg,
                  color = "CH4_bg")) +
    geom_line(aes(x = dtm.end,
                  y = CH4,
                  color = "CH4")) +
    labs(x = "Time [UTC]",
         y = expression("EMPA CH"[4]*" concentration [ppb]"),
         title = "Methane concentration EMPS & IRMS vs. time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    scale_color_manual(name = "Legend", values = c("CH4_energy" = "blue", "CH4_residential" = "red", "CH4_industrial" = "green", "CH4_natural_gas" = "yellow", "CH4_transport" = "pink", "CH4_waste" = "brown", "CH4_agriculture" = "purple", "CH4_bg" = "lightblue", "CH4" = "black")) +
    facet_wrap(~panel, scales = 'free', nrow = m)
  
  CH4_TimeLine
  
  
  # Save the Plot
  ggsave(paste0("24_EMPA_CH4_Sorces.png"),
         CH4_TimeLine,
         path = "4_Data/OutputData/Plots/24_EMPA",
         width = 10,
         height = 5)
  
  
  # X-axis variable
  x <- EMPA_Data$dtm.end
  
  # Variables to be stacked
  y <- EMPA_Data[, c(3, 4, 5, 6, 7, 8, 9)]
  
  cols <- hcl.colors(7, palette = "viridis", alpha = 0.8)
  

  
  png("4_Data/OutputData/Plots/24_EMPA/24_EMPA_CH4_Sorces_Stacked_Area.png",
      width=1000, 
      height=600)
  
    # Stacked area chart
  areaplot(x, y,
           col = cols,
           main = "Stacked area chart EMPA Sorces",
           xlab = "UTC",
           ylab = "EMPA model concentration",
           legend = TRUE,
           args.legend = list(x = "topleft", cex = 0.65))

  dev.off()
  
  
  
  
  # X-axis variable
  x <- EMPA_Data$dtm.end
  
  # Variables to be stacked
  y <- EMPA_Data[, c(3, 4, 5, 6, 7, 8, 9, 10)]
  
  cols <- hcl.colors(7, palette = "viridis", alpha = 0.8)
  
  png("4_Data/OutputData/Plots/24_EMPA/24_EMPA_and_IRMS_CH4_Sorces_Stacked_Area.png",
      width=1000, 
      height=600)
  
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  areaplot(x, y,
           col = cols,
           main = "Stacked area chart EMPA Sorces",
           xlab = "UTC",
           ylab = "EMPA model concentration",
           legend = TRUE,
           args.legend = list(x = "topleft", cex = 0.65),
           ylim = c(1800, 4200))
  
  par(new = TRUE)
  plot(TotalData_CH4$UTC, TotalData_CH4$X.CH4.,
       type = "l",
       cex = 2,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(StartTime, FinishTime),
       ylim = c(1800, 4200))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("IRMS CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  dev.off()
  
}








n <- 1

#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automaticaly access the data.
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

StartTime <- as.POSIXct('2021-08-01 22:03:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2021-08-30 00:00:00', 
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

TotalData$Direction[TotalData$Direction > 361] <- NA
TotalData$Speed[TotalData$Speed > 99] <- NA



EMPA_csv <- import("4_Data/9_EMPA/ts_sim_concDelta_HH_GEO_IFS_FP-C1.csv")





EMPA_TimeLine_Concentration(EMPA_csv, TotalData, StartTime, FinishTime, n)


CH4_TimeLine_total(EMPA_csv, TotalData, StartTime, FinishTime)


EMPA_TimeLine_Sorces(EMPA_csv, TotalData, StartTime, FinishTime, n)







