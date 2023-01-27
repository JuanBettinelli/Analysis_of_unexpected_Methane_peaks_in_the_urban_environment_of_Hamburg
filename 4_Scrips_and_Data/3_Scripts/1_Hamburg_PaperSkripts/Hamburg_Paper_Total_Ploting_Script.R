
# Author:
# Juan Bettinelli, TU Munich, juan.bettinelli@tum.de

# Script to be used in the Paper "Quantification of methane emissions in Hamburg using a network of FTIR spectrometers and an inverse modeling approach"
# Data from the Hamburg campaign 2021-2022.
# This Script is used for a Keeling analysed and a Total Time series, with the data collected in Hamburg Geomatikum in 2021.

##### This Script is old and not in use??????

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


#Set Working Directory
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


#################### Isotope Data ###################

#Read Concentration Data of the CSV Files
CH4_2H <-read.csv2("./4_Data/2_Geomatikum_CH4_Concentrations/2_CH4Concentration(1.8.2021-17.09.2021)/CH4 2H 20210816.csv",TRUE, ";")
CH4_13C <-read.csv2("./4_Data/2_Geomatikum_CH4_Concentrations/2_CH4Concentration(1.8.2021-17.09.2021)/CH4 13C 20210816.csv",TRUE, ";")
CH4_concentrations <-read.csv2("./4_Data/2_Geomatikum_CH4_Concentrations/2_CH4Concentration(1.8.2021-17.09.2021)/CH4 concentrations 20210816.csv",TRUE, ";")

#Convert the date into a readable format
CH4_con_w_d <- CH4_concentrations
CH4_con_w_d$fill.time.utc <- as.POSIXct(CH4_con_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", 
                                        tz = "utc")
CH4_2H_w_d <- CH4_2H
CH4_2H_w_d$fill.time.utc <- as.POSIXct(CH4_2H_w_d$fill.time.utc,
                                       format = "%d.%m.%y %H:%M", 
                                       tz = "utc")
CH4_13C_w_d <- CH4_13C
CH4_13C_w_d$fill.time.utc <- as.POSIXct(CH4_13C_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", 
                                        tz = "utc")

################### Plotting the Data #######################
#Merge Data
TotalData <- data.frame()

TotalData <- merge( CH4_con_w_d, CH4_2H_w_d, 
                    by.x = "fill.time.utc",
                    by.y = "fill.time.utc",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

TotalData <- merge( TotalData, CH4_13C_w_d, 
                    by.x = "fill.time.utc",
                    by.y = "fill.time.utc",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

# Calculate 1/Mole Fraction
TotalData$c13C <- 1/TotalData$X.CH4..13C
TotalData$c2H <- 1/TotalData$X.CH4..2H

# Save the Data
write.csv(TotalData,"4_Data/OutputData/CombineCH4Data(1.8.2021-17.09.2021).csv", row.names = FALSE)

# Plot CH4 Concentration
p <- ggplot(TotalData, aes(x = fill.time.utc, y = X.CH4.)) +
        geom_line() + 
        labs(x = "Fill Time [UTC]", y ="CH4 mole fraction [ppb]", title = "Stationary in-Situ Measurement") +
        scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%m-%Y", limit=c(as.POSIXct("2021-08-01 22:00:00"),as.POSIXct("2021-09-06 00:00:00"))) +
        theme(axis.text.x=element_text(angle=60, hjust=1))
p

# Plot Keeling Plot
q <- ggplot(TotalData, aes(x = c13C, y = d13C.VPDB)) +
        geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
        expand_limits(x = 0) +
        geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
        labs(x = "Mole Fraction", y = "Isotopic Signatures", title = " Keeling Plot, 12C, δ(13)C (mean = -59.2‰ ± 0.15‰ s.e)") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      
k <- ggplot(TotalData, aes(x = c2H, y = d2H.VPDB)) +
      expand_limits(x = 0) +
      geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
      geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
      labs(x = "Mole Fraction", y = "Isotopic Signatures", title = " Keeling Plot, 2H, δ(2)H (mean = -304.4‰ ± 1.5‰ s.e.)") +
      theme(axis.text.x=element_text(angle=60, hjust=1))

grid.arrange(q,k, ncol = 2)

# Select data during campaign
TotalData_c <- TotalData[1:1874,]

# Keeling for total Time series
c13C_Line <- lm(d13C.VPDB ~ c13C, TotalData_c )
c13C_coef <- coef(summary(c13C_Line))[, "Estimate"]
c13C_se <- coef(summary(c13C_Line))[, "Std. Error"] 

c2H_Line <- lm(d2H.VPDB ~ c2H, TotalData_c )
c2H_coef <- coef(summary(c2H_Line))[, "Estimate"]
c2H_se <- coef(summary(c2H_Line))[, "Std. Error"] 

# cat("Total timeseries: \n 12C, δ(13)C (mean =", c13C_coef[[1]],"‰ ±", c13C_se[[1]],"‰ s.e; n = 1)")  
# cat("Total timeseries: \n 2H, δ(2)H  (mean =", c2H_coef[[1]],"‰ ±", c2H_se[[1]],"‰ s.e; n = 1)")


# Keeling for just Peaks in a continuous series
# The peaks:
# 1<- 69:72
# 2<- 326:332
# 3<- 448:455
# 4<- 567:575
# 5<- 626:639
# 6<- 775:778
# 7<- 805:808
# 8<- 1042:1046
p <- TotalData[c(69:72, 326:332, 448:455, 567:575, 626:639, 775:778, 805:808, 1042:1046),]
p_c13C_Line <- lm(d13C.VPDB ~ c13C, p )
p_c13C_coef <- coef(summary(p_c13C_Line))[, "Estimate"]
p_c13C_se <- coef(summary(p_c13C_Line))[, "Std. Error"]

p_c2H_Line <- lm(d2H.VPDB ~ c2H, p )
p_c2H_coef <- coef(summary(p_c2H_Line))[, "Estimate"]
p_c2H_se <- coef(summary(p_c2H_Line))[, "Std. Error"] 

# cat("Just Peaks: \n 12C, δ(13)C (mean = ", p_c13C_coef[[1]],"‰ ± ", p_c13C_se[[1]],"‰ s.e; n = 1)") 
# cat("Just Peaks:: \n 2H, δ(2)H  (mean =", p_c2H_coef[[1]],"‰ ±", p_c2H_se[[1]],"‰ s.e; n = 1)")

## Not Used
# Keeling for just peaks, Peaks analyes sepeatly
# segments <- list(69:72, 326:332, 448:455, 567:575, 626:639, 775:778, 805:808, 1042:1046)
# y_interseptions <- c()
# y_interseptions_2H <- c()
#  
# for (i in segments) {
#   q <- TotalData[i,]
#   q_line <- lm(d13C.VPDB ~ c13C, q )
#   q_c13C_coef <- coef(summary(q_line))[, "Estimate"]
#   y_interseptions <- c(y_interseptions, q_c13C_coef[[1]])
#   q_line_2H <- lm(d2H.VPDB ~ c2H, q )
#   q_c2H_coef <- coef(summary(q_line_2H))[, "Estimate"]
#   y_interseptions_2H <- c(y_interseptions_2H, q_c2H_coef[[1]])
#   }
#  
# standard_error <- function(x) sd(x) / sqrt(length(x))
# 
# SE_intersept <- standard_error(y_interseptions)
# M_intersept <- mean(y_interseptions)
# SE_intersept_2H <- standard_error(y_interseptions_2H)
# M_intersept_2H <- mean(y_interseptions_2H)
# cat("Just Peaks Seperate: \n 12C, δ(13)C (mean =", M_intersept,"‰ ±", SE_intersept,"‰ s.e; n =", length(y_interseptions),")") 
# cat("Just Peaks Seperate: \n 2H, δ(2)H  (mean =", M_intersept_2H,"‰ ±", SE_intersept_2H,"‰ s.e; n", length(y_interseptions_2H),")")


# Total Time series excluding the peaks
r <- TotalData_c[-c(69:72, 326:332, 448:455, 567:575, 626:639, 775:778, 805:808, 1042:1046),]
r_c13C_Line <- lm(d13C.VPDB ~ c13C, r )
r_c13C_coef <- coef(summary(r_c13C_Line))[, "Estimate"]
r_c13C_se <- coef(summary(r_c13C_Line))[, "Std. Error"] 
r_c2H_Line <- lm(d2H.VPDB ~ c2H, r )
r_c2H_coef <- coef(summary(r_c2H_Line))[, "Estimate"]
r_c2H_se <- coef(summary(r_c2H_Line))[, "Std. Error"] 
# cat("Exluding Peaks: \n 12C, δ(13)C (mean =", r_c13C_coef[[1]],"‰ ±", r_c13C_se[[1]],"‰ s.e; n = 1)") 
# cat("Exluding Peaks: \n 2H, δ(2)H (mean =", r_c2H_coef[[1]],"‰ ±", r_c2H_se[[1]],"‰ s.e; n = 1)")

## Not Used
# Time Series split in large segments
# Segments:
# 1<- 1:219
# 2<- 220:391
# 3<- 392:508
# 4<- 509:593
# 5<- 594:786
# 6<- 787:907
# 7<- 908:1874

# segments <- list(1:219, 220:391, 392:508, 509:593, 594:786, 787:907, 908:1874)
# z_interseptions <- c()
# z_interseptions_2H <- c()
# 
# for (i in segments) {
#   s <- TotalData[i,]
#   
#   s_line <- lm(d13C.VPDB ~ c13C, s )
#   s_c13C_coef <- coef(summary(s_line))[, "Estimate"]
#   z_interseptions <- c(z_interseptions, s_c13C_coef[[1]])
# 
#   s_line_2H <- lm(d2H.VPDB ~ c2H, s )
#   s_c2H_coef <- coef(summary(s_line_2H))[, "Estimate"]
#   z_interseptions_2H <- c(z_interseptions_2H, s_c2H_coef[[1]])
#   }
# standard_error <- function(x) sd(x) / sqrt(length(x))
# 
# z_SE_intersept <- standard_error(z_interseptions)
# z_M_intersept <- mean(z_interseptions)
# z_SE_intersept_2H <- standard_error(z_interseptions_2H)
# z_M_intersept_2H <- mean(z_interseptions_2H)
# cat("Large Segments: \n 12C, δ(13)C (mean =", z_M_intersept,"‰ ±", z_SE_intersept,"‰ s.e; n =", length(z_interseptions),")") 
# cat("Large Segments: \n 2H, δ(2)H  (mean =", z_M_intersept_2H,"‰ ±", z_SE_intersept_2H,"‰ s.e; n = ", length(z_interseptions_2H),")")


#####Show data######
message("\n \nTotal timeseries: \n 12C, δ(13)C (mean = ", c13C_coef[[1]],"‰ ± ", c13C_se[[1]],"‰ s.e; n = 1)","\n 2H, δ(2)H  (mean =", c2H_coef[[1]],"‰ ±", c2H_se[[1]],"‰ s.e; n = 1)")
message("Just Peaks: \n 12C, δ(13)C (mean = ", p_c13C_coef[[1]],"‰ ± ", p_c13C_se[[1]],"‰ s.e; n = 1)", " \n 2H, δ(2)H  (mean =", p_c2H_coef[[1]],"‰ ±", p_c2H_se[[1]],"‰ s.e; n = 1)")
message("Excluding the peaks: \n 12C, δ(13)C (mean =", r_c13C_coef[[1]],"‰ ±", r_c13C_se[[1]],"‰ s.e; n = 1)","\n 2H, δ(2)H (mean =", r_c2H_coef[[1]],"‰ ±", r_c2H_se[[1]],"‰ s.e; n = 1)")
#message("Just Peaks Seperate: \n 12C, δ(13)C (mean =", M_intersept,"‰ ±", SE_intersept,"‰ s.e; n = ", length(y_interseptions),")", "\n 2H, δ(2)H  (mean =", M_intersept_2H,"‰ ±", SE_intersept_2H,"‰ s.e;n = ", length(y_interseptions_2H),")") 
#message("Large Segments: \n 12C, δ(13)C (mean =", z_M_intersept,"‰ ±", z_SE_intersept,"‰ s.e; n =", length(z_interseptions),")"," \n 2H, δ(2)H  (mean =", z_M_intersept_2H,"‰ ±", z_SE_intersept_2H,"‰ s.e; n = ", length(z_interseptions_2H),")")

#sprintf(paste("Excluding the peaks: \n 12C, δ(13)C (mean =", M_intersept,"‰ ±", SE_intersept,"‰ s.e; n = 1)","\n 2H, δ(2)H (mean =", r_c2H_coef[[1]],"‰ ±", r_c2H_se[[1]],"‰ s.e; n = 1)"))

        