
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
Keeling_Caluclation <- function(TotalData, start_time, H = 12) {
  # Select rows within the first 12 hours
  # selected_rows <- TotalData %>%
  #   filter(between(DateTime, start_time, start_time + hours(12)))
  TotalData <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "d13C.VPDB", "c13C", "d2H.VPDB", "c2H")]
  
  SelectedData <- filter(TotalData, TotalData$UTC >= start_time & TotalData$UTC < start_time + hours(H), .preserve = FALSE)

  if ((nrow(SelectedData[complete.cases(SelectedData[ , "d13C.VPDB"]),]) > 10) & (nrow(SelectedData[complete.cases(SelectedData[ , "d2H.VPDB"]),]) > 10)){
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
Keeling_Caluclation_Empa <- function(EMPA_csv, start_time, H = 12) {
  # Select rows within the first 12 hours
  # selected_rows <- TotalData %>%
  #   filter(between(DateTime, start_time, start_time + hours(12)))
  EMPA_csv <- EMPA_csv[complete.cases(EMPA_csv[ , "CH4"]),c("dtm.end", "CH4", "d13C_CH4", "c13C", "dD_CH4", "c2H")]
  
  SelectedData <- filter(EMPA_csv, EMPA_csv$dtm.end > start_time & EMPA_csv$dtm.end < start_time + hours(H), .preserve = FALSE)
  
  if ((nrow(SelectedData[complete.cases(SelectedData[ , "d13C_CH4"]),]) > 10) & (nrow(SelectedData[complete.cases(SelectedData[ , "dD_CH4"]),]) > 10)){
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

# Moving window in Hours
H <- 12

result_df <- data.frame(UTC = as.POSIXct(character()), c13C_coef = numeric(), c2H_coef = numeric())
CurrentTime <- StartTime

while (CurrentTime < FinishTime) {
  # Call the function to perform calculations and append to the result dataframe
  result <- Keeling_Caluclation(TotalData, CurrentTime, H)
  result_df <- bind_rows(result_df, result)
  
  # Move 1 hour forward
  CurrentTime <- CurrentTime + hours(1)
}

# hist(result_df$c13C_coef, main =  expression('Histogram of '*delta^13*'C in ‰'), xlab = expression(delta^13*'C in ‰'), ylab = "Number  with MKP", col = "blue", border = "black")
# hist(result_df$c2H_coef, main =  expression('Histogram of '*delta^2*'H in ‰'), xlab = expression(delta^2*'D in ‰'), ylab = "Number  with MKP", col = "red", border = "black")




result_df_EMPA <- data.frame(dtm.end = as.POSIXct(character()), c13C_coef_EMPA = numeric(), c2H_coef_EMPA = numeric())
CurrentTime <- StartTime

while (CurrentTime < FinishTime) {
  # Call the function to perform calculations and append to the result dataframe
  result <- Keeling_Caluclation_Empa(EMPA_csv, CurrentTime, H)
  result_df_EMPA <- bind_rows(result_df_EMPA, result)
  
  # Move 1 hour forward
  CurrentTime <- CurrentTime + hours(1)
}

# hist(result_df_EMPA$c13C_coef_EMPA, main =  expression('Histogram of '*delta^13*'C in ‰'), xlab = expression(delta^13*'C in ‰'), ylab = "Number  with MKP", col = "blue", border = "black")
# hist(result_df_EMPA$c2H_coef_EMPA, main =  expression('Histogram of '*delta^2*'H in ‰'), xlab = expression(delta^2*'D in ‰'), ylab = "Number  with MKP", col = "red", border = "black")

mean_Results <- data_frame() 
mean_Results[1,"c13C"] <- as.numeric(mean(result_df$c13C_coef, na.rm= TRUE)) 
mean_Results[1,"c13C_sd"] <- as.numeric(sd(result_df$c13C_coef, na.rm= TRUE)) 
mean_Results[1,"c2H"] <- as.numeric(mean(result_df$c2H_coef, na.rm= TRUE)) 
mean_Results[1,"c2H_sd"] <- as.numeric(sd(result_df$c2H_coef, na.rm= TRUE)) 

mean_Results[1,"c13C_EMPA"] <- as.numeric(mean(result_df_EMPA$c13C_coef_EMPA, na.rm= TRUE)) 
mean_Results[1,"c13C_sd_EMPA"] <- as.numeric(sd(result_df_EMPA$c13C_coef_EMPA, na.rm= TRUE)) 
mean_Results[1,"c2H_EMPA"] <- as.numeric(mean(result_df_EMPA$c2H_coef_EMPA, na.rm= TRUE)) 
mean_Results[1,"c2H_sd_EMPA"] <- as.numeric(sd(result_df_EMPA$c2H_coef_EMPA, na.rm= TRUE)) 

# Combine data into a data frame
df_C13 <- data.frame(value = c(result_df$c13C_coef, result_df_EMPA$c13C_coef_EMPA),
                 group = rep(c("IRMS Measument", "FLEXPART-COSMO Model")))

# Combine data into a data frame
df_2H <- data.frame(value = c(result_df$c2H_coef, result_df_EMPA$c2H_coef_EMPA),
                    group = rep(c("IRMS Measument", "FLEXPART-COSMO Model")))

# Create a grouped histogram with a different number of bins
Plot_13C <- ggplot(df_C13, aes(x = value, fill = group)) +
                    geom_histogram(position = "dodge", bins = 50, color = "black", alpha = 0.7) +
                    labs(title = expression('Moving window Keeling Plot Histogram of '*delta^13*'C in ‰'), x = expression(delta^13*'C in ‰'), y = "Frequency") +
                    scale_fill_manual(values = c("IRMS Measument" = "blue", "FLEXPART-COSMO Model" = "red"))+
                    theme(legend.position = "none")+
                    xlim(-65, -45)  # Set the x-axis limits


# Create a grouped histogram with a different number of bins
Plot_2H <- ggplot(df_2H, aes(x = value, fill = group)) +
                  geom_histogram(position = "dodge", bins = 50, color = "black", alpha = 0.7) +
                  labs(title = expression('Moving window Keeling Plot Histogram of '*delta^2*'H in ‰'), x = expression(delta^2*'H in ‰'), y = "Frequency") +
                  scale_fill_manual(
                    name = paste("Moving Window Size", H, "Hours"),  # Change the legend title
                    values = c("IRMS Measument" = "blue", "FLEXPART-COSMO Model" = "red")) +
                  theme(legend.position = "bottom")+
                  xlim(-350, -200)  # Set the x-axis limits

KeelingHistograms <- Plot_13C / Plot_2H

# Save and export the plot
ggsave("24_Moving_Keeling_Plot_Histograms.png", KeelingHistograms, path = "4_Data/OutputData/Plots/24_EMPA", width = 7, height = 12)



#--------------------------------------------------------------------------------


################ Keeling analyse ##############

# Keeling Analyse for total data of the champagne Time series
# For C13
c13C_Line_Total <- lm(d13C.VPDB ~ c13C, TotalData )
test <- coef(summary(c13C_Line_Total))[, "Estimate"]
mean_Results[1,"c13C_Total"] <- test[[1]]
test <- coef(summary(c13C_Line_Total))[, "Std. Error"] 
mean_Results[1,"c13C_sd_Total"] <- test[[1]]
# For H2
c2H_Line_Total <- lm(d2H.VPDB ~ c2H, TotalData )
test <- coef(summary(c2H_Line_Total))[, "Estimate"]
mean_Results[1,"c2H_Total"]  <- test[[1]]
test <- coef(summary(c2H_Line_Total))[, "Std. Error"] 
mean_Results[1,"c2H_sd_Total"] <- test[[1]]

################ Keeling analyse ##############

# Keeling Analyse for total data of the champagne Time series
# For C13
c13C_Line_EMPA_Total <- lm(d13C_CH4 ~ c13C, EMPA_csv )
test <- coef(summary(c13C_Line_EMPA_Total))[, "Estimate"]
mean_Results[1,"c13C_EMPA_Total"] <- test[[1]]
test <- coef(summary(c13C_Line_EMPA_Total))[, "Std. Error"] 
mean_Results[1,"c13C_sd_EMPA_Total"] <- test[[1]] 

# For H2
c2H_Line_EMPA_Total <- lm(dD_CH4 ~ c2H, EMPA_csv)
test <- coef(summary(c2H_Line_EMPA_Total))[, "Estimate"]
mean_Results[1,"c2H_EMPA_Total"] <- test[[1]]
test <- coef(summary(c2H_Line_EMPA_Total))[, "Std. Error"]
mean_Results[1,"c2H_sd_EMPA_Total"] <- test[[1]]

print(mean_Results)

#--------------------------------------------------------------------------------------------------------------



# Plotting the results in a combine isotope plot to identify emmittor type.
# regions are highlightet for the emmition type

## Create Ploygon to be used in the Plots
# Thermogenic
TH <- data.frame(x = c(-75, -40, -15, -40, -60), y = c(-350, -100, -150, -300, -350))
# Abiotic
A <- data.frame(x = c(-50, -50, -25, -10, -10), y = c(-450, -300, -50, -50, -450))


p0 <- ggplot() +
  geom_point(data=mean_Results, aes(x = c13C, y = c2H, color = "CF-IRMS MWK"), color = "red")+
  geom_errorbar(data=mean_Results, aes(x = c13C, xmin=c13C - c13C_sd, xmax= c13C - c13C_sd, y = c2H, ymin=c2H - c2H_sd, ymax=c2H + c2H_sd, colour="CF-IRMS MWK"), width=.02, alpha=0.5, color = "red") + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H, xmin=c13C - c13C_sd, xmax= c13C + c13C_sd, colour="CF-IRMS MWK"), width=.02, alpha=0.5, color = "red")+
  geom_point(data=mean_Results, aes(x = c13C_EMPA, y = c2H_EMPA, color = "FLEXPART-COSMO MWK"), color = "blue")+
  geom_errorbar(data=mean_Results, aes(x = c13C_EMPA, ymin=c2H_EMPA-c2H_sd_EMPA, ymax=c2H_EMPA+c2H_sd_EMPA, colour="FLEXPART-COSMO MWK"), width=.02, alpha=0.5, color = "blue") + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H_EMPA, xmin=c13C_EMPA - c13C_sd_EMPA, xmax= c13C_EMPA + c13C_sd_EMPA, colour="CF-IRMS MWK"), width=.02, alpha=0.5, color = "blue")+
  geom_point(data=mean_Results, aes(x = c13C_Total, y = c2H_Total, color = "CF-IRMS MWK"), color = "green")+
  geom_errorbar(data=mean_Results, aes(x = c13C_Total,  ymin=c2H_Total - c2H_sd_Total, ymax=c2H_Total + c2H_sd_Total, colour="CF-IRMS Total"), width=.02, alpha=0.5, color = "green") + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H_Total, xmin=c13C_Total - c13C_sd_Total, xmax= c13C_Total + c13C_sd_Total, colour="CF-IRMS Total"), width=.02, alpha=0.5, color = "green")+
  geom_point(data=mean_Results, aes(x = c13C_EMPA_Total, y = c2H_EMPA_Total, color = "FLEXPART-COSMO Total"), color = "purple")+
  geom_errorbar(data=mean_Results, aes(x = c13C_EMPA_Total, ymin=c2H_EMPA_Total - c2H_sd_EMPA_Total, ymax=c2H_EMPA_Total + c2H_sd_EMPA_Total, colour="FLEXPART-COSMO Total"), width=.02, alpha=0.5, color = "purple") + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H_EMPA_Total, xmin=c13C_EMPA_Total - c13C_sd_EMPA_Total, xmax= c13C_EMPA_Total + c13C_sd_EMPA_Total, colour="FLEXPART-COSMO Total"), width=.02, alpha=0.5, color = "purple")+
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
  geom_point(data=mean_Results, aes(x = c13C, y = c2H, color = "CF-IRMS MWK"))+
  geom_errorbar(data=mean_Results, aes(x = c13C, xmin=c13C - c13C_sd, xmax= c13C - c13C_sd, y = c2H, ymin=c2H - c2H_sd, ymax=c2H + c2H_sd, colour="CF-IRMS MWK")) + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H, xmin=c13C - c13C_sd, xmax= c13C + c13C_sd, colour="CF-IRMS MWK"))+
  geom_point(data=mean_Results, aes(x = c13C_EMPA, y = c2H_EMPA, color = "FLEXPART-COSMO MWK"))+
  geom_errorbar(data=mean_Results, aes(x = c13C_EMPA, ymin=c2H_EMPA-c2H_sd_EMPA, ymax=c2H_EMPA+c2H_sd_EMPA, colour="FLEXPART-COSMO MWK")) + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H_EMPA, xmin=c13C_EMPA - c13C_sd_EMPA, xmax= c13C_EMPA + c13C_sd_EMPA, colour="CF-IRMS MWK"))+
  geom_point(data=mean_Results, aes(x = c13C_Total, y = c2H_Total, color = "CF-IRMS MWK"))+
  geom_errorbar(data=mean_Results, aes(x = c13C_Total,  ymin=c2H_Total - c2H_sd_Total, ymax=c2H_Total + c2H_sd_Total, colour="CF-IRMS Total")) + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H_Total, xmin=c13C_Total - c13C_sd_Total, xmax= c13C_Total + c13C_sd_Total, colour="CF-IRMS Total"))+
  geom_point(data=mean_Results, aes(x = c13C_EMPA_Total, y = c2H_EMPA_Total, color = "FLEXPART-COSMO Total"))+
  geom_errorbar(data=mean_Results, aes(x = c13C_EMPA_Total, ymin=c2H_EMPA_Total - c2H_sd_EMPA_Total, ymax=c2H_EMPA_Total + c2H_sd_EMPA_Total, colour="FLEXPART-COSMO Total")) + #, position=pd
  geom_errorbarh(data=mean_Results, aes(y = c2H_EMPA_Total, xmin=c13C_EMPA_Total - c13C_sd_EMPA_Total, xmax= c13C_EMPA_Total + c13C_sd_EMPA_Total, colour="FLEXPART-COSMO Total"))+
  # scale_color_gradient2(name = "Wind Direction, °", midpoint=180, low="blue", mid="red",
  #                       high="blue", space ="Lab" )+
  scale_color_manual(name = "Keeling", values = c("CF-IRMS MWK" = "red", "FLEXPART-COSMO MWK" = "blue", "CF-IRMS Total" = "green", "FLEXPART-COSMO Total" = "purple"))+
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
ggsave("24_Dual_Isotopes_Total_MWK.png", Total_Plot, path = "4_Data/OutputData/Plots/24_EMPA", width = 12, height = 5.2)


