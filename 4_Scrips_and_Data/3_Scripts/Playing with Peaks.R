# Not working yet!!!!!!



setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")

CH4_Peaks <- import("4_Data/OutputData/CH4_Peaks.csv")
CH4_Peaks$UTC <- as.POSIXct(as.character(CH4_Peaks$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
CH4_Peaks$UTC_Beginning <- as.POSIXct(as.character(CH4_Peaks$UTC_Beginning), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
CH4_Peaks$UTC_Ending <- as.POSIXct(as.character(CH4_Peaks$UTC_Ending), 
                                      format = "%Y-%m-%d %H:%M:%S", 
                                      tz = "UTC")

# 
# for(i in 1:length(CH4_Peaks)){
#   Compare_Timeline(TotalData, n ) # (TotalData, CH4_Peaks[i,"UTC_Beginning"], CH4_Peaks[i,"UTC_Ending"])
# }
# 




# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots

  # replace Error points with NA
  is.na(TotalData$Wind_Speed) <- TotalData$Wind_Speed == "-999"
  is.na(TotalData$Water_Level) <- TotalData$Water_Level == "-777"


  # Split the TotalData Dataframe into seperate Datatframes, ther are used indifidualy to plot them in same Graph
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),c("UTC", "X.CH4.")]
  TotalData_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "Water_Level")]),c("UTC", "Water_Level")]
  TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Direction", "Speed")]),c("UTC", "Direction", "Speed")]
  
  # With in the Loop the timeline is split into multiple Plots
  for(i in 1:length(CH4_Peaks)){
    # First plot only created to tse the Axsi
    p1 <- ggplot(TotalData_CH4, aes(x = UTC,
                                                               y = X.CH4.)) +
      ylim(1600, 4300) +
      xlim(CH4_Peaks[i,"UTC_Beginning"], CH4_Peaks[i,"UTC_Ending"])+
      labs(y ="CH4 Concentration")+
      geom_line() +
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 0))
    p1
    
    # Second Plot only created to use the axis
    p2 <- ggplot(TotalData_WL, aes(x = UTC,
                                                             y = Water_Level)) +
      geom_line() +
      xlim(CH4_Peaks[i,"UTC_Beginning"], CH4_Peaks[i,"UTC_Ending"])+
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 0))
    p2

    # Plot inclues all Timelines
    p3 <- ggplot(data = TotalData_Wind, aes(x = UTC, y = Direction)) +
      geom_line(aes(color = "Wind Dircection")) +
      labs(x = "UTC",
           y ="Wind Direction, °",
           title = "Wind Direction, Waterlevel, CH4 Concentration vs. Time") +
      geom_line(data = TotalData_WL, aes(x = UTC, y = (Water_Level/1.5-200), color = "Water Level")) +
      scale_x_datetime(date_breaks = "1 day",
                       date_labels = "%d-%b")+
      geom_line(TotalData_CH4, mapping = aes(x = UTC, y = (X.CH4./7-250) , color = "CH4.")) +
      # scale_y_continuous(sec.axis = sec_axis(trans = ~(.*1.5+200),
      #                                        name="Waterlevel, mm"))+
      ylim(0, 360) +
      xlim(CH4_Peaks[i,"UTC_Beginning"], CH4_Peaks[i,"UTC_Ending"])+
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 20),
            axis.text.x=element_text(angle=60, hjust=1),
            axis.title.y = element_text(color = "black",
                                        size=13),
            axis.text.y = element_text(color = "black"),
            strip.text.x = element_blank(),
            legend.position = "bottom",
            legend.title=element_blank())
    p3

    # the Axis from the first to Plots is now includet in the third plot
    p4 <- wrap_elements(get_plot_component(p1, "ylab-l")) +
      wrap_elements(get_y_axis(p1)) +
      # wrap_elements(get_plot_component(p2, "ylab-l")) +
      # wrap_elements(get_y_axis(p2)) +
      p3 +
      plot_layout(widths = c(1, 1, 40))
    p4

    #Save the plot
    ggsave(paste0("5_CH4_WaterLevel_WindDirection_Peaks_",i,".png"), p4, path = "4_Data/OutputData/Plots", width = 10, height = 5)
  }
