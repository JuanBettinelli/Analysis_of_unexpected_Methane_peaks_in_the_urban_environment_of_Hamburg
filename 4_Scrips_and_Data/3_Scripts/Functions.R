# Scripts with functions used in Plotting_With_Compleate_CSV_File_Data.R
# Author: Juan Bettinelli


panel_function <- function(TotalData, n){
  if (n == 0){
    #for fixed panel
    TotalData$panel[TotalData$UTC <= "2021-08-10 23:59:00"] <- 0
    TotalData$panel[TotalData$UTC >= "2021-08-11 00:00:00" & TotalData$UTC <= "2021-08-18 23:59:00"] <- 1
    TotalData$panel[TotalData$UTC >= "2021-08-19 00:00:00" & TotalData$UTC <= "2021-08-28 23:59:00"] <- 2
    TotalData$panel[TotalData$UTC >= "2021-08-29 00:00:00"] <- 3
    return(TotalData)
  }
  else{
    #for automatic panel
    TotalData <- TotalData %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData))*n))
    return(TotalData)
  }
}
panel_No_function <- function(n){
  if (n == 0){
    m <- 1
    return(m)
  }
  else{
    m <- n
    return(m)
  }
}



Compare_Timeline <- function(TotalData, n ) {
  # Filter Data frame
  TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  
  if (n == 0){
    # fixed panel
    TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC <= "2021-08-10 23:59:00"] <- 0
    TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC >= "2021-08-11 00:00:00" & TotalData_CH4_WL$UTC <= "2021-08-18 23:59:00"] <- 1
    TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC >= "2021-08-19 00:00:00" & TotalData_CH4_WL$UTC <= "2021-08-28 23:59:00"] <- 2
    TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC >= "2021-08-29 00:00:00"] <- 3
    m <- 4
  }
  # for automatic panel
  else{
    # for automatic panel
    TotalData_CH4_WL <- TotalData_CH4_WL %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData_CH4_WL))*n))
    m <- n
  }
  
  # Plot CH4, Waterlavel & Time
  CH4_TimeLine <- ggplot(TotalData_CH4_WL) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y ="CH4 Concentration [ppb]",
         title = "CH4 Concentration & Elbe Waterlevel vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = Water_Level*5),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./5,
                                           name="Water Level, mm"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  CH4_TimeLine
  
  #Export the plot to PNG file
  ggsave("1_CH4_WL.png", CH4_TimeLine, path = "4_Data/OutputData/Plots", width = 10, height = 5)
  
  
  # Filter Data frame for Wind
  TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Wind_Direction", "Wind_Speed")]),]
  
  if (n == 0){
    #for fixed panel
    TotalData_Wind$panel[TotalData_Wind$UTC <= "2021-08-10 23:59:00"] <- 0
    TotalData_Wind$panel[TotalData_Wind$UTC >= "2021-08-11 00:00:00" & TotalData_Wind$UTC <= "2021-08-18 23:59:00"] <- 1
    TotalData_Wind$panel[TotalData_Wind$UTC >= "2021-08-19 00:00:00" & TotalData_Wind$UTC <= "2021-08-28 23:59:00"] <- 2
    TotalData_Wind$panel[TotalData_Wind$UTC >= "2021-08-29 00:00:00"] <- 3
    n <- 4
  }
  else{
  #for automatic panel
    TotalData_Wind <- TotalData_Wind %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData_Wind))*n))
    m <- n
  }
  # Plot Wind, speed, dirction vs time
  
  Wind_TimeLine <- ggplot(TotalData_Wind) +
    geom_line(aes(x = UTC,
                  y = Wind_Direction),
              col = "black") +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Speed & Wind Direction vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "purple",
                                            size=13),
          axis.text.y.right = element_text(color = "purple"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = Wind_Speed*70),
              col = "purple") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./70,
                                           name="Wind Speed, m/s"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  Wind_TimeLine
  
  #Export the plot ti PNG file
  ggsave("2_Wind_D_S.png", Wind_TimeLine, path = "4_Data/OutputData/Plots", width = 10, height = 5)
  
  
  
  # Only wind Direction
  Wind_Direction_TimeLine <- ggplot(TotalData_Wind) +
    geom_line(aes(x = UTC,
                  y = Wind_Direction),
              col = "black") +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Direction vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "purple",
                                            size=13),
          axis.text.y.right = element_text(color = "purple"),
          strip.text.x = element_blank()) +
    # geom_line(aes(x = UTC,
    #               y = Wind_Speed*70),
    #           col = "green") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./70,
                                           name="Wind Speed, m/s"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  Wind_Direction_TimeLine
  
  # Export Plot to PNG file
  ggsave("2.1_Wind_D.png", Wind_Direction_TimeLine, path = "4_Data/OutputData/Plots", width = 10, height = 5)
  
  
  
  
  # Plot Wind Speed
  Wind_Speed_TimeLine <- ggplot(TotalData_Wind) +
    # geom_line(aes(x = UTC,
    #               y = Wind_Direction),
    #           col = "black") +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Speed vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "purple",
                                            size=13),
          axis.text.y.right = element_text(color = "purple"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = Wind_Speed*70),
              col = "purple") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./70,
                                           name="Wind Speed, m/s"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  Wind_Speed_TimeLine
  
  # Export Plot to PNG file
  ggsave("2.2_Wind_S.png", Wind_Speed_TimeLine, path = "4_Data/OutputData/Plots", width = 10, height = 5)
  
  
  # Plot Wind, speed, dirction vs time
  options(ggplot2.continuous.colour="viridis")
  Wind_TimeLine <- ggplot(TotalData_Wind, aes(x = UTC,
                                              y = Wind_Direction,
                                              colour = Wind_Speed)) +
    geom_line() +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Speed & Wind Direction vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          strip.text.x = element_blank()) +
    facet_wrap(~panel, scales = 'free', nrow = n) +
    guides(color = guide_legend(title = "Wind Speed, m/s"))
  Wind_TimeLine
  
  #Export the plot ti PNG file
  ggsave("3_Wind_D_S.png", Wind_TimeLine, path = "4_Data/OutputData/Plots", width = 10, height = 5)

  
  if (n == 0){
    #for fixed panel
    TotalData$panel[TotalData$UTC <= "2021-08-10 23:59:00"] <- 0
    TotalData$panel[TotalData$UTC >= "2021-08-11 00:00:00" & TotalData$UTC <= "2021-08-18 23:59:00"] <- 1
    TotalData$panel[TotalData$UTC >= "2021-08-19 00:00:00" & TotalData$UTC <= "2021-08-28 23:59:00"] <- 2
    TotalData$panel[TotalData$UTC >= "2021-08-29 00:00:00"] <- 3
    m<-1
  }
  else{
    #for automatic panel
    TotalData <- TotalData %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData))*n))
    m <- n
  }
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),c("UTC", "X.CH4.","panel")]
  TotalData_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "Water_Level")]),c("UTC", "Water_Level","panel")]
  TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Direction", "Speed")]),c("UTC", "Direction", "Speed","panel")]

  for(i in 0:(m-1)){
    p1 <- ggplot(TotalData_CH4[TotalData_CH4$panel == i,], aes(x = UTC,
                                                               y = X.CH4.)) + 
      ylim(1600, 4300) +
      labs(y ="CH4 Concentration")+
      geom_line() +
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 0))
    p1
    
    p2 <- ggplot(TotalData_WL[TotalData_WL$panel == i,], aes(x = UTC,
                                                             y = Water_Level)) +
      geom_line() +
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 0))
    p2
    
    p3 <- ggplot(data = TotalData_Wind[TotalData_Wind$panel == i,], aes(x = UTC, y = Direction)) +
      geom_line(aes(color = "Wind Dircection")) + 
      ylim(0, 360) +
      labs(x = "UTC",
           y ="Wind Direction, °",
           title = "Wind Direction, Waterlevel, CH4 Concentration vs. Time") +
      geom_line(data = TotalData_WL[TotalData_WL$panel == i,], aes(x = UTC, y = (Water_Level/1.5-200), color = "Water Level")) +
      scale_x_datetime(date_breaks = "1 day",
                       date_labels = "%d-%b")+
      geom_line(TotalData_CH4[TotalData_CH4$panel == i,], mapping = aes(x = UTC, y = (X.CH4./7-250) , color = "CH4.")) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~(.*1.5+200),
                                             name="Waterlevel, mm"))+
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
    
    p4 <- wrap_elements(get_plot_component(p1, "ylab-l")) +
      wrap_elements(get_y_axis(p1)) +
      # wrap_elements(get_plot_component(p2, "ylab-l")) +
      # wrap_elements(get_y_axis(p2)) +
      p3 + 
      plot_layout(widths = c(1, 1, 40))
    p4
    
    ggsave(paste0("5_CH4_Wl_WD_",i,".png"), p4, path = "4_Data/OutputData/Plots", width = 10, height = 5)
  }
  
  
  }







Compare_Timeline_Basic <- function(TotalData) {
  # Basic Plot for CH4 vs Waterlevel
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
  TotalData_CH4 <- TotalData_CH4[,c("UTC", "X.CH4.", "Water_Level")]
  WL_CH4_Data <- melt(TotalData_CH4, id.var="UTC")
  
  png(file="4_Data/OutputData/Plots/4_Basic_Plot_CH4_Wl.png",
      width=600, height=350)
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  plot(TotalData_CH4$UTC, TotalData_CH4$Water_Level,
       type = "l",
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Elbe Waterlevel, mm",
       xlim = c(StartTime, FinishTime))
  
  par(new = TRUE)
  plot(TotalData_CH4$UTC, TotalData_CH4$X.CH4.,
       main = "WaterLevel(WSV)/CH4 Concentation Vs. Time",
       type = "l",
       cex = 2,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(c(StartTime, FinishTime)))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
  
}


CH4_TimeLine <- function(TotalData, StartTime, FinishTime, n){
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
    
  CH4_TimeLine <- ggplot(CH4Data, aes(x = UTC, y = X.CH4.)) +
    geom_line() +
    labs(x = "Fill Time [UTC]",
         y ="CH4 mole fraction [ppb]",
         title = "CH4 mole fraction vs. Time") +
    scale_x_datetime(date_breaks = "2 day",
                     date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
    theme(axis.text.x=element_text(angle=60,
                                   hjust=1),
          strip.text.x = element_blank())+
    facet_wrap(~panel,
               scales = 'free',
               nrow = m)
  
  ggsave(paste0("4_CH4_Timeline.png"),
         CH4_TimeLine,
         path = "4_Data/OutputData/Plots",
         width = 10,
         height = 5)
  
}



Basic_Wind_DWD_CH4 <- function(TotalData, StartTime, FinishTime){
  png(file="4_Data/OutputData/Plots/6_Basic_Plot_CH4_Wind_DWD.png",
      width=600,
      height=350)
  par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Wind_Direction,
       main = "Wind Direction & Speed (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Direction, °",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Wind_Speed,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Speed, m/s",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  par(mfrow=c(1,1))
  dev.off() 
}
Basic_Wind_D_WL_CH4 <- function(TotalData, StartTime, FinishTime){ 
  png(file="4_Data/OutputData/Plots/6_Basic_Plot_CH4_Wind_DWD_Waterlevel.png",
      width=600,
      height=350)
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Water_Level,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Elbe Waterlevel, mm",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
       type = "p",
       pch='.',
       lwd = 1.5,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$Wind_Direction,
       type = "p",
       pch='.',
       cex = 2,
       col="Blue",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  axis(side=4,
       col.axis="blue",
       col="blue")
  mtext("CH4 Concentration & Wind Direction, °",
        col="red",
        side=4,
        line=3)
  dev.off() 
  
}



Basic_Wind_110m_CH4 <- function(TotalData, StartTime, FinishTime){

  png(file="4_Data/OutputData/Plots/6_Basic_Plot_CH4_Wind_110m.png",
      width=600,
      height=350)
  
  par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  # first plot
    plot(TotalData$UTC, TotalData$Direction110m,
         main = "Wind Direction & Speed (MAST 110m)/CH4 Concentation Vs. Time",
         type = "p",
         pch='.',
         cex = 2,
         xlab = "Date/Time UTC",
         ylab = "Wind Direction, °",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$X.CH4.,
         type = "p",
         pch='.',
         col="red",
         axes = FALSE,
         bty = "n",
         xlab = "",
         ylab = "",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
    
    axis(side=4,
         col.axis="red",
         col="red")
    mtext("CH4 Concentration",
          col="red",
          side=4,
          line=3)
    
    par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
    # first plot
    plot(TotalData$UTC, TotalData$Speed110m,
         type = "p",
         pch='.',
         cex = 2,
         xlab = "Date/Time UTC",
         ylab = "Wind Speed, m/s",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$X.CH4.,
         type = "p",
         pch='.',
         col="red",
         axes = FALSE,
         bty = "n",
         xlab = "",
         ylab = "",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
    
    axis(side=4,
         col.axis="red",
         col="red")
    mtext("CH4 Concentration",
          col="red",
          side=4,
          line=3)
    par(mfrow=c(1,1))
    par(mfrow=c(1,1))
    dev.off() 
}


Basic_Wind_Geomatikum_CH4 <- function(TotalData, StartTime, FinishTime){
  
  png(file="4_Data/OutputData/Plots/6_Basic_Plot_CH4_Wind_Geomatikum.png",
      width=600,
      height=350)

  par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Direction,
       main = "Wind Direction (Geomatikum)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Direction, °",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  
  par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Speed,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Speed, m/s",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  par(mfrow=c(1,1))
  dev.off() 
}

Basic_CH4_WaterLevel_Wind_Speed <- function(TotalData){
  
    IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
    i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
    
    ticks <- seq(from=min(TotalData$UTC), by='1 days', length=50)
    
    k <- 1
    for(j in IntervalDate){
      png(file=paste0("4_Data/OutputData/Plots/7_Basic_Plot_CH4_WL_Speed_",k,".png"),
          width=600,
          height=350)
      par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
      # first plot
      plot(TotalData$UTC, TotalData$Water_Level,
           type = "p",
           pch='.',
           cex = 2,
           # lwd = 1,
           bty = "n",
           axes = FALSE,
           # xlab = "Date/Time UTC",
           # ylab = "Elbe Waterlevel, mm",
           xlim = c(i,j))
      axis(side=2,
           col.axis="Black",
           col="Black",
           ylab = "Elbe Waterlevel, mm")
      
      par(new = TRUE)
      plot(TotalData$UTC, TotalData$X.CH4.,
           main = "WaterLevel(WSV)/CH4 Concentation/Wind Speed (DWD) Vs. Time",
           type = "p",
           pch='.',
           cex = 2,
           # lwd = 1.5,
           col="red",
           axes = FALSE,
           bty = "n",
           xlab = "",
           ylab = "",
           xlim = c(i,j))
      
      axis(side=4,
           col.axis="red",
           col="red")
      axis.POSIXct(side=1,
                   at = ticks,
                   #labels=format(TotalData$UTC,"%Y-%m-%d"),
                   las=2)
      
      
      par(new = TRUE)
      plot(TotalData$UTC, TotalData$Wind_Speed,
           type = "p",
           pch='.',
           cex = 2,
           col="Blue",
           axes = FALSE,
           bty = "n",
           xlab = "",
           ylab = "",
           xlim = c(i,j))
      axis(side=4,
           col.axis="blue",
           col="blue")
      mtext("CH4 Concentration & Wind Speed",
            col="red",
            side=4,
            line=3)
      dev.off() 
      i <- j
      k <- k+1
    }
    
}




Basic_CH4_WaterLevel_Wind_Direction <- function(TotalData){
  
  IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
  i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
  
  ticks <- seq(from=min(TotalData$UTC), by='1 days', length=50)
  k <- 1
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  for(j in IntervalDate){
    png(file=paste0("4_Data/OutputData/Plots/7_Basic_Plot_CH4_WL_Direction_",k,".png"),
        width=600,
        height=350)
    # first plot
    plot(TotalData$UTC, TotalData$Water_Level,
         type = "p",
         pch='.',
         cex = 1,
         # lwd = 1,
         bty = "n",
         axes = FALSE,
         # xlab = "Date/Time UTC",
         # ylab = "Elbe Waterlevel, mm",
         xlim = c(i,j))
    axis(side=2,
         col.axis="Black",
         col="Black",
         las = 1,
         ylab = "Elbe Waterlevel, mm")
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$X.CH4.,
         # main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
         type = "p",
         pch='.',
         cex = 3,
         col="red",
         axes = FALSE,
         xlab = "",
         ylab = "",
         xlim = c(i,j))
    
    axis(side=4,
         col.axis="red",
         col="red",
         las = 1)
    
    axis.POSIXct(side=1,
                 at = ticks,
                 #labels=format(TotalData$UTC,"%Y-%m-%d"),
                 las=2)
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$Wind_Direction,
         type = "p",
         pch='.',
         cex = 3,
         col="Blue",
         axes = FALSE,
         xlab = "",
         ylab = "",
         xlim = c(i,j))
    axis(side=4,
         col.axis="blue",
         col="blue",
         las = 1)
    mtext("CH4 Concentration & Wind Direction, °",
          col="red",
          side=4,
          line=3)
    mtext("WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time", side = 3, line = - 2, outer = TRUE)
    
    dev.off() 
    i <- j
    k <- k+1
  }
}


Basic_Rain_CH4 <- function(TotalData, StartTime, FinishTime){
  
  png(file="4_Data/OutputData/Plots/8_Basic_Plot_CH4_Rain.png",
      width=600,
      height=350)
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$precipitation_height,
       main = "Rain quantity (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Rain Quantity, mm",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
}

Basic_Temp_CH4 <- function(TotalData, StartTime, FinishTime){
  
  png(file="4_Data/OutputData/Plots/8_Basic_Plot_CH4_Temp.png",
      width=600,
      height=350)
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$temperature_air_mean_200,
       main = "Temperature (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Temperature, °C",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
}


Basic_Humidity_CH4 <- function(TotalData, StartTime, FinishTime){
  
  png(file="4_Data/OutputData/Plots/8_Basic_Plot_CH4_Humidity.png",
      width=600,
      height=350)
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$humidity,
       main = "Relative humidity (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Relative humidity, g/m3",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
}