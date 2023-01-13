setwd("/Volumes/~ge95fim/TUM-PC/Dokumente/Hamburg_Campaign/00_Isotrope Plots")

#Read Concentration Data of the CSV Files
CH4_2H <-read.csv2("CH4 2H 20210816.csv",TRUE, ";")
CH4_13C <-read.csv2("CH4 13C 20210816.csv",TRUE, ";")
CH4_concentrations <-read.csv2("CH4 concentrations 20210816.csv",TRUE, ";")


#Convert the date into a readable format
CH4_con_w_d <- CH4_concentrations
CH4_con_w_d$fill.time.utc <- as.POSIXlt(CH4_con_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", tz = "utc")
CH4_2H_w_d <- CH4_2H
CH4_2H_w_d$fill.time.utc <- as.POSIXlt(CH4_2H_w_d$fill.time.utc,
                                       format = "%d.%m.%y %H:%M", tz = "utc")
CH4_13C_w_d <- CH4_13C
CH4_13C_w_d$fill.time.utc <- as.POSIXlt(CH4_13C_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", tz = "utc")


par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# first plot
plot(CH4_con_w_d$fill.time.utc, CH4_con_w_d$X.CH4.,
     main = "CH4 mole fraction [ppb]",
     type = "l",
     col="black",
     cex = 5,
     xlab = "Date/Time UTC",
     ylab = "X CH4 [ppb]",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(CH4_2H_w_d$fill.time.utc, CH4_2H_w_d$X.CH4..2H,
     type = "l",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(CH4_13C_w_d$fill.time.utc, CH4_13C_w_d$X.CH4..13C,
     type = "l",
     col="blue",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

legend(x = "topright",          
       legend=c("Total", "13C", "2H"),  
       inset = c(-0.3, 0),
       lty = c(1,1,1),           
       col = c("black", "red", "blue"),          
       lwd = 1,
       xpd = TRUE)                 

par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
#first plot
plot(CH4_2H_w_d$fill.time.utc, CH4_2H_w_d$d2H.VPDB,
     main = "CH4 isotopes [‰]",
     type = "l",
     col="red",
     cex = 5,
     xlab = "Date/Time UTC",
     ylab = "CH4 isotopes [‰]",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))


par(new = TRUE)
plot(CH4_13C_w_d$fill.time.utc, CH4_13C_w_d$d13C.VPDB,
     type = "l",
     col="blue",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

# legend(1, 95, legend=c("Total", "13C", "2H"),
#        col=c("black", "red", "blue"), lty=1:2, cex=0.8)


