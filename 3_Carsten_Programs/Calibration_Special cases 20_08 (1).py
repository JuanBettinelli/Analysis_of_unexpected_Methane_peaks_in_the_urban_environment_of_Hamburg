# -*- coding: utf-8 -*-
"""
Created on Tue Sep  7 21:55:38 2021

@author: carst
Version 1.1 14.09.2021

The automated python Code should read in the start and end date which shall be analysed.
For this we need a fixed directoryy structure --> create a ReadMe to explain the structure

##

!!!At the 20.08 we did boat measurments with the G4302 Picarro. To evaluate this day we need merge the data separatly 

##
"""


"""
#The files of the mobile measurements of G2301, G4302 and the GPS file needs to be merged into one csv file.
#The merging follows the time column of all files. The time of G2301 and G4302 must be corrected by the time delay of the inlet (take from documentation)
#By merging the missing values (NaNs for GPS, Methane, CO2 and Ethane) will be linear interpolated from the closest neighboors.
"""

#import numpy as np
#import matplotlib.pyplot as plt
import pandas as pd
import os # Maybe used to automatically read all specific data in
import fnmatch 
import time
#import gpxpy as gp
from datetime import timedelta,datetime
#from gpxcsv import gpxtolist
import simplekml #needed to wrtie kml files
#from pathlib import Path # Maybe used to automatically read all specific data in

abspath = os.path.abspath('') ## String which contains absolute path to the script file
os.chdir(os.path.dirname(os.path.abspath('Calibration_1.1.py')))

start_time = time.time()



#Parameters editable by User
date_start = '20210820' #format YYYYMMDD as string
date_end = '20210820' #End da
#delayG23 = -1 #Delay of the instrument measurments --> substract from instrument time to calibrate the times of instrument and GPS
#delayG43 = 8

#delayG23_list=[3.5,1.5,2.5,2.5,-1,-0.5,0,0,-0.5,-0.5,0]
#delayG43_list=[6.5,7.5,8.5,8.5,10,11.5,12,15,14.5,17.5,17.5]
delayG23 = 15
delayG43 = 18.5


"""
Parameters we want to use of the Picarro files
"""



cols_G23 = ['DATE','TIME','ALARM_STATUS','species','CO2','CO2_dry','CH4','CH4_dry','H2O'] #Columns we want to use
cols_G43 = ['DATE','TIME','ALARM_STATUS','C2H6','C2H6_dry','CH4','CH4_dry','H2O', 'C2C1ratio']
cols_GPS_23 = ['time','latitude','longitude','accuracy (m)','altitude (m)','speed (m/s)']
cols_GPS_43 = ['date time','latitude','longitude','accuracy(m)','altitude(m)','speed(m/s)']
#_________________________________________________________________________________________________________
# Start of code
#Functions

"""
the calibration function is different for every type and instrument.
Here I define a function that takes the type of molecule and gas as an argument to use the correct formular
inst = ['G23','G43']
typ= ['CO2','CH4','C2H6']
"""
def calibration(x, inst, typ):
    if inst == 'G23':
        if typ == 'CH4':
            return 1.0014*x+0.005
        if typ == 'CO2':
            return 1.0088*x+0.320
    if inst =='G43':
        if typ == 'CH4':
            return 1.0043*x+0.033
        if typ == 'C2H6':
            return 0.9950*x

"""
Define a merge_interpolate function which takes two pandas Dataframes:
merges both together, sorts it after a column of your choice and then reset the index.
After this it should return the new dataframe where all NaN values are interpolated --> End result which we want

df1 = Dataframe 1
df2 = Dataframe 2
col = column to sort by as string 
"""
def merge_interpolate(df1,df2,col):
    outer = pd.merge(df1,df2,how='outer')
    outer.sort_values(by=col, inplace = True)
    outer.reset_index(drop=True, inplace=True)
    outer.interpolate(method='linear', axis = 0, inplace=True)
    #print(outer)
    return outer



"""
# original code from https://knpcode.com/python/fnmatch-module-python-file-name-pattern-matching/

Read in of GPS files in the GPS directory. This directory contains all GPS files which were measured during the campaign.
Use the starting date format to read in the specific GPS file with the Name : YYYYMMDD -*.gpx
The for loop stops as soon as it got a match for a specific date
"""
def gps_read(date,GPSpath,cols):
    #print('Search pattern is', pattern)
    files = os.listdir(GPSpath)
    #print('All Files:', files)
    for file in files:
        if fnmatch.fnmatch(file, date+'-*.txt'):
            GPS_ = pd.read_csv(GPSpath+file,usecols=cols)
            break
    return GPS_


#________________________________________________________________________________________________________

da1 = datetime.strptime(date_start,"%Y%m%d")
da2 = datetime.strptime(date_end,"%Y%m%d")

numdays = (da2-da1).days
date_list = [da1+ timedelta(days=x) for x in range(numdays+1)]

i=0
for da in date_list:
    """
    Setting working directory 
    """
    date = da.strftime('%Y%m%d')

    """
    Strip down date format for further use
    """
    #da = datetime.strptime(date_start,'%Y%m%d')
    day= da.strftime('%d')
    month = da.strftime('%m')
    year = da.strftime('%y')

    """
    #fixed parameters ! only change if change of directory is necessary!
    """
    GPSpath_G23 = "GPS\\Car\\"
    GPSpath_G43 = "GPS\\Boat\\"# directory of GPS data of the car
    G23_dir = "G2301\\" + day + "\\"
    G43_dir = "G4302\\" + day + "\\"


    """
    # reading in the files as a Dataframe 
    """
    G2301_prox=[]
    for file in os.listdir(G23_dir):
        if file.endswith(".dat"):
            df_prox = pd.read_csv(G23_dir+file, sep="\s+", usecols= cols_G23)
            G2301_prox.append(df_prox)

    G2301 = pd.concat(G2301_prox, ignore_index=True, sort=False)


    G4302_prox=[]
    for file in os.listdir(G43_dir):
        if file.endswith(".dat"):
            df_prox = pd.read_csv(G43_dir+file, sep="\s+", usecols= cols_G43)
            G4302_prox.append(df_prox)

    G4302 = pd.concat(G4302_prox, ignore_index=True, sort=False)
  

    """  
    #Read in GPS file from GPS folder of a specific date  
    """
    GPS_23 = gps_read(date,GPSpath_G23,cols_GPS_23)
    GPS_43 = gps_read(date,GPSpath_G43,cols_GPS_43)
    
    GPS_23.rename({'accuracy(m)':'accuracy','altitude(m)':'altitude','speed(m/s)':'speed'},axis=1, inplace=True)
    GPS_43.rename({'accuracy(m)':'accuracy','altitude(m)':'altitude','speed(m/s)':'speed'},axis=1, inplace=True)
#__________________________________________________________________________________________
    """
    # The time format of the GPS tracker is YYYY-MM-DDTHH:MM:SSZ. 
    #Separate Date and Time and add the new column DATE
    """
#DATE = []
#for s in range(len(GPS_.time)) :
#    x = GPS_.time[s]
#    x_list = x.split('T')
#    GPS_.loc[s,'time'] = x_list[1].split('Z')[0]
#    DATE.append(x_list[0]) 
        
#GPS = GPS_.assign(DATE = DATE)


    """
    reading the time values of all dataframes (GPS + Picarros) as a datetime.time object --> needed for the sorting
    """
    GPS_23['time'] = pd.to_datetime(GPS_23['time'], format='%Y-%m-%d %H:%M:%S').dt.time #creating a datetime column 
    GPS_23.rename({'time':'TIME'}, axis=1,inplace=True) # rename the column for consistancy
    
    GPS_43['date time'] = pd.to_datetime(GPS_43['date time'], format='%Y-%m-%d %H:%M:%S').dt.time #creating a datetime column 
    GPS_43.rename({'date time':'TIME'}, axis=1,inplace=True) # rename the column for consistancy
    #print(GPS['TIME'])
    G2301['TIME'] = (pd.to_datetime(G2301['TIME'], format='%H:%M:%S.%f') - timedelta(seconds=delayG23)).dt.time#creating a datetime column for the merging creating a datetime.time object
    G4302['TIME'] = (pd.to_datetime(G4302['TIME'], format='%H:%M:%S.%f') - timedelta(seconds=delayG43)).dt.time#creating a datetime column for the merging
    #G2301['DATE'] = pd.to_datetime(G2301['DATE'],format='%Y-%m-%d')
    """
    #calibrate the gas measurements and add it as a column to the dataframe --> keep the Raw measurements
    """
    G2301 = G2301.assign(CH4_cal = calibration(G2301['CH4_dry'],'G23','CH4'))
    G2301 = G2301.assign(CO2_cal = calibration(G2301['CO2_dry'],'G23','CO2'))

    G4302 = G4302.assign(CH4_cal = calibration(G4302['CH4_dry'],'G43','CH4'))
    G4302 = G4302.assign(C2H6_cal = calibration(G4302['C2H6_dry'],'G43','C2H6'))

    """
    #The Picarro data must be masked by the time values of the GPS data. 
    #All Piccaro points before and after the GPS tracking must be excluded.
    """
    G23_ = G2301[(G2301['TIME']>= GPS_23.TIME.iloc[0]) & (G2301['TIME']<= GPS_23['TIME'].iloc[-1])]
    G43_ = G4302[(G4302['TIME']>= GPS_43.TIME.iloc[0])&(G4302['TIME']<= GPS_43['TIME'].iloc[-1])]

    """
    # Merging the Piccaro files with the GPS files
    """
    G23 = merge_interpolate(G23_,GPS_23,'TIME')
    G43 = merge_interpolate(G43_,GPS_43,'TIME')
    
    
    G23['DATE'].fillna(value=da.strftime(format='%Y-%m-%d'), inplace=True)
    G43['DATE'].fillna(value=da.strftime(format='%Y-%m-%d'), inplace=True)

    """
    #save dataframes as a csv. file
    """
    G23.to_csv("Results\\G2301\\G2301_"+date+'.csv')
    G43.to_csv("Results\\G4302\\G4302_"+date+'.csv')
    i=i+1

print("--- %s seconds ---" % (time.time() - start_time))