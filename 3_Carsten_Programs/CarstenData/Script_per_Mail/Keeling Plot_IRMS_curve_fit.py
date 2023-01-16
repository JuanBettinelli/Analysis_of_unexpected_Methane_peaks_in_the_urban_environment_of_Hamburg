# -*- coding: utf-8 -*-
"""
Created on Wed Oct 27 15:55:02 2021

@author: carst
"""


import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from datetime import timedelta
from matplotlib.patches import Ellipse
import source_signatures
#import datetime
#from datetime import time as ti
#import os
#import time
from scipy.optimize import curve_fit
from scipy import odr
from scipy.signal import find_peaks
"""
abspath = os.path.abspath('') ## String which contains absolute path to the script file
os.chdir(os.path.dirname(os.path.abspath('Keeling Plot.py')))

start_time = time.time()
"""
plot = False

def func(p,x):
    m, b = p
    return m*x+b
def lin(x,m,b):
    return m*x+b
lin_model = odr.Model(func)
#define Keeling function for IRMS peak areas
#returns popt and pcov of the fit for every peak area. Also it returns the fitted Data for plotting as a list
#Due to the error calculation of Carina some points don't have errors. To include these points anyway we assume a error of the mean of all standard deviation
def Keeling(data, peak,ips_l,ips_r, species):
    if species == 'C':
        spec = 'd13C VPDB'
        err_spec = 'sd d13C'
        conc = '[CH4] 13C'
        err_conc='sd [CH4]'
        time = 'fill time utc'
    elif species == 'D':
        spec='d2H VPDB'
        err_spec = 'sd d2H'
        conc = '[CH4] 2H'
        err_conc='sd [CH4].1'
        time ='fill time utc.1'
    #read in the data in a dummy dataframe    
    dum = data[[conc, err_conc ,spec,err_spec]]
    dum = dum[dum[conc].notna()]
    t = data['fill time utc.2']
    
    #mask for the Background. Definition after Menoud et al. 2021. 10th percentil of a timeinterval +- 24 hours around the peak 
    BG = (data[time]>= t[peak]+timedelta(days=-1))&(data[time]<= t[peak]+timedelta(days=1))
    mask= (data[time]>=t[ips_l])&(data[time]<=t[ips_r])
    #some rows are missing just the error values for concentration and isotopic composition
    #fill in these lines with an artificial error = mean of all standard deviations
    #(This must be done, because the odr. fitting function returns nan, if error are missing)
    #The odr fit is used because it includes error of the x and y-axis in the fit.
    mean_err_spec = np.mean(dum[err_spec])
    mean_err_conc = np.mean(dum[err_conc])
    
    #Creating x (conc**-1) and y (isotope in permille) arrays
    
    #The evaluated points for the Keeling plot consists out of:
        #1. The peak area from left ips to right ips: for definition see https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.peak_widths.html#scipy.signal.peak_widths
        #2. background values. Definition after Menoud et al. 2021. 10th percentil of a timeinterval +- 24 hours around the peak
    x = np.asarray(dum[conc][mask])
    p = np.percentile(dum[conc][BG],0.1)
    x_BG = dum[conc][BG & (dum[conc][BG]<= p)]
    np.append(x,x_BG)
    x= x**(-1)
    #print(x)
    
    dx =np.asarray(dum[err_conc][mask])
    #dp = np.percentile(dum[err_conc][BG],0.1)
    dx_BG = dum[conc][BG & (dum[conc][BG]<= p)]
    np.append(dx,dx_BG)
    
    
    dx = np.nan_to_num(dx, nan=mean_err_conc) 
    dx = np.where(dx <= 1*10**(-6),mean_err_conc, dx)
    dx = dx/(x**(-2)) 
    #print(dx)
    
    y = np.asarray(dum[spec][mask])
    
    #q = np.percentile(dum[spec][BG],0.1)
    y_BG = dum[spec][BG & (dum[conc][BG]<= p)]
    np.append(y, y_BG)
    #print(y)
    
    dy = np.asarray(dum[err_spec][mask])
    #dq = np.percentile(dum[err_spec][BG],0.1)
    dy_BG = dum[err_spec][BG & (dum[conc][BG]<= p)]
    np.append(dy,dy_BG)
    dy = np.nan_to_num(dy, nan=mean_err_spec)
    dy = np.where(dy <= 1*10**(-6),mean_err_spec, dy)
    #print(dy)
    
    #popt,pcov = curve_fit(lin, x, y, sigma = dy)
    popt,pcov = curve_fit(lin, x, y)
    perr = np.sqrt(np.diag(pcov))
    
    """
    data = odr.RealData(x,y, sx=dx, sy=dy)
    odr_out = odr.ODR(data, lin_model, beta0=[0., 1.])
    out = odr_out.run()
    
    popt = out.beta
    perr = out.sd_beta
    """
    #Return
    #popt: Fit parameters (slope [m], y_intersection [b]/see func).
    #perr: Covariance Matrix
    #list: fitted data
    return popt, perr, [x,dx,y,dy]
    #return [x,dx,y,dy]
    

filepath='D:\carsten\Documents\HH Code\Data\Isotope\\'

filename = 'Hamburg AIR 01082021'


df = pd.read_excel(filepath+filename+'.xlsx', parse_dates=['fill time utc','fill time utc.1','fill time utc.2'] )
dummy = df.dropna(subset=['[CH4]'])
time = dummy['fill time utc.2']
time = np.asarray(time.dt.to_pydatetime())


base1 = dummy['[CH4]'].rolling(50, center=True) # 24 hour rolling window
base2 = dummy['[CH4]'].rolling(110, center=True)
tenth = base1.quantile(0.1)
fifth = base1.quantile(0.05)

tenth2 = base2.quantile(0.1)
fifth2 = base2.quantile(0.05)

mean_10th= np.mean(tenth2)
#________________________________________________________________________________________________

CH4 = np.asarray(dummy['[CH4]'])
peaks, properties = find_peaks(CH4, prominence=(100) ,rel_height=1, width=3, height=mean_10th)


left = np.round(properties['left_ips'])
right = np.round(properties['right_ips'])

ls =[]
for i in range(len(peaks)):
    if i == 57:
        continue
    popt_C,pcov_C, dat_C = Keeling(df, peaks[i], left[i],right[i], 'C')
    popt_D,pcov_D, dat_D = Keeling(df, peaks[i], left[i],right[i], 'D')
    
    #dat_C = Keeling(df, peaks[i], left[i],right[i], 'C')
    #dat_D = Keeling(df, peaks[i], left[i],right[i], 'D')

    x_C,dx_C,y_C,dy_C = dat_C
    x_D,dx_D,y_D,dy_D = dat_D
    
    nstd = 3. # to draw 5-sigma intervals
    popt_up_C = popt_C + nstd * pcov_C
    popt_dw_C = popt_C - nstd * pcov_C

    popt_up_D = popt_D + nstd * pcov_D
    popt_dw_D = popt_D - nstd * pcov_D

    #Carbon
    x_fit_C = np.linspace(0, max(x_C), 100)
    fit_C = func(popt_C, x_fit_C)
    fit_up_C = func(popt_up_C, x_fit_C)
    fit_dw_C = func(popt_dw_C, x_fit_C)

    #Deuterium
    x_fit_D = np.linspace(0, max(x_D), 100)
    fit_D = func(popt_D, x_fit_D)
    fit_up_D = func(popt_up_D, x_fit_D)
    fit_dw_D= func(popt_dw_D, x_fit_D)
    #print([i,popt_C[1],pcov_C[1], popt_D[1], pcov_D[1]])
    ls.append([popt_C[1],pcov_C[1], popt_D[1], pcov_D[1]])
    
#plot
    


    if False:
        fig, ax = plt.subplots(2, figsize=(12,12))
        ax[0].errorbar(x_C, y_C, yerr=dy_C, xerr=dx_C,ecolor='k', fmt='none', label='location 1')
        ax[0].scatter(x_C, y_C, 10, color='k')
        ax[0].set_xlabel(r'concentration[$ppm^{-1}$]', fontsize=18)
        ax[0].set_ylabel(r'$\delta$ C_13[‰]', fontsize=18)
        ax[0].set_title('Keeling Plot location 1 Carbon13', fontsize=18)
        ax[0].plot(x_fit_C, fit_C, 'r', lw=2, label='best fit curve')
        ax[0].fill_between(x_fit_C, fit_up_C, fit_dw_C, alpha=.25, label='3-sigma interval')
        ax[0].legend(loc='best')
        ax[0].set_xlim(0,max(x_C))

    
        ax[1].errorbar(x_D, y_D, yerr=dy_D, xerr=dx_D, fmt='none', label='location 1',marker='+')
        ax[1].scatter(x_D, y_D, 10, color='k')
        ax[1].set_xlabel(r'concentration[$ppm^{-1}$]', fontsize=18)
        ax[1].set_ylabel(r'$\delta D[‰]$', fontsize=18)
        ax[1].set_title('Keeling Plot location 1 Deuterium', fontsize=18)
        ax[1].plot(x_fit_D, fit_D, 'r', lw=2, label='best fit curve')
        #ax[0].plot(x0, y0, 'k–', lw=2, label='True curve')
        ax[1].fill_between(x_fit_D, fit_up_D, fit_dw_D, alpha=.25, label='3-sigma interval')
        ax[1].legend(loc='best')
        ax[1].set_xlim(0,max(x_D))

        plt.subplots_adjust(hspace = 0.3)
        plt.savefig(filepath+'\Signatures\Peak '+str(i)+'.png')
        plt.show()
        plt.close()

source = pd.DataFrame(ls, columns= ['src_d13C','std_d13C', 'src_d2H', 'std_d2H'])

gaus_= source_signatures.gaus_df
ells_micro = Ellipse((gaus_['mean_d13C'][0], gaus_['mean_d2H'][0]), 2*gaus_['std_d13C'][0], 2*gaus_['std_d2H'][0], 0)
ells_fossil = Ellipse((gaus_['mean_d13C'][1], gaus_['mean_d2H'][1]),2*gaus_['std_d13C'][1], 2*gaus_['std_d2H'][1], 0)
ells_pyro = Ellipse((gaus_['mean_d13C'][2], gaus_['mean_d2H'][2]), 2*gaus_['std_d13C'][2], 2*gaus_['std_d2H'][2], 0)


a = plt.subplot(111)
ells_micro.set_clip_box(a.bbox)
ells_micro.set_alpha(0.5)
ells_micro.set_label('microbial')
ells_micro.set_facecolor('b')
a.add_patch(ells_micro)

ells_fossil.set_clip_box(a.bbox)
ells_fossil.set_alpha(0.5)
ells_fossil.set_label('fossil fuel')
ells_fossil.set_facecolor('g')
a.add_patch(ells_fossil)

ells_pyro.set_clip_box(a.bbox)
ells_pyro.set_alpha(0.5)
ells_pyro.set_label('pyrogenic')
ells_pyro.set_facecolor('r')
a.add_patch(ells_pyro)

#ax.errorbar(source['src_d13C'],source['src_d2H'], xerr=source['std_d13C'], yerr=source['std_d2H'], fmt='None')
a.scatter(source['src_d13C'],source['src_d2H'])
a.set_xlabel(r'$\delta ^{13}C$')
a.set_ylabel(r'$\delta ^2H$')
a.set_xlim(-100,0)
a.set_ylim(-350,0)
a.legend()
plt.show()

if plot:
    
    fig, ax = plt.subplots(3,figsize=(16,12), sharex=True)

    #ax[0].plot(time,tenth)
    ax[0].plot(time,CH4, label=r'concentration')
    ax[0].plot(time[peaks], CH4[peaks], 'xk')
    ax[0].set_ylabel(r'$CH_4 concentration$')
    ax[0].legend()

    ax[1].plot(df['fill time utc'], df['d13C VPDB'], 'r', label=r'$\delta {}^{13}$C ')
    ax[1].set_ylabel(r'$\delta {}^{13}$C VSMOW')
    ax[1].legend()

    ax[2].plot(df['fill time utc.1'], df['d2H VPDB'], 'g', label =r'$\delta$D')
    ax[2].set_ylabel(r'$\delta$D VSMOW')
    ax[2].set_xlabel(r'time')
    ax[2].legend()
    plt.gcf().autofmt_xdate()
    plt.show()

    fig2,ax2= plt.subplots(3,figsize=(18,14), sharex=True)
    ax2[0].plot(time, tenth, label='10th,window=24h')
    ax2[0].plot(time,fifth, label='5th, window=24h')
    ax2[0].plot(time, tenth2, label='10th,window=48h')
    ax2[0].plot(time,fifth2, label='5th, window=48h')
    ax2[0].legend()

    ax2[1].plot(time, tenth-fifth, label='diff: 10th-5th, 24h')
    ax2[1].plot(time, tenth2-fifth2, label='diff: 10th-5th, 48h')
    ax2[1].legend()
    ax2[2].plot(time, tenth-tenth2, label='diff: 10th 24h - 48h')
    ax2[2].plot(time, fifth-fifth2, label='diff: 5th 24h - 48h')
    ax2[2].legend()


#fig, ax = plt.subplots(2,figsize=(12, 12))
#plt.subplot(1,2,1)