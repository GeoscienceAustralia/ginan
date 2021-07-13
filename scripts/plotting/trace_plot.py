'''
Plot a variable (XYZ coord, ZTD, residuals) vs. time with TRACE file as input

Input:
trace_file      -   .TRACE file location from pea           - str
output_dir      -   output_directory for plots              - str
    Plotting Options
-PPP            -   Plot XYZ Coords in PPP mode             - str
-SPP            -   Plot XYZ Coords in SPP mode             - str
-ZTD            -   Plot ZTD Estimates from PPP             - str
-RES            -   Plot residuals of all satellites        - str
-resid_sats     -   Plot residuals of list of satellites    - str
                    in comma separated format, 
                    e.g. G02,G05,G32,G25,G15
-resid_LC       -   Choose linear combination plotted       - str
                    default: L1L2
                    options: L1L2, L1L5
-resid_codpha   -   Choose code or phase plotted            - str
                    default: Code
                    options: Code, Phase, or Code,Phase
-resid_fit      -   Choose pre or post fit plotted          - str
                    default: Postfit
                    options: Prefit, Postfit, or Prefit,Postfit


Output:
Time-series plot/s of the variable/s saved in output_dir


Ronald Maj (based on code developed by Michael Moore at Geoscience Australia)
2020-12-03 15:34
'''

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
import pandas as pd
from pathlib import Path
from numpy import loadtxt

from p_tqdm import p_map

import argparse
from datetime import datetime, timedelta
import re

from gn_lib.gn_io.trace import _read_trace, _read_trace_res, _read_trace_LC, _read_trace_el
from gn_lib.gn_io.sinex import _read_snx_solution
from gn_lib.gn_io.trop import _read_tro_solution
from gn_lib.gn_const import J2000_ORIGIN
from gn_lib.gn_datetime import yydoysec2datetime as _yydoysec2datetime
from gn_lib.gn_datetime import j20002datetime as _j20002datetime

# Old trace file parsing function - still needed for SPP
#==============================================================================
def parseTRACEfile(tracefile): #, ds, satels):
    
    # Final dict
    output = {}
    
    # Placeholder for the datetime of Epoch 1
    epoch1_dt = 0        

    # Open the TRACE file:
    with open(tracefile) as f:
        
        # Run through line by line
        for line in f:
            
            # Removing any trailing space
            line = line.rstrip()            
            
            if 'station    : ' in line:
                output['station'] = line.split(':')[1]


            # Now search through for any time a new Epoch block starts and save State and Residual info
            if '################# Starting Epoch' in line:

                parts = line.split(' ')

                # Get the Epoch number:
                for part in parts:
                    try:
                        epoch = int(part)
                    except ValueError:
                        continue
                
                line = f.readline()
                line = line.rstrip()
                
                # Before reaching the end of the Epoch block, save all the info:
                while '- States' not in line:
                    
                    line = f.readline()

                    # First, find the start datetime of the processing, i.e. Epoch 1 
                    if (epoch1_dt == 0) & ('*1 PDE-CS GPST' in line):
                        parts = line.split(' ')
                        nums = []
                        for part in parts:
                            try:
                                nums.append(float(part))
                            except ValueError:
                                continue

                        # GPS-Time start point:
                        gps_st = datetime.strptime('1980 006','%Y %j')
                        # Add number of weeks and seconds to start point to get Epoch 1:
                        epoch1_dt = gps_st + timedelta(weeks = nums[0], seconds = nums[1])
                        # Save to output
                        output['Epoch1'] = epoch1_dt

                    # If spp info, record:
                    if '*3 sppos  sol:' in line:
                        
                        if 'sppPos' not in output:
                            output['sppPos'] = {}
                            output['sppPos']['X'] = []
                            output['sppPos']['Y'] = []
                            output['sppPos']['Z'] = []
                        
                        parts = line.split(' ')
                        nums = []
                        for part in parts:
                            try:
                                nums.append(float(part))
                            except ValueError:
                                continue
                        
                        output['sppPos']['X'].append(nums[0])
                        output['sppPos']['Y'].append(nums[1])
                        output['sppPos']['Z'].append(nums[2])

            else:
                pass
    
    return output 

def calc_diff_lag(y, x, lag):
    ''' Calculate y[n+1] - y[n] up to (n-1), and output new_y
    '''
    new_y = y[lag:] - y[:-lag]
    if len(x) == len(new_y):
        return new_y,x
    else:
        x = x[:-lag]
        return new_y,x


def resid_plot(
    data, 
    station, 
    LC, 
    cod_or_ph, 
    fit, 
    sats, 
    st_epoch = 0,
    en_epoch = None, 
    xlim1 = None, 
    xlim2 = None, 
    ymin = None,
    ymax = None,
    flex_st = None, 
    flex_en = None,
    save_fig = False, 
    show = False,
    diff_lag = 0):
    """
    Plot of the Residuals for given inputs of Linear-Combination type, 
    code/phase, pre/post fit, and sats

    Input
    data      - Results to be plotted (dict from parseTRACEfile) - dict
    station     - The station of interest - str
    LC          - Options being 'L1L2' and 'L1L5' - str
    cod_or_ph   - Code or Phase data (to plot) - str 
    fit         - Type of residual to plot: either 'Postfit' or 'Prefit' - str
    sat         - List of satellites to plot - list of str
    st_epoch    - If plotting is to start at epoch other than Epoch 1 - int
    
    OPTIONAL
    If a portion of the plot is to be highlight to show a flex event, need 
    start and end epochs
    flex_st     - Epoch number where flex event starts - int
    flex_en     - Epoch number where flex event ends - int
    If the figure is to be saved, set save_fig = True, this will save to pwd

    Output
    Zenith Tropospheric Delay Plot
    """
    fig1,ax1 = plt.subplots(1,1, figsize = (12,6) )

    # Get Epoch info:
    epochs = data.index.get_level_values(1).values
    epoch1 = J2000_ORIGIN + np.timedelta64(epochs[st_epoch],'s')

    if en_epoch == None:
        epochN = J2000_ORIGIN + np.timedelta64(epochs[-1],'s')
    else:
        epochN = J2000_ORIGIN + np.timedelta64(epochs[en_epoch],'s')
    
    date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")+'-'+epochN.astype(datetime).strftime("%j")

    #epoch1 = data['Epoch1'] 
    alph = 0.95

    for sat in sats:


        sat_mask = (data['PRN'] == sat)

        if cod_or_ph == 'Code':
            cp = 'P'
        elif cod_or_ph == 'Phase':
            cp = 'L'
        if LC == 'L1L2':
            cp += '12'
        elif LC == 'L1L5':
            cp += '15'

        cp_mask = (data.index.get_level_values(0) == cp)
        
        data2plot = data[sat_mask & cp_mask]
        y = np.array(data2plot[fit.upper()])
        data2plot = data2plot.reset_index(level=[0,1])
        x = np.array(data2plot['level_1'])
        x_times = J2000_ORIGIN + np.array(x,dtype='timedelta64[s]')
        if diff_lag:
            y,x_times = calc_diff_lag(y,x_times,diff_lag)
        ax1.plot(x_times[st_epoch:en_epoch], y[st_epoch:en_epoch], label=sat, alpha=alph)
        alph *= 0.88
    
    # If indices given for start and end to flex event given, highlight that part
    if flex_st:
        ax1.axvspan(flex_st, flex_en, alpha=0.1, color='red')   
    
    ax1.set_ylim(ymin=ymin,ymax=ymax)

    ax1.set_ylabel('Residual (m)')
    # Formating x-axis
    myFmt = mdates.DateFormatter('%b-%d %H:%M')
    ax1.xaxis.set_major_formatter(myFmt)
    fig1.autofmt_xdate() 
    
    #ax1.set_xlabel(f'Epoch Number (Epoch 1: {epoch1.strftime("%Y-DOY-%j %H:%M:%S")})')
    ax1.grid(True)
    ax1.legend()
    if xlim1:
        ax1.set_xlim(xmin=xlim1, xmax=xlim2)
        ax1.title.set_text(f'{station} - {LC} - {cod_or_ph} {fit} Residual Plot - {date_str} Epoch {xlim1}-{xlim2}')
        if save_fig:
            f_save = f'{save_fig}{date_str}_{station}-{LC}-_{cod_or_ph}_{fit}_Residuals__Epoch-{xlim1}-{xlim2}.png'
            fig1.savefig(f_save)
            print(f'Saved: {f_save}')
    elif st_epoch !=0:
        ax1.title.set_text(f'{station} - {LC} - {cod_or_ph} {fit} Residual Plot - {date_str} Epoch {st_epoch}-end')
        if save_fig:
            f_save = f'{save_fig}{date_str}_{station}-{LC}-_{cod_or_ph}_{fit}_Residuals_St_Epoch-{st_epoch}.png'
            fig1.savefig(f_save)
            print(f'Saved: {f_save}')
    else:
        ax1.title.set_text(f'{station} - {LC} - {cod_or_ph} {fit} Residual Plot - {date_str}')
        if save_fig:
            f_save = f'{save_fig}{date_str}_{station}-{LC}-_{cod_or_ph}_{fit}_Residuals.png'
            fig1.savefig(f_save)
            print(f'Saved: {f_save}')
    if show:
        fig1.show()
    return fig1,ax1



def pos_plot(
    data, 
    station, 
    st_epoch = 0, 
    en_epoch = None, 
    flex_st = None, 
    flex_en = None, 
    save_fig = False, 
    show = False,
    diff_lag = 0):
    """
    Coordinate (X,Y,Z) plot using the data from the parseTRACEfile function and named station.
    This is a 3 panel plot, each running horizontally and in X, Y, Z order from top to bottom

    Input
    data      - Results to be plotted (dict from parseTRACEfile) - dict
    station     - The station of interest - str
    st_epoch    - If plotting is to start at some epoch other than Epoch 1 (optional) - int
    
    OPTIONAL
    If a portion of the plot is to be highlight to show a flex event, need start and end epochs
    flex_st     - Epoch number where flex event starts - int
    flex_en     - Epoch number where flex event ends - int
    If the figure is to be saved, set save_fig = True, this will save to pwd

    Output
    PPP POS Plot
    """

    # Get Epoch info:
    epochs = data.index.get_level_values(1).values
    epoch1 = J2000_ORIGIN + np.timedelta64(epochs[st_epoch],'s')
    date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")
    
    # Set up figure
    fig2,ax2 = plt.subplots(3,1, figsize = (18,14) )

    # Get y, x and error ues for the X coord plot
    yX = data.EST.X.values[st_epoch:en_epoch]
    yerrX = np.sqrt(data.VAR.X.values[st_epoch:en_epoch])
    x = epochs[st_epoch:en_epoch]
    x_times = J2000_ORIGIN + np.array(x,dtype='timedelta64[s]')

    # Plot the x and y values for the top most subplot, including error range
    if diff_lag:
        yX,x_times = calc_diff_lag(yX,x_times,diff_lag)
    
    ax2[0].plot(x_times,yX)
    ax2[0].fill_between(x_times, yX-yerrX, yX+yerrX,alpha=0.2)

    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax2[0].axvspan(flex_st, flex_en, alpha=0.2, color='red')

    # Label the subplot
    ax2[0].set_ylabel('X Coordinate (m)')
    ax2[0].tick_params(labelbottom=False)    
    ax2[0].title.set_text('X')
    ax2[0].grid(True)


    # Get y and error values for the Y coord plot
    yY = data.EST.Y.values[st_epoch:en_epoch]
    yerrY = np.sqrt(data.VAR.Y.values[st_epoch:en_epoch])

    # Plot the x and y values for the middle subplot, including error range
    if diff_lag:
        yY,x_times = calc_diff_lag(yY,x_times,diff_lag)
    
    ax2[1].plot(x_times,yY)
    ax2[1].fill_between(x_times, yY-yerrY, yY+yerrY,alpha=0.2)
    
    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax2[1].axvspan(flex_st, flex_en, alpha=0.2, color='red')    
    
    # Label the subplot
    ax2[1].set_ylabel('Y Coordinate (m)')
    ax2[1].tick_params(labelbottom=False)  
    ax2[1].title.set_text('Y')
    ax2[1].grid(True)


    # Get y and error values for the Z coord plot
    yZ = data.EST.Z.values[st_epoch:en_epoch]
    yerrZ = np.sqrt(data.VAR.Z.values[st_epoch:en_epoch])

    # Plot the x and y values for the bottom subplot, including error range
    if diff_lag:
        yZ,x_times = calc_diff_lag(yZ,x_times,diff_lag)    
    
    ax2[2].plot(x_times,yZ)
    ax2[2].fill_between(x_times, yZ-yerrZ, yZ+yerrZ,alpha=0.2)
    
    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax2[2].axvspan(flex_st, flex_en, alpha=0.2, color='red')   
    
    # Label the subplot
    ax2[2].set_ylabel('Z Coordinate (m)')
    #ax2[2].set_xlabel(f'Epoch Number (Epoch 1: {epoch1.strftime("%Y-DOY-%j %H:%M:%S")})')
    ax2[2].title.set_text('Z')
    ax2[2].grid(True)

    # Formating x-axis
    myFmt = mdates.DateFormatter('%b-%d %H:%M')
    ax2[2].xaxis.set_major_formatter(myFmt)
    fig2.autofmt_xdate() 


    # Given title to the entire figure and show
    fig2.suptitle(f'Coordinate Estimates - {station} - {date_str}')
    
    if save_fig:
        plt.tight_layout()
        f_save = f'{save_fig}{date_str}_-{station}-PPP_XYZ_Coordinates.png'
        fig2.savefig(f_save)  
        print(f'Saved: {f_save}')  
    
    if show:
        fig2.show()
    return fig2,ax2



def trop_plot(
    data, 
    station, 
    st_epoch = 0, 
    en_epoch = None,
    flex_st = None, 
    flex_en = None, 
    save_fig = False, 
    show = False,
    diff_lag = 0):
    """
    Plot of the Zenith Tropospheric Delay

    Input
    data      - Results to be plotted (df from _read_trace) - pandas DataFrame
    st_epoch    - If plotting is to start at some epoch other than first Epoch in the df (optional) - int
    
    OPTIONAL
    If a portion of the plot is to be highlight to show a flex event, need start and end epochs
    flex_st     - Epoch number where flex event starts - int
    flex_en     - Epoch number where flex event ends - int
    If the figure is to be saved, set save_fig = True, this will save to pwd

    Output
    Zenith Tropospheric Delay Plot
    """
    # Get Epoch info:
    epochs = data.index.get_level_values(1).values
    epoch1 = J2000_ORIGIN + np.timedelta64(epochs[st_epoch],'s')
    date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")
    
    # Set up figure
    fig3,ax3 = plt.subplots(1,1, figsize = (12,6) )

    # Get y, x and error values for the ZTD plot
    y = data.EST.trop.values[st_epoch:en_epoch]
    yerr = np.sqrt(data.VAR.trop.values[st_epoch:en_epoch])
    x = epochs[st_epoch:en_epoch]
    x_times = J2000_ORIGIN + np.array(x,dtype='timedelta64[s]')

    # Plot x,y values and the error range
    if diff_lag:
        y,x_times = calc_diff_lag(y,x_times,diff_lag)     
    
    ax3.plot(x_times,y)
    ax3.fill_between(x_times, y-yerr, y+yerr,alpha=0.2)

    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax3.axvspan(flex_st, flex_en, alpha=0.2, color='red')   

    # Labels
    ax3.set_ylabel('ZTD (m)')
    #ax3.set_xlabel(f'Epoch 1: {epoch1.astype(datetime).strftime("%Y-DOY-%j %H:%M:%S")}')
    ax3.title.set_text(f'Zenith Tropospheric Delay - {station} - {date_str}')
    ax3.grid(True)

    # Formating x-axis
    myFmt = mdates.DateFormatter('%b-%d %H:%M')
    ax3.xaxis.set_major_formatter(myFmt)
    fig3.autofmt_xdate() 

    if save_fig:
        plt.tight_layout()
        f_save = f'{save_fig}{date_str}_-{station}-PPP_ZTD_Estimates.png'
        fig3.savefig(f_save) 
        print(f'Saved: {f_save}')  
    if show:
        fig3.show()    
    return fig3,ax3



def spppos_plot(
    data, 
    station, 
    st_epoch = 0, 
    en_epoch = None,
    flex_st = None, 
    flex_en = None, 
    save_fig = False, 
    show = False,
    diff_lag = 0):
    """
    Coordinate (X,Y,Z) plot using the data from the parseTRACEfile function and named station.
    This is a 3 panel plot, each running horizontally and in X, Y, Z order from top to bottom

    Input
    data      - Results to be plotted (dict from parseTRACEfile) - dict
    station     - The station of interest - str
    st_epoch    - If plotting is to start at some epoch other than Epoch 1 (optional) - int
    
    OPTIONAL
    If a portion of the plot is to be highlight to show a flex event, need start and end epochs
    flex_st     - Epoch number where flex event starts - int
    flex_en     - Epoch number where flex event ends - int
    If the figure is to be saved, set save_fig = True, this will save to pwd

    Output
    SPP POS Plot
    """

    epoch1 = data['Epoch1']
    
    # Set up figure
    fig4,ax4 = plt.subplots(3,1, figsize = (18,18) )


    # Get y, x and error values for the X coord plot
    y = np.array(data['sppPos']['X'])[st_epoch:en_epoch]
    if en_epoch == None:
        x = list(range(st_epoch, len(y) + st_epoch))
    else:
        x = list(range(st_epoch, len(y) + st_epoch - en_epoch))

    # Plot the x and y values for the top most subplot, including error range
    if diff_lag:
        y,x = calc_diff_lag(y,x,diff_lag) 
    ax4[0].plot(x,y)
    
    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax4[0].axvspan(flex_st, flex_en, alpha=0.2, color='red')
    
    # Label the subplot
    ax4[0].set_ylabel('X Coordinate (m)')
    ax4[0].title.set_text('X')
    ax4[0].grid(True)


    # Get y and error values for the Y coord plot
    y = np.array(data['sppPos']['Y'])[st_epoch:en_epoch]

    # Plot the x and y values for the middle subplot, including error range
    if diff_lag:
        y,x = calc_diff_lag(y,x,diff_lag) 
    ax4[1].plot(x,y)
    
    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax4[1].axvspan(flex_st, flex_en, alpha=0.2, color='red')    
    
    # Label the subplot
    ax4[1].set_ylabel('Y Coordinate (m)')
    ax4[1].title.set_text('Y')
    ax4[1].grid(True)


    # Get y and error values for the Z coord plot
    y = np.array(data['sppPos']['Z'])[st_epoch:en_epoch]

    # Plot the x and y values for the bottom subplot, including error range
    if diff_lag:
        y,x = calc_diff_lag(y,x,diff_lag) 
    ax4[2].plot(x,y)
    
    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax4[2].axvspan(flex_st, flex_en, alpha=0.2, color='red')   
    
    # Label the subplot
    ax4[2].set_ylabel('Z Coordinate (m)')
    ax4[2].set_xlabel(f'Epoch Number (Epoch 1: {epoch1.strftime("%Y-DOY-%j %H:%M:%S")})')
    ax4[2].title.set_text('Z')
    ax4[2].grid(True)

    # Given title to the entire figure and show
    fig4.suptitle(f'SPP Coordinate Estimates - {station} - {epoch1.strftime("%Y-DOY-%j")} - {(timedelta(seconds = 30*(x[-1]+1))).days} Day Plot')
    if save_fig:
        f_save = f'{save_fig}{epoch1.strftime("%Y-DOY-%j")}_-{station}-SPP_XYZ_Coordinates.png'
        fig4.savefig(f_save) 
        print(f'Saved: {f_save}')
    if show:
        fig4.show()
    return fig4,ax4



def pos_diff_plot(
    data, 
    station, 
    snx_path, 
    st_epoch = 0, 
    en_epoch = None,
    ymin = None,
    ymax = None,
    flex_st = None, 
    flex_en = None, 
    save_fig = False, 
    show = False,
    monument = 'A'):
    """
    Coordinate (X,Y,Z) plot using the data from the parseTRACEfile function and named station.
    This is a 3 panel plot, each running horizontally and in X, Y, Z order from top to bottom

    Input
    data        - Results to be plotted (dict from parseTRACEfile) - dict
    station     - The station of interest - str
    snx_path    - snx file path to obtain reference coordinates to compare against - Path obj
    
    OPTIONAL
    st_epoch    - If plotting is to start at some epoch other than Epoch 1 (optional) - int
    If a portion of the plot is to be highlight to show a flex event, need start and end epochs
    flex_st     - Epoch number where flex event starts - int
    flex_en     - Epoch number where flex event ends - int
    If the figure is to be saved, set save_fig = True, this will save to pwd

    Output
    PPP POS Difference Plot
    """

    # Get Epoch info:
    unzipped_indexes = zip(*data.index)
    index_arr = np.array(list(unzipped_indexes))
    epochs = index_arr[1]
    epoch1 = J2000_ORIGIN + np.timedelta64(epochs[0],'s')
    date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")
    
    # Get snx reference coordinates-
    df_snx = _read_snx_solution(snx_path)
    station += f'_{monument}'

    snx_epoch = J2000_ORIGIN + np.array(df_snx.loc[station].index[0],dtype='timedelta64[s]')

    x_snx = df_snx.loc[station].EST.STAX.values[0]
    y_snx = df_snx.loc[station].EST.STAY.values[0]
    z_snx = df_snx.loc[station].EST.STAZ.values[0]
    xSD_snx = df_snx.loc[station].STD.STAX.values[0]
    ySD_snx = df_snx.loc[station].STD.STAY.values[0]
    zSD_snx = df_snx.loc[station].STD.STAZ.values[0]

    # Set up figure
    fig5,ax5 = plt.subplots(1,1, figsize = (12,6) )

    # Get y, x and error values for the X coord plot
    yX = data.EST.X.values[st_epoch:en_epoch] - x_snx
    yerrX = np.sqrt(data.VAR.X.values[st_epoch:en_epoch] + xSD_snx**2)
    x = epochs[st_epoch:en_epoch]
    x_times = J2000_ORIGIN + np.array(x,dtype='timedelta64[s]')

    # Plot the x and y values for the top most subplot, including error range
    ax5.plot(x_times,yX,label='X Estimate (PEA-IGS)')
    ax5.fill_between(x_times, yX-yerrX, yX+yerrX,alpha=0.2)

    # Get y and error values for the Y coord plot
    yY = data.EST.Y.values[st_epoch:en_epoch] - y_snx
    yerrY = np.sqrt(data.VAR.Y.values[st_epoch:en_epoch] + ySD_snx**2)

    # Plot the x and y values for the middle subplot, including error range
    ax5.plot(x_times,yY,label='Y Estimate (PEA-IGS)')
    ax5.fill_between(x_times, yY-yerrY, yY+yerrY,alpha=0.2)

    # Get y and error values for the Z coord plot
    yZ = data.EST.Z.values[st_epoch:en_epoch] - z_snx
    yerrZ = np.sqrt(data.VAR.Z.values[st_epoch:en_epoch] + zSD_snx**2)

    # Plot the x and y values for the bottom subplot, including error range
    ax5.plot(x_times,yZ,label='Z Estimate (PEA-IGS)')
    ax5.fill_between(x_times, yZ-yerrZ, yZ+yerrZ,alpha=0.2)
    
    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax5.axvspan(flex_st, flex_en, alpha=0.2, color='red')

    ax5.set_ylim(ymin=ymin,ymax=ymax)

    ax5.grid(True)
    ax5.legend()

    # Formating x-axis
    myFmt = mdates.DateFormatter('%b-%d %H:%M')
    ax5.xaxis.set_major_formatter(myFmt)
    fig5.autofmt_xdate() 


    # Given title to the entire figure and show
    fig5.suptitle(f' Difference of Coordinate Estimates (PEA - IGS) - {station} - {date_str}')
    
    if save_fig:
        f_save = f'{save_fig}{date_str}_-{station}-PPP_SNX_Diff_XYZ_Coordinates.png'
        fig5.savefig(f_save)    
        print(f'Saved: {f_save}')
    if show:
        
        fig5.show()
    return fig5,ax5



def trop_diff_plot(
    data, 
    station, 
    tro_path, 
    st_epoch = 0, 
    en_epoch = None,
    flex_st = None, 
    flex_en = None, 
    save_fig = False, 
    show = False):
    """
    Difference plot of the Zenith Tropospheric Delay (against IGS or other AC .tro file)

    Input
    data        - Results to be plotted (df from _read_trace) - pandas DataFrame
    station     - Station of interest
    tro_path    - Path to the .tro file

    
    OPTIONAL
    st_epoch    - If plotting is to start at some epoch other than first Epoch in the df (optional) - int
    If a portion of the plot is to be highlight to show a flex event, need start and end epochs
    flex_st     - Epoch number where flex event starts - int
    flex_en     - Epoch number where flex event ends - int
    If the figure is to be saved, set save_fig = True, this will save to pwd

    Output
    Difference plot of Zenith Tropospheric Delay (PEA vs. AC)
    """
    
    # Set up figure
    fig6,ax6 = plt.subplots(2,1, figsize = (12,12) )

    # Get .tro reference
    df_tro = _read_tro_solution(tro_path, recenter=False)
    AC = Path(tro_path).stem[:3].upper()

    # Create df of overlapping data
    data_list = [data.EST.trop.iloc[st_epoch:en_epoch],df_tro.TROTOT.VAL/1000]
    err_list = [data.VAR.trop.iloc[st_epoch:en_epoch],df_tro.TROTOT.STD/1000]

    compare_data = pd.concat(data_list,axis=1,join='inner')
    compare_err = pd.concat(err_list,axis=1,join='inner')
    compare_data['difference'] = compare_data['trop']-compare_data['VAL']

    # Get y, x and error values for the ZTD plots
    y = compare_data.trop.values
    yerr = np.sqrt(compare_err.trop.values)
    
    y_tro = compare_data.EST.values
    err_tro = compare_err.STD.values
    
    y_diff = compare_data.difference.values
    err_diff = np.sqrt(yerr**2 + err_tro**2)
    
    x_tro = compare_data.index.get_level_values(1).values
    x_times = J2000_ORIGIN + np.array(x_tro,dtype='timedelta64[s]')
    
    epoch1 = x_times[0]
    date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")

    # Plot trop values and the error range for each
    
    ax6[0].plot(x_times,y,'.-',label='PEA Estimate')
    ax6[0].fill_between(x_times, y-yerr, y+yerr,alpha=0.2)
    
    ax6[0].plot(x_times,y_tro,'.-',label=f'{AC} .TRO Estimate')
    ax6[0].fill_between(x_times, y_tro-err_tro, y_tro+err_tro,alpha=0.2)

    # Labels
    ax6[0].set_ylabel('ZTD (m)')
    ax6[0].title.set_text(f'Zenith Tropospheric Delay - PEA vs {AC} Difference Plots - {station} - {date_str}')
    ax6[0].grid(True)
    ax6[0].legend()

    # Plot difference of trop values and the error range
    ax6[1].plot(x_times,y_diff,'.-',label=f'Difference (PEA - {AC})')
    ax6[1].fill_between(x_times, y_diff-err_diff, y_diff+err_diff,alpha=0.2)

    # Labels
    ax6[1].set_ylabel(f'PEA-{AC} ZTD Difference (m)')
    ax6[1].grid(True)
    ax6[1].legend()

    # Formating x-axis
    myFmt = mdates.DateFormatter('%b-%d %H:%M')
    ax6[1].xaxis.set_major_formatter(myFmt)
    fig6.autofmt_xdate() 

    # If indices given for start and end to flex event give, highlight that part
    if flex_st:
        ax6[0].axvspan(flex_st, flex_en, alpha=0.2, color='red')   
        ax6[1].axvspan(flex_st, flex_en, alpha=0.2, color='red')   

    if save_fig:
        plt.tight_layout()
        f_save = f'{save_fig}{date_str}_-{station}-PPP_ZTD_Difference_{AC}.png'
        fig6.savefig(f_save) 
        print(f'Saved: {f_save}')
    if show:
        fig6.show()    
    return fig6,ax6



def pos_mean_plot(
    data, 
    station, 
    mean_epoch_st = 240, 
    mean_epoch_en = None, 
    st_epoch = 500, 
    en_epoch = None, 
    flex_st = None, 
    flex_en = None, 
    save_fig = False, 
    save_prefix = '',
    show = False,
    man_fig = None,
    man_ax = np.array([])):
    """
    pos_plot of mean values
    Inputs:
    data            - pandas df created from _read_trace() function
    station         - str of station name (used for title, label)

    Optional:
    mean_epoch_st 
    mean_epoch_en   - Calculate the mean coord value based on 
                    start and end indices provided:
                    - defaults: 240, 500
    st_epoch
    en_epoch        - Start and end points to display
    flex_st
    flex_en         - Highlight part of plot between st and en 
                    datetime64 format
    save_fig 
    save_prefix     - allows plot to be saved to file (with prefix)
    show            - set to True if wanting to display in window
    man_fig 
    man_ax          - allows input of existing figure objects

    """

    # Get Epoch info:
    epochs = data.index.get_level_values(1).values
    epoch1 = J2000_ORIGIN + np.timedelta64(epochs[st_epoch],'s')
    date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")
    
    if (man_fig == None) & (man_ax.size == 0): 
        # Set up figure
        fig7,ax7 = plt.subplots(3,1, figsize = (18,14) )
    else:
        # Otherwise use supplied fig and ax objects:
        fig7,ax7 = man_fig,man_ax

    # Calculate the means for each dimension:
    if mean_epoch_en == None:
        mean_epoch_en = st_epoch
    
    # Means
    mean_period_data = data[mean_epoch_st:mean_epoch_en]
    xmean = mean_period_data.EST.X.mean()
    ymean = mean_period_data.EST.Y.mean()
    zmean = mean_period_data.EST.Z.mean()
    
    rep_x = (data.EST.X - xmean).values.reshape(len(data.EST.X),1)
    rep_y = (data.EST.Y - ymean).values.reshape(len(data.EST.X),1)
    rep_z = (data.EST.Z - zmean).values.reshape(len(data.EST.X),1)

    xyz_deviation = np.concatenate((rep_x,rep_y,rep_z), axis=1)
    new_arr = np.concatenate((xyz_deviation,data.VAR[['X','Y','Z']].values),axis=1)
    data = pd.DataFrame(new_arr, index = data.index, columns = data.columns[:6])

    # x values
    x = epochs[st_epoch:en_epoch]
    x_times = J2000_ORIGIN + np.array(x,dtype='timedelta64[s]')

    # Get y, x and error values for the X coord plot
    yX = data.EST.X.values[st_epoch:en_epoch]
    yerrX = np.sqrt(data.VAR.X.values[st_epoch:en_epoch])
    

    # Plot the x and y values for the top most subplot, including error range
    ax7[0].plot(x_times,yX,label=station)
    ax7[0].fill_between(x_times, yX-yerrX, yX+yerrX,alpha=0.2)

    # Label the subplot
    ax7[0].set_ylabel('Deviation from Mean X Coord (m)')
    ax7[0].tick_params(labelbottom=False)    
    ax7[0].title.set_text('X')
    ax7[0].grid(True)
    ax7[0].legend(bbox_to_anchor=(1.04,1), loc="upper left")


    # Get y and error values for the Y coord plot
    yY = data.EST.Y.values[st_epoch:en_epoch]
    yerrY = np.sqrt(data.VAR.Y.values[st_epoch:en_epoch])

    # Plot the x and y values for the middle subplot, including error range
    ax7[1].plot(x_times,yY)
    ax7[1].fill_between(x_times, yY-yerrY, yY+yerrY,alpha=0.2)
    
    # Label the subplot
    ax7[1].set_ylabel('Deviation from Mean Y Coord (m)')
    ax7[1].tick_params(labelbottom=False)  
    ax7[1].title.set_text('Y')
    ax7[1].grid(True)


    # Get y and error values for the Z coord plot
    yZ = data.EST.Z.values[st_epoch:en_epoch]
    yerrZ = np.sqrt(data.VAR.Z.values[st_epoch:en_epoch])

    # Plot the x and y values for the bottom subplot, including error range
    ax7[2].plot(x_times,yZ)
    ax7[2].fill_between(x_times, yZ-yerrZ, yZ+yerrZ,alpha=0.2)
    
    # Label the subplot
    ax7[2].set_ylabel('Deviation from Mean Z Coord (m)')
    #ax7[2].set_xlabel(f'Epoch Number (Epoch 1: {epoch1.strftime("%Y-DOY-%j %H:%M:%S")})')
    ax7[2].title.set_text('Z')
    ax7[2].grid(True)


    # If times given for start and end to flex event give, highlight that part
    if flex_st:   
        ax7[0].axvspan(flex_st, flex_en, alpha=0.02, color='red')
        ax7[1].axvspan(flex_st, flex_en, alpha=0.02, color='red')
        ax7[2].axvspan(flex_st, flex_en, alpha=0.02, color='red')


    # Formating x-axis
    myFmt = mdates.DateFormatter('%b-%d %H:%M')
    ax7[2].xaxis.set_major_formatter(myFmt)
    fig7.autofmt_xdate() 



    if (man_fig == None) & (man_ax == np.array([])): 
        # Given title to the entire figure and show
        fig7.suptitle(f'Coordinate Estimates - {station} - {date_str}')
    else:
        fig7.suptitle(f'Coordinate Estimates - Multiple Stations - {date_str}')
    
    if save_fig:
        #plt.tight_layout(rect=[0,0,0.75,1])
        if (man_fig == None) & (man_ax == np.array([])): 
            f_save = f'{save_fig}{save_prefix}{date_str}_-MultiStation-PPP_XYZ_Coordinates.png'
            fig7.savefig(f_save,bbox_inches="tight")
            print(f'Saved: {f_save}')
        else:
            f_save = f'{save_fig}{save_prefix}{date_str}_-{station}-PPP_XYZ_Coordinates.png'
            fig7.savefig(f_save,bbox_inches="tight")   
            print(f'Saved: {f_save}') 
    
    if show:
        #plt.tight_layout(rect=[0,0,0.75,1])
        fig7.show()
    
    return fig7,ax7




def obsLC_plot(
    data, 
    station, 
    obsLC_op = 'gf12', 
    cod_or_ph = 'Code', 
    sats = ['ALL'], 
    st_epoch = 0,
    en_epoch = None, 
    xlim1 = None, 
    xlim2 = None, 
    ymin = None,
    ymax = None,
    flex_st = None, 
    flex_en = None,
    save_fig = False, 
    show = False,
    diff_lag = 0,
    el_flag = None,
    el_plot = False):

    if 'ALL' in sats:
        sats = sorted(list(set(data['PRN'])))

    if cod_or_ph == 'Code':
        LP = 'P'
    else:
        LP = 'L'

    if (obsLC_op[0] == 'P') or (obsLC_op[0:2] == 'mp'):
        LP = 'P'
        cod_or_ph = 'Code'

    if (obsLC_op[0] == 'L') or (obsLC_op[0:2] in ['mw','wl']):
        LP = 'L'
        cod_or_ph = 'Phase'


    # Get Epoch info:
    epochs = data.index.values
    epoch1 = J2000_ORIGIN + np.timedelta64(epochs[st_epoch],'s')
    if en_epoch == None:
        epochN = J2000_ORIGIN + np.timedelta64(epochs[-1],'s')
    else:
        epochN = J2000_ORIGIN + np.timedelta64(epochs[en_epoch],'s')

    date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")+'-'+epochN.astype(datetime).strftime("%j")


    # Introduce datetime column:
    data['datetime'] = _j20002datetime(np.array(data.index))

    fig8,ax8 = plt.subplots(1,1,figsize=(20,12))

    op_mask = (data.combo == obsLC_op)
    LP_mask = (data.LP == LP)
    sat_mask = (data.PRN.isin(sats))

    

    if el_plot:
        p_data = data[op_mask&LP_mask&sat_mask].reset_index(
        ).pivot(
            index = 'datetime',
            columns='PRN',
            values=['value','el']
            )
        p_data = p_data[st_epoch:en_epoch]
        
        alph = 1.0
        count = 0
        for sat in sats:
            
            count+=1
            if count%10 == 0:
                alph -= 0.3
            
            ax8.plot(p_data.el[sat],p_data.value[sat],'.',label=sat, alpha=alph)
        ax8.set_xlabel(f'Elevation Angle (deg)',fontsize=18)        

    
    else:
        p_data = data[op_mask&LP_mask&sat_mask].reset_index(
        ).pivot(
            index = 'datetime',
            columns='PRN',
            values='value'
            )        
        x = p_data.index.values[st_epoch:en_epoch]
        alph = 1.0
        count = 0
        for sat in sats:
            
            count+=1
            if count%10 == 0:
                alph -= 0.3
            
            y = p_data[sat].values[st_epoch:en_epoch]
            
            if diff_lag:
                y,x = calc_diff_lag(y,x,diff_lag)
            
            ax8.plot(x, y,marker='.',label=sat, alpha=alph)
        ax8.set_xlabel(f'Date / Time',fontsize=18)
        
    ax8.set_ylim(ymin=ymin,ymax=ymax)
    ax8.set_ylabel(f'{cod_or_ph}_{obsLC_op} (m)',fontsize=18)

    if el_flag == 0.001:
        el_flag = None

    if el_flag:
        ax8.set_title(f'Plot of {cod_or_ph}_{obsLC_op}, excl. el. below {el_flag} Epoch1: {data.datetime.iloc[0]}', fontsize=24)
    else:
        ax8.set_title(f'Plot of {cod_or_ph}_{obsLC_op}, Epoch1: {data.datetime.iloc[0]}', fontsize=24)
    expt = ax8.yaxis.get_offset_text()
    expt.set_size(14)

    # myFmt = mdates.DateFormatter('%b-%d %H:%M')
    # ax8.xaxis.set_major_formatter(myFmt)
    # fig8.autofmt_xdate() 

    ax8.grid(True)
    ax8.legend()

    if xlim1:
        ax8.set_xlim(xmin=xlim1, xmax=xlim2)
        if el_flag:
            ax8.title.set_text(f'{station} {cod_or_ph}_{obsLC_op} Plot - {date_str} Epoch {xlim1}-{xlim2} excl. el. below {el_flag}')
        else:
            ax8.title.set_text(f'{station} {cod_or_ph}_{obsLC_op} Plot - {date_str} Epoch {xlim1}-{xlim2}')
        if save_fig:
            f_save = f'{save_fig}{date_str}_{station}{cod_or_ph}_{obsLC_op}__Epoch-{xlim1}-{xlim2}.png'
            fig8.savefig(f_save)
            print(f'Saved: {f_save}')
    elif st_epoch !=0:
        if el_flag:
            ax8.title.set_text(f'{station} - {cod_or_ph}_{obsLC_op} Plot - Epoch1: {date_str} excl. el. below {el_flag}')
        else:
            ax8.title.set_text(f'{station} - {cod_or_ph}_{obsLC_op} Plot - Epoch1: {date_str}')
        if save_fig:
            f_save = f'{save_fig}{date_str}_{station}-{cod_or_ph}_{obsLC_op}_St_Epoch-{st_epoch}.png'
            fig8.savefig(f_save)
            print(f'Saved: {f_save}')
    else:
        if el_flag:
            ax8.title.set_text(f'{station} - {cod_or_ph}_{obsLC_op} Plot - {date_str} excl. el. below {el_flag}')
        else:
            ax8.title.set_text(f'{station} - {cod_or_ph}_{obsLC_op} Plot - {date_str}')
        if save_fig:
            f_save = f'{save_fig}{date_str}_{station}-{cod_or_ph}_{obsLC_op}.png'
            fig8.savefig(f_save)
            print(f'Saved: {f_save}')
    if show:
        fig8.show()
    return fig8,ax8
        


if __name__ == "__main__":

    # Introduce command line parser
    parser = argparse.ArgumentParser(
        description = '''Plot a variable (XYZ coord, ZTD, residuals) 
        vs. time with TRACE file as input'''
    )
    
    # Command line function arguments
    parser.add_argument("trace_file", 
        help = "Path to trace file to plot"
    )

    parser.add_argument("output_dir", 
        help = "Output directory"
    )

    parser.add_argument("-PPP", "--PPP", action='store_true',
        help = "Option to plot X,Y,Z station coordinate estimates in PPP mode"
    )

    parser.add_argument("-PPP_diff", "--PPP_diff",
        help = """Option to plot difference between X,Y,Z station coordinate 
        estimated in PPP mode via PEA vs. IGS estimates. Must include 
        directory to .snx file:
        e.g. -PPP_diff /data/acs/pea/proc/products/gfzWWWWD.snx """
    )

    parser.add_argument("-SPP", "--SPP", action='store_true',
        help = "Option to plot X,Y,Z station coordinate estimates in SPP mode"
    )

    parser.add_argument("-ZTD", "--ZTD", action='store_true',
        help = "Option to plot Zenith Tropospheric Delay estimates"
    )

    parser.add_argument("-ZTD_diff", "--ZTD_diff",
        help = """Option to plot difference between ZTD
        estimated in PPP mode via PEA vs. IGS/AC estimates. Must include 
        directory to .tro file:
        e.g. -ZTD_diff /data/acs/pea/proc/products/codWWWWD.tro """
    )

    parser.add_argument("-RES", "--RES", action='store_true',
        help = """Option to plot residuals with default settings: 
        all satellites IFLC-L1L2, Code, Postfit"""
    )

    parser.add_argument("-resid_sats", "--resid_sats", 
        help = """Option for plots of residual desired need to
        include -resid_sats and comma separated list of satellites: 
        e.g. -resid_sats G02,G05,G32"""
    )

    parser.add_argument("-resid_LC", "--resid_LC", 
        help = """Option to change Linear Combination plotted
        Options are: L1L2 (default) and L1L5 
        """
    )

    parser.add_argument("-resid_codpha", "--resid_codpha", 
        help = """Option to change residual plotted
        Options are: Code (default) and Phase 
        """
    )

    parser.add_argument("-resid_fit", "--resid_fit", 
        help = """Option to change whether pre or post fit are plotted
        Options are: Postfit (default) and Prefit 
        """
    )

    parser.add_argument("-obsLC_def", "--obsLC_def", action='store_true',
        help = """Option to plot default observation / LC with settings: 
        all satellites, gf12, Code"""
    )

    parser.add_argument("-obsLC_sats", "--obsLC_sats", 
        help = """Option to choose satellites to plot of Observations / LC,
        need to include -obsLC_sats and comma separated list of satellites: 
        e.g. -obsLC_sats G02,G05,G32"""
    )

    parser.add_argument("-obsLC_op", "--obsLC_op", 
        help = """Option to choose which Observation / Linear Combination to plot,
        need to include -obsLC_op and comma separated list of options: 
        Options are: L1, P1, L2, P2, L5, P5, mp1, mp2, mp5, gf12 (default), gf15, 
        gf25, mw12, mw15, mw25, wl12, wl15, wl25, if12, if15, if25.
        Passing "-obsLC_op ALL" will produce a plot for each one
        """
    )

    parser.add_argument("-obsLC_codpha", "--obsLC_codpha", 
        help = """Option to choose code and/or phase to plot
        Options are: Code (default) and Phase.
        To choose both, include as comma separated: Code,Phase
        """
    )

    parser.add_argument("-obsLC_all", "--obsLC_all", 
        help = """Option to produce a plot for all possibilities.
        All satellites on each plot, a plot for each Observation and LC 
        """
    )

    parser.add_argument("-st_epoch", "--st_epoch", 
        help = """Option to change start Epoch of plot
        Default: 0
        """
    )

    parser.add_argument("-en_epoch", "--en_epoch", 
        help = """Option to change end Epoch of plot
        Default: None
        """
    )
    
    parser.add_argument("-flex_st", "--flex_st", 
        help = """Optional highlighting of plot - Start Epoch
        """
    )

    parser.add_argument("-flex_en", "--flex_en", 
        help = """Optional highlighting of plot - End Epoch
        """
    )

    parser.add_argument("-y_lims", "--y_lims",
        help = """Option to set a lower and upper bound on plot.
        Must be comma separated, of the form: -y_lims -10,10 """
    )

    parser.add_argument("-diff_lag", "--diff_lag",
        help = """Optional lag difference plot, with y[n+1] - y[n] plotted 
        for all n up to (n-1). Need to include lag period following call
        e.g. -diff_lag 1 """
    )

    parser.add_argument("-excl_el_below", "--excl_el_below",
        help='''Option to ignore data below specified elevation angle, e.g. to 
        exclude below 15 degrees: -exc_el_below 15.0'''
    )

    parser.add_argument("-plot_elev", "--plot_elev", action = 'store_true',
        help='''Option to plot elevation angle along x-axis rather than datetime'''
    )
    
    # Get command line args:
    args = parser.parse_args()
    
    # Assign variables from command line args:
    trace_file = Path(args.trace_file)
    output_dir = args.output_dir

    # Optional arguments:
    # POS plots
    PPP = args.PPP
    PPP_diff = args.PPP_diff
    SPP = args.SPP
    # ZTD plots
    ZTD = args.ZTD
    ZTD_diff = args.ZTD_diff
    # Residual plot options
    RES = args.RES
    resid_sats = args.resid_sats
    resid_LC = args.resid_LC
    resid_codpha = args.resid_codpha
    resid_fit = args.resid_fit
    # Observations / LC plot options
    obsLC_def = args.obsLC_def
    obsLC_sats = args.obsLC_sats
    obsLC_op = args.obsLC_op 
    obsLC_codpha = args.obsLC_codpha
    obsLC_all = args.obsLC_all
    # Start/end plot options
    st_epoch = args.st_epoch
    en_epoch = args.en_epoch
    flex_st = args.flex_st
    flex_en = args.flex_en
    diff_lag = args.diff_lag
    y_lims = args.y_lims
    excl_el = args.excl_el_below
    el_plot = args.plot_elev

    if y_lims:
        
        ymin,ymax = y_lims.split(',')
        
        if ymin[0] == '/':
            ymin = ymin[1:]
        ymin = float(ymin)
        
        if ymax[0] == '/':
            ymax = ymax[1:]
        ymax = float(ymax)
    
    else:
        ymin = None
        ymax = None

    if el_plot & (not excl_el):
        excl_el = 0.001

    if excl_el:
        excl_el = float(excl_el)

    

    # Read data: 
    data = _read_trace(str(trace_file))
    data_res = None
    # Use old 
    if SPP:
        data_res = parseTRACEfile(trace_file)

    # Establish plot options (if any)
    if st_epoch:
        st_epoch = int(st_epoch)
    else:
        st_epoch = 0

    if en_epoch:
        en_epoch = int(en_epoch)
    else:
        en_epoch = None
    
    if not flex_st:
        flex_st = None
    
    if not flex_en:
        flex_en = None

    if not diff_lag:
        diff_lag = 0
    else:
        diff_lag = int(diff_lag)
        
    if data_res == None:
        # Get station and Epoch info:
        stations = list(set(data.iloc[st_epoch:].index.get_level_values(0).values))
        epochs = data.iloc[st_epoch:].index.get_level_values(1).values
        epoch1 = J2000_ORIGIN + np.timedelta64(epochs[0],'s')
        date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")
    else:
        date_str = data_res['Epoch1'].strftime("%Y-DOY-%j")
        stations = [data_res['station']]

    for station in stations:

        if PPP:
            pos_plot(
                data, 
                station, 
                st_epoch = st_epoch, 
                en_epoch = en_epoch,
                flex_st = np.datetime64(flex_st), 
                flex_en = np.datetime64(flex_en), 
                save_fig = output_dir,
                diff_lag = diff_lag)

        if PPP_diff:
            pos_diff_plot(
                data, 
                station, 
                PPP_diff, 
                st_epoch = st_epoch, 
                en_epoch = en_epoch,
                ymin = ymin,
                ymax = ymax,
                flex_st = np.datetime64(flex_st), 
                flex_en = np.datetime64(flex_en), 
                save_fig = output_dir)         

        if ZTD:
            trop_plot(
                data, 
                station, 
                st_epoch = st_epoch, 
                en_epoch = en_epoch,
                flex_st = np.datetime64(flex_st), 
                flex_en = np.datetime64(flex_en), 
                save_fig = output_dir,
                diff_lag = diff_lag)

        if ZTD_diff:
            trop_diff_plot(
                data, 
                station, 
                ZTD_diff, 
                st_epoch = st_epoch, 
                en_epoch = en_epoch,
                flex_st = np.datetime64(flex_st), 
                flex_en = np.datetime64(flex_en), 
                save_fig = output_dir)

        if SPP:
            spppos_plot(
                data_res, 
                station, 
                st_epoch = st_epoch, 
                en_epoch = en_epoch,
                flex_st = np.datetime64(flex_st), 
                flex_en = np.datetime64(flex_en), 
                save_fig = output_dir,
                diff_lag = diff_lag)

        if RES or resid_sats or resid_LC or resid_codpha or resid_fit:

            data = _read_trace_res(str(trace_file))

            # Assign chosen options or set defaults
            if not resid_LC:
                resid_LC = 'L1L2'

            if not resid_codpha:
                resid_codpha_list = ['Code']
            else:
                resid_codpha_list = resid_codpha.split(',')

            if not resid_fit:
                resid_fit_list = ['Postfit']
            else:
                resid_fit_list = resid_fit.split(',')

            if not resid_sats:
                resid_sats = sorted(list(set(data['PRN'].values)))
            else:
                resid_sats = resid_sats.split(',')

            for codpha in resid_codpha_list:
                for fit in resid_fit_list:
                    resid_plot(
                        data, 
                        station, 
                        LC = resid_LC, 
                        cod_or_ph = codpha, 
                        fit = fit, 
                        sats = resid_sats, 
                        st_epoch = st_epoch, 
                        en_epoch = en_epoch,
                        xlim1 = None, 
                        xlim2 = None, 
                        ymin = ymin,
                        ymax = ymax,
                        flex_st = np.datetime64(flex_st), 
                        flex_en = np.datetime64(flex_en), 
                        save_fig = output_dir,
                        diff_lag = diff_lag)

        if obsLC_def or obsLC_sats or obsLC_op or obsLC_codpha or obsLC_all:
            print('\n\nReading TRACE for Obs and LC data...')
            data = _read_trace_LC(str(trace_file))

            if excl_el:
                df_el = _read_trace_el(str(trace_file))
                df_el = df_el.reset_index().set_index(['time','PRN'])
                data = data.reset_index().set_index(['time','PRN'])
                data = data.join(df_el)
                data = data.reset_index().set_index('time')
                data = data[data['el'] > excl_el]

            # Assign chosen options or set defaults
            if not obsLC_codpha:
                obsLC_codpha_list = ['Code']
            else:
                obsLC_codpha_list = obsLC_codpha.split(',')

            if not obsLC_op:
                obsLC_op_list = ['g12']
            else:
                obsLC_op_list = obsLC_op.split(',')

            if not obsLC_sats:
                obsLC_sats_list = sorted(list(set(data['PRN'].values)))
            else:
                obsLC_sats_list = obsLC_sats.split(',')
            print('\nPlotting obs/LC...')
            
            if obsLC_def:
                obsLC_codpha_list = ['Code']
                obsLC_op_list = ['gf12']
                obsLC_sats_list = sorted(list(set(data['PRN'].values)))

            for codpha in obsLC_codpha_list:
                for obsLC_op in obsLC_op_list:
                    obsLC_plot(
                            data, 
                            station, 
                            obsLC_op = obsLC_op, 
                            cod_or_ph = codpha, 
                            sats = obsLC_sats_list, 
                            st_epoch = st_epoch, 
                            en_epoch = en_epoch,
                            xlim1 = None, 
                            xlim2 = None, 
                            ymin = ymin,
                            ymax = ymax,
                            flex_st = np.datetime64(flex_st), 
                            flex_en = np.datetime64(flex_en), 
                            save_fig = output_dir,
                            diff_lag = diff_lag,
                            el_flag = excl_el,
                            el_plot = el_plot)
                            #el_flag = el_flag)

