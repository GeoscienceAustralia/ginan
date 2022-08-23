#!/usr/bin/python3
import matplotlib as plb
plb.use('Agg')

# python3 compareClock.py --standard /data/acs/pea/example/EX03/standard/igs20624.clk --test /data/acs/pea/example/EX03/standard/aus20624.clk
from matplotlib import rcParams
import matplotlib.pyplot as plt
import numpy as np
import os
from pathlib import Path
import math
from numpy import loadtxt
from matplotlib.ticker import MultipleLocator

import argparse
import datetime as dt
import re

#==============================================================================
def parsePPPOutputFile(pppfile): #, ds, satels):

    recPOSRGX   = re.compile('REC_POS\s+(\w+)\s+(\d+)\s+(-?[\d\.]*)\s+(-?[\d\.]*)\s+(-?[\d\.]*)')
    
    output = {}

    count = 0
    with open(pppfile) as fstandard:
        for line in fstandard:
            line = line.rstrip()

            if( recPOSRGX.search(line) ):
                match = recPOSRGX.search(line)
                #print("RECpos match",match[1],match[2],match[3],match[4],match[5])

                if ( 'recPos' not in output) :
                    output['recPos'] = {}
                    output['recPos']['stns'] = []
                
                if ( match[1] not in output['recPos']['stns'] ): 
                    output['recPos']['stns'].append(match[1])
                    output['recPos'][match[1]] = {}
                    output['recPos'][match[1]]['X'] = [] 
                    output['recPos'][match[1]]['Y'] = [] 
                    output['recPos'][match[1]]['Z'] = [] 

                if ( int(match[2]) == 0):  
                    output['recPos'][match[1]]['X'].append(np.float(match[3])) # = [] 
                elif ( int(match[2]) == 1): 
                    output['recPos'][match[1]]['Y'].append(np.float(match[3])) # = [] 
                elif ( int(match[2]) == 2): 
                    output['recPos'][match[1]]['Z'].append(np.float(match[3]))
                
    return output 


def get_sinex_recpos(snxfile, station):
    '''
    Given the SINEX file and station name, return the X,Y,Z coords of receiver position (REC POS)
    
    Input
    snxfile - directory that holds the sinex file - str
    station - station name in 4 char format: SSSS - str

    Output
    X,Y,Z - coords of the receiver position - 3-tuple of floats
    STD_X, STD_Y, STD_Z - standard deviation of coords = 3-tuple of floats 
    '''

    sfile = Path(snxfile)
    with open(sfile, 'r') as f:

        # Exit flag
        ex = 0

        while ex == 0:
            
            # Read lines until station info is reached:
            line = f.readline()

            # Once you reach the station POS estimate, find the station of interest
            if '+SOLUTION/ESTIMATE' in line:
                
                # Skip next line
                line = f.readline()

                # Search for station name
                station_found = 0

                while station_found == 0:
                    
                    line = f.readline()

                    if line[14:18] == station:
                        
                        if line[7:11] == 'STAX':
                            xval = float(line[47:68])
                            xstd = float(line[69:-1])
                        elif line[7:11] == 'STAY':
                            yval = float(line[47:68])
                            ystd = float(line[69:-1])
                        elif line[7:11] == 'STAZ':
                            zval = float(line[47:68])
                            zstd = float(line[69:-1])
                            station_found = 1
                            ex = 1
                        
                    if '-SOLUTION/ESTIMATE' in line:
                        print('>>>>>>>Station Not Found<<<<<<<<<<<<')
                        station_found = 1
                        ex = 1 
            
            else:
                continue

    return (xval, yval, zval),(xstd,ystd,zstd)
    

def get_date(pppfile, multi=False):
    '''
    From the filename, get out the date so it can be used in naming the figures
    '''
    
    f1d = pppfile.split('.')
    f1s = f1d[-2].split('/')
    f1l = f1s[-1].split('_')

    for f in f1l:
        try:
            idate = int(f)
        except ValueError:
            if (len(f) == 2) & ('D' in f):
                multi = f
            continue

    try:
        idate
    except NameError:
        idate = int(f1l[1][7:])
    
    if multi:
        idate = str(idate) + f'_multi_{multi}'
    
    return str(idate)

def plot_PPP(pppfile, snxfile, multi=False):
    '''
    Taking in the .PPP file and snx file, produce 2 plots:
    * POS - mean(POS)
    * POS - Sinex soln
    Saved to pwd
    '''
    results = parsePPPOutputFile(pppfile)#, results, satels)
    

    #==============================================================================
    ## Plot the results up
    #==============================================================================

    for stn in (results['recPos']['stns']):
        snx,stds = get_sinex_recpos(snxfile, stn)
        print("Plotting station:",stn)

        if multi:
            date_str = get_date(pppfile, multi=multi)
        else:
            date_str = get_date(pppfile)
        # Figure 1: results compared to median
        fig1,ax1 = plt.subplots(1,1,figsize=(13,8))
        plt.title(stn+" Estimated pos - median position_"+date_str[:4]+'_DOY'+date_str[4:7]+'_H'+date_str[7:])
        
        X = results['recPos'][stn]['X'][:] - np.median(results['recPos'][stn]['X'][:])
        Y = results['recPos'][stn]['Y'][:] - np.median(results['recPos'][stn]['Y'][:])
        Z = results['recPos'][stn]['Z'][:] - np.median(results['recPos'][stn]['Z'][:])
        
        medZ =np.median(results['recPos'][stn]['Z'][:])
        
        ax1.plot(X,label='X') #,results['std1'],'o',alpha=ALPHA,label='East sigma')
        ax1.plot(Y,label='Y') #,results['std2'],'o',alpha=ALPHA,label='North sigma')
        ax1.plot(Z,label='Z') #,results['std3'],'o',alpha=ALPHA,label='Up sigma')
        
        ax1.legend(loc='best')
        ax1.grid(True)
        
        ax1.set_xlabel('Epoch')
        ax1.set_ylabel('Difference (m)')
        
        figsavename1 = stn+f'_pos__{date_str}.png'
        fig1.savefig(figsavename1)
        print("Saved the plot:", figsavename1)


        # Figure 2: results compared to snx solution
        # Plot the difference with respect to the SINEX solution
        fig2,ax2 = plt.subplots(1,1,figsize=(13,8))
        plt.title(stn+" Estimated pos - sinex position_"+date_str[:4]+'_DOY'+date_str[4:7]+'_H'+date_str[7:])
        
        snxX = np.array(results['recPos'][stn]['X'][:]) - snx[0]
        snxY = np.array(results['recPos'][stn]['Y'][:]) - snx[1]
        snxZ = np.array(results['recPos'][stn]['Z'][:]) - snx[2]

        ax2.plot(snxX,label='X') 
        ax2.plot(snxY,label='Y') 
        ax2.plot(snxZ,label='Z')
        
        plt.yscale('symlog', linthresh=0.1)
        
        ax2.grid(True)
        ax2.legend(loc='best')
        ax2.set_xlabel('Epoch')
        ax2.set_ylabel('Difference (m)')
        #plt.grid('true')

        figsavename2 = stn+f'_snx_pos_{date_str}.png'
        fig2.savefig(figsavename2)
        print("Saved the plot:", figsavename2)
        return figsavename1, figsavename2



#==============================================================================
parser = argparse.ArgumentParser(description='Compare clock solutions from processing runs of the pea',
        epilog='''\

    pppPlot.py:\nthis script takes the output from the pea and plots the differences between the median of the estimated coordinates, and then those provided in the IGS SINEX solution.

    To run, first grep the reported receiver positions reported in the station specific trace files to a seperate file

    > grep "REC_POS" /data/acs/pea/output/exs/EX01_IF/EX01_IF-ALIC201919900.TRACE > ALIC_201919900.PPP

    > pppPlot.py --ppp output/EX01_IF_PPP/ALIC_201919900.PPP --snx proc/exs/products/igs19P2062.snx

    you should be able to view the plots ALIC

#==============================================================================

    ''')


if __name__ == "__main__":

    parser.add_argument('--ppp', dest = 'pppfile', default = '', help = "Directory of PPP file")
    parser.add_argument('--snx', dest = 'snxfile', default = '', help = "Directory of SNX file")

    args = parser.parse_args()
    #==============================================================================

    if not args.pppfile:
        print("Usage: pppPlot.py --ppp <output/EX01_IF_PPP/ALIC_201919900.PPP> --snx <products/igs19P2062.snx>")
        print("Outputs: <station>_pos.png, <station>_snx_pos.png")
        raise SystemExit

    plot_PPP(args.pppfile, args.snxfile)




