#!/usr/bin/env python

# This script is used for plotting the orbit RMS from precise orbit determiantion.
# Please change the path of file and file name. The figure is saved as *.png.

# Output file name format is expressed as year+DOY+hour(system time).

# Authors: Tzupang Tseng, Carl Wang and Salim Masoumi
#
# Date: 11-06-2019
#
#

import os
import argparse
from argparse import ArgumentParser
import time
import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import math

# File-related statements
#os.chdir('/data/test/')
gnss = 'ALL'
# Command line argument
parser = argparse.ArgumentParser()

parser.add_argument('-i', type=str, required=True, help='Input file name')
parser.add_argument('-d', type=str, required=False, default='.', help='Output directory path')
parser.add_argument('-c', type=str, required=False, default='.', help='GNSS Constellation [G,E,R,C,J or ALL]')

args = parser.parse_args()

inputfile = args.i
outputdir = args.d
gnss = args.c
outputdir = os.path.abspath(outputdir)

# End of command line argument

infile = open(inputfile, 'r')
mat_1 = np.loadtxt(inputfile,skiprows=1)
year = time.strftime("%Y")
doy  = time.strftime("%j")
hr   = time.strftime("%H")
minu = time.strftime("%M")
# Output file name format is expressed as year+DOY+hour(system time).
inputbase = os.path.basename(inputfile)
outputfile = inputbase.replace("_orbdiff_rtn.out", "")
file_plt_1 = outputdir + '/orbrms_' + outputfile + gnss + '.png'
message = 'Output file: ' + file_plt_1


# Get list of PRN's to be plotted based on command line option (gnss) 
prns = np.unique(mat_1[:,1])
if   gnss == 'G':
     prns = prns[(prns>=1) & (prns<=100)]
elif gnss == 'R':
     prns = prns[(prns>=101) & (prns<=200)]
elif gnss == 'E':
     prns = prns[(prns>=201) & (prns<=300)]
elif gnss == 'C':
     prns = prns[(prns>=301) & (prns<=400)]
elif gnss == 'J':
     prns = prns[(prns>=401) & (prns<=500)]

assert len(prns)!=0, f'No {gnss} GNSS SVs found. Please check the input or -c option'

# Print output filename and path
print(message)
print ('GNSS: ',gnss, 'PRNs: ',prns)

#-------plot 1-1------------------------------------------------------------
prn = mat_1[:,1]
plt.figure(figsize=(13,8))
plt.subplot(111)
title = 'Satellite orbit RMS : GNSS - ' + gnss 
bar_width = 0.25
#for I in range(1,int(max(prn))+1):
i = -1
rtot = []
ttot = []
ntot = []
r_all_std = []
t_all_std = []
n_all_std = []
for I in prns:
    i=i+1
    r = []
    t = []
    n = []
    for x in range (0,len(prn)):
        if prn[x] == I:
           r.append(mat_1[x,10]*100)
           t.append(mat_1[x,11]*100)
           n.append(mat_1[x,12]*100)
    rtot.append(r)
    ttot.append(t)
    ntot.append(n)
    r_mean= np.mean(r,dtype=np.float64)
    t_mean= np.mean(t,dtype=np.float64)
    n_mean= np.mean(n,dtype=np.float64)
    r_std = np.std(r,dtype=np.float64)
    t_std = np.std(t,dtype=np.float64)
    n_std = np.std(n,dtype=np.float64)
    r_rms = math.sqrt(r_mean**2+r_std**2)
    t_rms = math.sqrt(t_mean**2+t_std**2)
    n_rms = math.sqrt(n_mean**2+n_std**2)
    r_all_std.append(r_std)
    t_all_std.append(t_std)
    n_all_std.append(n_std)
# Check RMS calculation (all good SCM)
#    rrms = math.sqrt(sum(x*x for x in r)/len(r))
#    trms = math.sqrt(sum(x*x for x in t)/len(t))
#    nrms = math.sqrt(sum(x*x for x in n)/len(n))

#    print("PRN: Mean     R/T/N/3D: %3d : %6.3f %6.3f %6.3f %6.3f" %(I,r_mean,t_mean,n_mean,np.sqrt(r_mean**2+t_mean**2+n_mean**2)))
#    print("PRN: Std Dev  R/T/N/3D: %3d : %6.3f %6.3f %6.3f %6.3f" %(I,r_std,t_std,n_std,np.sqrt(r_std**2+t_std**2+n_std**2)))
    print("PRN: RMS      R/T/N/3D: %3d : %6.3f %6.3f %6.3f %6.3f" %(I,r_rms,t_rms,n_rms,np.sqrt(r_rms**2+t_rms**2+n_rms**2)))
 
    if i ==0:
       plt.bar(I-bar_width, r_rms, bar_width,alpha=0.8, color='r', align='center', label='Radial')
       plt.bar(I, t_rms, bar_width, alpha=0.8, color='g', align='center', label='Along-track')
       plt.bar(I+bar_width, n_rms, bar_width,alpha=0.8, color='b', align='center', label= 'Cross-track')
       plt.legend()
    else:
        plt.bar(I-bar_width, r_rms, bar_width, color='r', align='center')
        plt.bar(I, t_rms, bar_width, color='g', align='center')
        plt.bar(I+bar_width, n_rms, bar_width, color='b', align='center')

rtot_mean= np.mean(rtot,dtype=np.float64)
ttot_mean= np.mean(ttot,dtype=np.float64)
ntot_mean= np.mean(ntot,dtype=np.float64)
rtot_std = np.std(rtot,dtype=np.float64)
ttot_std = np.std(ttot,dtype=np.float64)
ntot_std = np.std(ntot,dtype=np.float64)
rtot_rms = math.sqrt(rtot_mean**2+rtot_std**2)
ttot_rms = math.sqrt(ttot_mean**2+ttot_std**2)
ntot_rms = math.sqrt(ntot_mean**2+ntot_std**2)
r_mean_std = np.mean(r_all_std,dtype=np.float64)
t_mean_std = np.mean(t_all_std,dtype=np.float64)
n_mean_std = np.mean(n_all_std,dtype=np.float64)

print(" ")
print("Combined statistics")
print("PRN: Mean     R/T/N/3D:  ALL: %6.3f %6.3f %6.3f %6.3f" %(rtot_mean,ttot_mean,ntot_mean,np.sqrt(rtot_mean**2+ttot_mean**2+ntot_mean**2)))
print("PRN: Std Dev  R/T/N/3D:  ALL: %6.3f %6.3f %6.3f %6.3f" %(rtot_std,ttot_std,ntot_std,np.sqrt(rtot_std**2+ttot_std**2+ntot_std**2)))
print("PRN: RMS      R/T/N/3D:  ALL: %6.3f %6.3f %6.3f %6.3f" %(rtot_rms,ttot_rms,ntot_rms,np.sqrt(rtot_rms**2+ttot_rms**2+ntot_rms**2)))
print("PRN: Mean STD R/T/N/3D:  ALL: %6.3f %6.3f %6.3f %6.3f" %(r_mean_std,t_mean_std,n_mean_std,np.sqrt(r_mean_std**2+t_mean_std**2+n_mean_std**2)))

plt.xlim( int(min(prns))-1, int(max(prns))+1 )
plt.yticks(fontsize=16)
plt.xticks(fontsize=16)
plt.ylabel('RMS (cm)',fontsize=20)
plt.xlabel('Satellite PRN number',fontsize=20)
plt.title(title,fontsize=20)
plt.grid()
plt.savefig(file_plt_1,dpi=150)

