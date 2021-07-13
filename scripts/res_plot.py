#!/usr/bin/env python

# This script is used for plotting the orbit residuals from precise orbit determiantion.
# Please change the path of file and file name. The figure is saved as *.png.

# Output file name format is expressed as year+DOY+hour(system time).

# Authors: Tzupang Tseng, Carl Wang and Salim Masoumi
#
# Date: 11-06-2019
#
# Modified: S McClusky 20-11-2019
#
import os
import sys, getopt
import argparse
from argparse import ArgumentParser
import time
import numpy as np
import matplotlib as mpl
mpl.use('Agg')
#mpl.use('Gtk')
import matplotlib.pyplot as plt
from matplotlib.pyplot import cm
import gc
import math

gnss = 'ALL'
prn_list = []

# Command line argument
parser = argparse.ArgumentParser()
   
parser.add_argument('-i', type=str, required=True, help='Input file name')
parser.add_argument('-d', type=str, required=False, default='.', help='Output directory path')
parser.add_argument('-c', type=str, required=False, default='ALL', help='GNSS Constellation [G,E,R,C or ALL]')
parser.add_argument('-p', nargs='+',type=int, required=False, help='List of PRN #s to plot')

args = parser.parse_args()

inputfile = args.i
outputdir = args.d
gnss = args.c
outputdir = os.path.abspath(outputdir)
prn_list = args.p
#print(prn_list)
#os.chdir(outputdir)

# End of command line argument
infile = open(inputfile,'r')
mat_1 = np.loadtxt(infile,skiprows=1)
prn = mat_1[:,1]
year  = time.strftime("%Y")
doy   = time.strftime("%j")
hr    = time.strftime("%H")
minu  = time.strftime("%M")

# Output file name format (same root as input file)
inputbase = os.path.basename(inputfile)
outputfile = inputbase.replace("_orbdiff_rtn", "")
file_plt_1 = outputdir + '/orbres_' + outputfile + '_' + gnss + '.png'

# Get list of PRN's to be plotted based on command line option (gnss) 
if prn_list:
    prns = prn_list
    gnss = "PRN: %s" %(', '.join(map(str,prns)))
    ext = "%s" %('_'.join(map(str,prns)))
    print ("Plotting ", gnss)
    print("")
    file_plt_1 = outputdir + '/orbres_' + outputfile + '_' + ext + '.png'
else:
    prns = np.unique(prn.astype(int))

    if gnss == 'G':
        prns = prns[(prns>=1) & (prns<=100)]
    elif gnss == 'E':
        prns = prns[(prns>=101) & (prns<=200)]
    elif gnss == 'R':
        prns = prns[(prns>=201) & (prns<=300)]
    elif gnss == 'C':
        prns = prns[(prns>=301) & (prns<=400)]

    print ("Plotting GNSS: ",gnss)
    print ("PRNS: ", *prns)
    print("")

#-------plot 1-1------------------------------------------------------------
f1 = plt.figure(figsize=(13,8))
ax1 = f1.add_subplot(311)
title = 'Satellite orbit residuals : GNSS - ' + gnss
plt.title(title,fontsize=20)
i = -1
#for I in range(1,int(max(prn))+1):
for I in prns:
    i=i+1
#    print(int(prns[i]))
    a = []
    b = []
    for x in range (0,len(prn)):
        if prn[x] == I:
           a.append(mat_1[x,10]*100)
           b.append((mat_1[x,0]-mat_1[0,0])*24)
    ax1.plot(b, a, 'o-', color=plt.cm.hsv(int(prns[i])),markersize=1,label="G%d"%(I,))
    leg = ax1.legend(loc='best', ncol=3, fancybox=True,bbox_to_anchor=(1, 1) )
    leg.get_frame().set_alpha(0.35)
 
box = ax1.get_position()
ax1.set_position([box.x0, box.y0, box.width * 0.8, box.height])

#hi_max_la = float(max(mat_1[:,10]))
#lo_min_la = float(min(mat_1[:,10]))
#plt.ylim(lo_min_la,hi_max_la)
ax1.set_xlim(0, 24)
ax1.set_ylabel("Radial (cm)",fontsize=16)
#plt.xlabel(" Time (hour)",fontsize=16)
plt.yticks(fontsize=16)
plt.xticks(fontsize=16)
#plt.ticklabel_format(useOffset=False, axis='y')
# plt.axvline(xv,lw=1,color="r")
ax1.grid()
#plt.legend(prop={'size':8}, loc=2)
#plt.show()

#-------plot 1-2----------------------------------------------------------      
ax2 = f1.add_subplot(312)
i = -1
#for I in range(int(max(prn))):
for I in prns:
    i=i+1
#    print(int(prns[i])) 
    a = []
    b = []
    for x in range (0,len(prn)):
        if prn[x] == I:
           a.append(mat_1[x,11]*100)
           b.append((mat_1[x,0]-mat_1[0,0])*24)
    ax2.plot(b, a, 'o-',  color=plt.cm.hsv(int(prns[i])), markersize=1)

box = ax2.get_position()
ax2.set_position([box.x0, box.y0, box.width * 0.8, box.height])
#hi_max_la = float(max(mat_1[:,11]))
#lo_min_la = float(min(mat_1[:,11]))
#plt.ylim(lo_min_la,hi_max_la)
ax2.set_xlim(0, 24)
ax2.set_ylabel("Along-track (cm)",fontsize=16)
#plt.xlabel("Time (hour)",fontsize=16)
plt.yticks(fontsize=16)
plt.xticks(fontsize=16)

#plt.ticklabel_format(useOffset=False, axis='y')
#plt.axvline(xv,lw=1,color="r")
ax2.grid()
#plt.legend(prop={'size':8}, loc=2)
#plt.show()
#-------plot 1-3-----------------------------------------------------------------------      
ax3 = f1.add_subplot(313)
i = -1
#for I in range(int(max(prn))):
for I in prns:
    i=i+1
#   print('Plotting PRN: ', int(prns[i])) 
    a = []
    b = []
    for x in range (0,len(prn)):
        if prn[x] == I:
           a.append(mat_1[x,12]*100)
           b.append((mat_1[x,0]-mat_1[0,0])*24)
    ax3.plot(b, a, 'o-', color=plt.cm.hsv(int(prns[i])), markersize=1)

box = ax3.get_position()
ax3.set_position([box.x0, box.y0, box.width * 0.8, box.height])
#hi_max_la = float(max(mat_1[:,12]))
#lo_min_la = float(min(mat_1[:,12]))
#plt.ylim(lo_min_la,hi_max_la)
ax3.set_xlim(0, 24)
ax3.set_ylabel("Cross-track (cm)",fontsize=16)
ax3.set_xlabel("Time (hour)",fontsize=16)
plt.yticks(fontsize=16)
plt.xticks(fontsize=16)

#plt.ticklabel_format(useOffset=False, axis='y')
#plt.axvline(xv,lw=1,color="r")
ax3.grid()
#plt.show()
#file_plt_1 = 'yearly_' + sta + '_neh.png'
f1.savefig(file_plt_1,dpi=150)
plt.clf()

# Print output filename and path
message = 'Output file: ' + file_plt_1
print(message)

