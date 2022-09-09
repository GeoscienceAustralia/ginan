#!/usr/bin/python
# --------------------------------------------------------------
# -------                       comprun.py
# ------- compare rec and sat clk from different runs
# ------- Date: 08/2019
# ------- Input:
# -------      grep 'network xp' netprocessing.out > xp.out
# ------- Author:  Tao Li
# ------- (c) Geoscience Australia & CRC-SI
# --------------------------------------------------------------

import matplotlib as plb
#plb.use('Gtk')
plb.use('Agg')

from matplotlib import rcParams
import matplotlib.pyplot as plt
import numpy as np
from numpy import loadtxt

import argparse
#==============================================================================
parser = argparse.ArgumentParser(description='Compare processing runs of the pea',
        epilog='''\

    comprun.py this script takes the grepd out put from netprocessing.out and plots the differences between the two runs.
    This is useful to test the compiled system produces the same results as those achieved from the release version.
    After running the program plots showing the difference between positions, receiver clocks , satellite clocks and ZTD
    estimates are provided.

    For you latest run do:

    > grep ‘network xp’ netprocessing.out > xp_test.txt,

    For the standard version:

    > grep 'network xp' standardrunpath/netprocessingout > xp_standard.txt,

    and then run:

    > comprun.py --standard standardrunpath/xp_standard.txt --test netprocessing.out’,

    you should be able to get the plots ‘Posdiff.png’, ‘recclk.png’, ‘satclk.png’ and ‘zwd.png’
#==============================================================================

    ''')


parser.add_argument('--standard',	dest='standard',help="File to compare the processing run to")
parser.add_argument('--test',		dest='test',	help="Latest processing results")

args = parser.parse_args()
#==============================================================================
file1 = args.standard #raw_input("first run: \n")
file2 = args.test     #raw_input("second run: \n")

# number of receivers
nr = 87# int(raw_input("receiver number: \n"))
optpos = 1 #int(raw_input("estimating position 0 or 1: \n"))

# number of pos
npos = 0;
if optpos == 1 :
    npos = 3*nr
# number of zwds
nt = nr

# number of satellites
ns = np.loadtxt(file1, usecols = (3,), unpack = True)
ns = int (ns[0])

# number of unknowns
na = npos+nr-1+nt+ns

sol1 = loadtxt(file1, usecols = np.arange(4,na+5), unpack = True)
sol2 = loadtxt(file2, usecols = np.arange(4,na+5), unpack = True)

ne1 = len(sol1[0])
ne2 = len(sol2[0])

ne=ne1
if (ne1>ne2):
    ne=ne2

# difference between two runs
#sol = np.subtract(sol1,sol2)
sol = np.subtract(sol1[:,:ne],sol2[:,:ne])

# number of epochs
#ne = len(sol[0])

# x label
nh = int(ne/120)
intv = 6
xlb = []
xt =  []
j = 0
for i in range(0,nh+1):
    if (i%intv == 0):
        xlb.append(format(i,"02")+'H')
        xt.append(i*120)
        j = j+1
print( xlb, xt)

# data for receiver clock, zwd, satellite clock
if optpos == 1:
    rpos 	= sol[0				: npos-1]
rclk 		= sol[npos			: npos+nr-1]
zwd 		= sol[npos+nr-1		: npos+nr-1+nt]
satclk 		= sol[npos+nr-1+nt	: na]


# figure plotting for receiver position
rcParams['axes.linewidth'] = 3.0

if optpos == 1:
    fgrclk = plt.figure()
    for i in range(0,npos-1):
        plt.plot(sol[i],linewidth=2.0)
    plt.xlim(1,ne)
    plt.ylim(-0.2,0.2)
    #plt.ylim(-10,10)
    plt.xlabel('Epoch',fontsize=14)
    plt.ylabel('Difference (m)',fontsize=14)
    plt.xticks(xt,xlb,fontsize=14)
    plt.yticks(fontsize=14)
    plt.title('Pos diff',fontsize=14)
    plt.gca().grid(which='major', axis='y',linestyle='--')
    plt.show()
    plt.savefig('Posdiff',bbox_inches='tight')


# figure plotting for receiver clock
fgrclk = plt.figure()
for i in range(0,nr-1):
    plt.plot(sol[i],linewidth=2.0)
plt.xlim(1,ne)
plt.ylim(-0.2,0.2)
#plt.ylim(-10,10)
plt.xlabel('Epoch',fontsize=14)
plt.ylabel('Difference (m)',fontsize=14)
plt.xticks(xt,xlb,fontsize=14)
plt.yticks(fontsize=14)
plt.title('Rec clock',fontsize=14)
plt.gca().grid(which='major', axis='y',linestyle='--')
plt.show()
plt.savefig('recclk',bbox_inches='tight')

# figure plotting for zwd
fgzwd = plt.figure()
fig, ax = plt.subplots()
for i in range(nr-1,nr-1+nr):
    plt.plot(sol[i],linewidth=2.0)
plt.xlim(1,ne)
plt.ylim(-0.2,0.2)
#plt.ylim(-1.0,1.0)
plt.xlabel('Epoch',fontsize=14)
plt.ylabel('Difference (m)',fontsize=14)
plt.xticks(xt,xlb,fontsize=14)
plt.yticks(fontsize=14)
#ax.tick_params(axis='y',which='minor',bottom=False)
plt.title('ZWD',fontsize=14)
plt.gca().grid(which='major', axis='y',linestyle='--')
plt.show()
plt.savefig('zwd',bbox_inches='tight')

# figure plotting for satellite clock
fgsclk = plt.figure()
for i in range(nr-1+nr,na):
    plt.plot(sol[i],linewidth=2.0)
plt.xlim(1,ne)
#plt.ylim(-1.0,1.0)
plt.ylim(-0.2,0.2)
plt.xlabel('Epoch',fontsize=14)
plt.ylabel('Difference (m)',fontsize=14)
plt.xticks(xt,xlb,fontsize=14)
plt.yticks(fontsize=14)
plt.title('Sat clock',fontsize=14)
plt.gca().grid(which='major', axis='y',linestyle='--')
plt.show()
plt.savefig('satclk',bbox_inches='tight')

