#!/usr/bin/python3
import matplotlib as plb
plb.use('Agg')

# python3 compareClock.py --standard /data/acs/pea/example/EX03/standard/igs20624.clk --test /data/acs/pea/example/EX03/standard/aus20624.clk
from matplotlib import rcParams
import matplotlib.pyplot as plt
import numpy as np
import os
import math
from numpy import loadtxt
from matplotlib.ticker import MultipleLocator

#import statsmodels.api as sm
import argparse
import datetime as dt
import re
#==============================================================================
def parseClockFile(clkfile,ds):

    endOfHeaderRGX = re.compile('END OF HEADER')
    rcvClkRGX      = re.compile('^AR')
    satClkRGX      = re.compile('^AS')

    headerFlag = 0

    with open(clkfile) as fstandard:
        for line in fstandard:
            line = line.rstrip()

            if headerFlag > 0:
                if rcvClkRGX.search(line):
                    station = line[3:7].strip()

                    YYYY = int(line[8:12].strip())
                    MM   = int(line[13:15].strip())
                    DD   = int(line[16:18].strip())
                    hh   = int(line[19:21].strip())
                    mm   = int(line[22:24].strip())
                    #ss   = int(line[24:34].strip())
                    ss   = int(line[24:27].strip())
                    val  = int(line[35:37].strip())
                    offset = float(line[38:59].strip().replace('D','E'))
                    if val > 1:
                        rate   = float(line[60:70].strip().replace('D','E'))

                    if station not in ds['station_clocks']['stations']:
                        ds['station_clocks']['stations'].append(station)
                        ds['station_clocks'][station] = {}
                        ds['station_clocks'][station]['epochs']  = []
                        ds['station_clocks'][station]['offsets'] = []
                        ds['station_clocks'][station]['rates']  = []
               
                    epoch = dt.datetime(YYYY,MM,DD,hh,mm,ss,00)
                    ds['station_clocks'][station]['epochs'].append(epoch)
                    ds['station_clocks'][station]['offsets'].append(offset)
                    if val > 1:
                        ds['station_clocks'][station]['rates'].append(rate)
                elif satClkRGX.search(line):
                    satprn = line[3:7].strip()

                    YYYY = int(line[8:12].strip())
                    MM   = int(line[13:15].strip())
                    DD   = int(line[16:18].strip())
                    hh   = int(line[19:21].strip())
                    mm   = int(line[22:24].strip())
                    ss   = int(line[24:27].strip())
                    val  = int(line[35:37].strip())
                    offset = float(line[38:59].strip().replace('D','E'))
                    #print(satprn,offset)
                    #exit(0)
                    if val > 1:
                        rate   = float(line[60:70].strip().replace('D','E'))
                    if satprn not in ds['satellite_clocks']['satprns']:
                        ds['satellite_clocks']['satprns'].append(satprn)
                        ds['satellite_clocks'][satprn] = {}
                        ds['satellite_clocks'][satprn]['epochs']  = []
                        ds['satellite_clocks'][satprn]['offsets'] = []
                        ds['satellite_clocks'][satprn]['rates']  = []
               
                    epoch = dt.datetime(YYYY,MM,DD,hh,mm,ss,00)
                    ds['satellite_clocks'][satprn]['epochs'].append(epoch)
                    ds['satellite_clocks'][satprn]['offsets'].append(offset)
                    if val > 1:
                        ds['satellite_clocks'][satprn]['rates'].append(rate)

            elif endOfHeaderRGX.search(line):
                #print("line:",line)
                headerFlag = 1
        
    return ds


#==============================================================================
parser = argparse.ArgumentParser(description='Compare clock solutions from processing runs of the pea',
        epilog='''\

    compareClock.py:\nthis script takes the clock files out put from the pea and plots the differences between the two runs.

    > compareClock.py --standard standardrunpath/aus1947.clk --test aus1947.clk

    you should be able to view the plots G01.png .. G32.png, std.png and rms.png

#==============================================================================

    ''')
        
parser.add_argument('--standard',dest='standard',default='',help="standard clock")
parser.add_argument('--test',dest='test',default='',help="test clock")
parser.add_argument('--pivot',dest='pivot',default='',help="pivot clock")
parser.add_argument('--pivot_sta',dest='pivot_sta',action='store_true',default=False,help="Pivot clock is a station clock")

args = parser.parse_args()
#==============================================================================
# speed of light 
c = 299792458.

standard = {}
standard['station_clocks'] = {}
standard['station_clocks']['stations'] = []
standard['satellite_clocks'] = {}
standard['satellite_clocks']['satprns'] = [] 

test= {}
test['station_clocks'] = {}
test['station_clocks']['stations'] = []
test['satellite_clocks'] = {}
test['satellite_clocks']['satprns'] = [] 

# TODO add outlier detection into comparison?? -lowess?
standard = parseClockFile(args.standard,standard)
test     = parseClockFile(args.test,test)

# set up the data structure for a single difference
diff = {} 
diff['station_clocks'] = {}
diff['station_clocks']['stations'] = []
diff['satellite_clocks'] = {}
diff['satellite_clocks']['satprns'] = [] 

# keep this value to make sure the pivot satellite has enough
# entries to be useful
max_epochs = 0
#
#  Calculate the differences
#  between the clock offset values
#  make sure we are taking differences at the same epoch
#
for clktype in ['satellite_clocks','station_clocks']:

    if clktype == 'satellite_clocks':
        mkr = 'satprns'
    elif clktype == 'station_clocks':
        mkr = 'stations'
    else:
        exit(-1)

    for satprn in standard[clktype][mkr]:
        if satprn not in test[clktype][mkr]:
            continue

        diff[clktype][satprn] = {}
        diff[clktype][satprn]['offsets'] = []
        diff[clktype][satprn]['epochs'] = []

        i  = 0
        t  = 0
    
        while( (t < np.size(test[clktype][satprn]['offsets'])) and (i < np.size(standard[clktype][satprn]['offsets'])) ):
            # check epochs match
            tst_epoch = test[clktype][satprn]['epochs'][t]
            std_epoch = standard[clktype][satprn]['epochs'][i]

            while( tst_epoch != std_epoch ):
                if std_epoch > tst_epoch:
                    #print("std_epoch > tst_epoch",i,t,std_epoch,tst_epoch)
                    t = t+1
                    tst_epoch = test[clktype][satprn]['epochs'][t]

                if std_epoch < tst_epoch:
                    #print("std_epoch < tst_epoch",i,t,std_epoch,tst_epoch)
                    i = i + 1
                    std_epoch = standard[clktype][satprn]['epochs'][i]

            diff[clktype][satprn]['offsets'].append( (test[clktype][satprn]['offsets'][t] - standard[clktype][satprn]['offsets'][i]) )
            diff[clktype][satprn]['epochs'].append( test[clktype][satprn]['epochs'][t] )
        
            # increment everything by one
            i  = i+1
            t  = t+1
            
        if np.size(diff[clktype][satprn]['epochs']) > max_epochs :
            max_epochs =  np.size(diff[clktype][satprn]['epochs'])

#==============================================================================
# Now take the difference from a reference pair of differences wrt to each other clock
# loop over the test data set of satellites, but check that it exists in the differences calculated above..
#==============================================================================
if args.pivot_sta:
    new_pivot_clktype = 'station_clocks'
else:
    new_pivot_clktype = 'satellite_clocks'

if len(args.pivot) > 0:
    new_pivot_sat = args.pivot
else:
    new_pivot_sat = 'G01'
    
ddiff = {}
ddiff['satellite_clocks'] = {}
ddiff['station_clocks'] = {}
#print("Double differnce pivot sat is:",pivot_sat,pivot_clktype)
stdev = {}
stdev['satellite_clocks'] = []
stdev['station_clocks'] = []
stdev['global'] = []

# do three iterations to try and find the best pivot clock
end = 3
minstd = np.inf

for itr in range(0,end):
    print("=====================================================================")
    print("iteration:",itr,new_pivot_sat,new_pivot_clktype)
    print("=====================================================================")
    pivot_sat = new_pivot_sat
    pivot_clktype = new_pivot_clktype

    for clktype in ['satellite_clocks','station_clocks']:

        if clktype == 'satellite_clocks':
            mkr = 'satprns'
        elif clktype == 'station_clocks':
            mkr = 'stations'
        else:
            exit(-1)

        for satprn in standard[clktype][mkr]:
            if satprn not in test[clktype][mkr]:
                continue
            elif satprn == pivot_sat:
                continue
            elif satprn not in diff[clktype]:
                continue

            ddiff[clktype][satprn] = {}
            ddiff[clktype][satprn]['offsets'] = []
            ddiff[clktype][satprn]['epochs'] = []

            i  = 0
            t  = 0
            # sync up the epochs
            while( (t < np.size(diff[clktype][satprn]['offsets']) ) and ( i < np.size(diff[pivot_clktype][pivot_sat]['offsets'])) ):
         
                # check epochs match
                tst_epoch = diff[clktype][satprn]['epochs'][t]
                std_epoch = diff[pivot_clktype][pivot_sat]['epochs'][i]

                while( tst_epoch != std_epoch ):

                    if std_epoch > tst_epoch:
                        #print("std_epoch > tst_epoch",i,t,std_epoch,tst_epoch)
                        t = t+1
                        tst_epoch = diff[clktype][satprn]['epochs'][t]

                    if std_epoch < tst_epoch:
                        #print("std_epoch < tst_epoch",i,t,std_epoch,tst_epoch)
                        i = i + 1
                        std_epoch = diff[pivot_clktype][pivot_sat]['epochs'][i]

                ddiff[clktype][satprn]['offsets'].append( (diff[pivot_clktype][pivot_sat]['offsets'][i] - diff[clktype][satprn]['offsets'][t]))
                ddiff[clktype][satprn]['epochs'].append( diff[clktype][satprn]['epochs'][t] )

                i = i+1
                t = t+1


        # make sure to clear the data structures before the next iteration.
        if itr == end-1:
            for satprn in standard[clktype][mkr]:
                if satprn not in test[clktype][mkr]:
                    continue
                elif satprn == pivot_sat:
                    continue
                elif satprn not in diff[clktype]:
                    continue
                #print("stdev:",satprn, np.std( ddiff[clktype][satprn]['offsets']) )
                stdev['global'].append( np.std( ddiff[clktype][satprn]['offsets']) )
                stdev[clktype].append( np.std( ddiff[clktype][satprn]['offsets']) )

                plt.figure()
                plt.title("Difference between pivot:"+pivot_sat+"and"+satprn)
                plt.plot(ddiff[clktype][satprn]['offsets'])
                plt.savefig(satprn)
        else:
            for satprn in standard[clktype][mkr]:
                if satprn not in test[clktype][mkr]:
                    continue
                elif satprn == pivot_sat:
                    continue
                elif satprn not in diff[clktype]:
                    continue

                # check to see if this is the lowest std clock
                # then asusme thi swould make the best pivot clock
                # need to run a check that it has an obsevation
                # for each available epoch or atleast 95%
                if np.std(ddiff[clktype][satprn]['offsets']) < minstd:
                    # do an epoch number check
                    cut_off = max_epochs - int(max_epochs/100 * 5)
                    print("checking",satprn,"has enough epochs",np.size(standard[clktype][satprn]['epochs']),np.size(test[clktype][satprn]['epochs']),cut_off)
                    if (np.size(standard[clktype][satprn]['epochs']) < cut_off ) or (np.size(test[clktype][satprn]['epochs']) < cut_off ) :
                       continue
                    minstd = np.std(ddiff[clktype][satprn]['offsets'])
                    new_pivot_clktype = clktype
                    new_pivot_sat = satprn
                    print("Changing pivot clock to :",satprn,minstd)
        
            ddiff[clktype] = {}

print("======")
print("satellite_clocks mean stdev  : ",np.mean(stdev['satellite_clocks']))
print("satellite_clocks median stdev: ",np.median(stdev['satellite_clocks']))
print("======")
print("station_clocks mean stdev    : ",np.mean(stdev['station_clocks']))
print("station_clocks median stdev  : ",np.median(stdev['station_clocks']))
print("======")
print("global mean stdev            : ",np.mean(stdev['global']))
print("global median stdev          : ",np.median(stdev['global']))
print("======")
exit(0)

fp = open('ga','w')
nv = 0
prns = []

# ==================== loop over the satellite PRN ====================
for i in range(1,ns+1):
    prn = "G"+str(i)
    if (i<10):
        prn = "G0"+str(i)

    k = 0
    sd_rec_all = []
    # loop over the products to be compared
    for prod in files:
        j = 0
        fmt = 'AS ' + prn 
        
        # check if prn result is in the product
        if fmt in open(prod).read():
            j = 1;

        if (j == 0):
            sw = 1
            print("No " + prn + ' in ' + prod)
            continue

        # grep the clock values from clk file
        ofile = "."+prod[0:3] + '_' + prn + '.txt'
        ofile1 = ofile + '1'
        cmd = 'grep \'AS ' + prn + '\' ' + prod + ' > ' + ofile
    
        cmd1 = 'awk \'{print $10}\' ' + ofile + '>' + ofile1
         
        os.system(cmd)
        os.system(cmd1)
        val = loadtxt(ofile1, comments="#", delimiter=",", unpack=False)
        
        if (len(val) != 2880):
            print('rec %s sat %s has epochs less than 2880' % (prod[0:3],prn))
            sw = 1
            continue
        
        k = k + 1 
        # single differencing between reference and user
        if (k == 1):    # reference product
            ref = np.array(list(map(float,val)))
        
        # 1st user product
        elif (k == 2):
            usr = np.array(list(map(float,val)))
            sd_rec = np.subtract(ref,usr)
            #print "rec sd %d %s " % (k,prod[0:3] + '-' + files[0][0:3])
        
        # 2nd, 3rd, ..., user products
        else: 
            usr = np.array(list(map(float,val)))
            
            #append all the user products in sd_rec
            sd_rec = np.append(sd_rec,np.subtract(ref,usr))
            #print "rec sd %d %s " % (k,prod[0:3] + '-' + files[0][0:3])
                     

    # omit satellites with less 2880 epochs
    if (sw == 1):
        sw = 0
        continue

    # assuming the first satellite has clock values
    if (i == 1):
        ref_s = sd_rec
    else:
        prns.append(prn)
        usr_s = sd_rec

        # labels and ticks
        day_start = 57841
        day_end = 57842
        xtlb = [None]*9
        xt = [None]*9
        j=0
        
        for i in range(0,25):
            if (i%3 == 0):
                if (i<10):
                    t = '0'+str(i)+':00'
                else:
                    t = str(i)+':00'
            
                xtlb[j] = t
                xt[j] = day_start+j*3.0/24.
                j = j+1

        rcParams['axes.linewidth'] = 3.0    # border line width
        plt.figure()
        for m in range(1,k):
            dd = np.subtract(ref_s[2880*(m-1):2880*m],usr_s[2880*(m-1):2880*m]) * c
        
            plt.plot(range(1,2880+1),dd,linestyle="-",linewidth='2.0',label=files[m][0:3].upper())
            plt.xlim(1,2880)
            plt.ylim(-0.5,0.5)
            plt.ylabel("(m)",fontsize=16)
            plt.xlabel("Hours",fontsize=16)
            #tks = [-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5]
            #plt.yticks(tks,fontsize=16)
            plt.title(prn+'-G01',fontsize=16)
            plt.xticks([0,720,1440,2160,2880],["0H","6H","12H","18H","24H"],fontsize=16)
            #plt.xticks(xt,xtlb,fontsize=16) 
            plt.rcParams['xtick.major.size'] = 8
            plt.rcParams['xtick.minor.size'] = 5
            plt.rcParams['ytick.major.size'] = 8
            plt.rcParams['ytick.minor.size'] = 5
            plt.legend()
            rms = 0
            std = 0
            #start = 2160
            #start = 1
            start = 1 #1920
            end = 2820
            #end = 2880
            ave = np.mean(dd[start:end])
            count = 0
            for it in range(start-1,end):
#            for it in range(1,2761):
                rms = rms+dd[it]*dd[it]
                std = std+(dd[it]-ave)*(dd[it]-ave)
                count = count + 1

            rms = math.sqrt(rms/count) * 1e9 / c
            std = math.sqrt(std/(count-1)) * 1e9 / c

            fp.write("%s %s rms(ns)= %8.4f std(ns)= %8.4f " 
                    % (files[m][0:3].upper(),prn, rms, std))
        fp.write('\n')

        #plt.grid()
        plt.gca().grid(which='major',axis='y',linestyle='--') 
        plt.show()
        plt.savefig(prn,bbox_inches='tight')
    nv = nv + 1
fp.close()

# excluding pivot satellite
nv = nv - 1

cmd = 'rm *.txt*'
os.system(cmd)


cmd = 'awk \'{print $4, $6, $10, $12, $16, $18}\' ga > ga1'
os.system(cmd)

# number of user ACs
na = len(files)-1

array_rms = np.loadtxt('ga1')[:,0:2*na:2]
array_std = np.loadtxt('ga1')[:,1:2*na:2]

# plot rms
plt.figure()
fg, ax = plt.subplots()
for i in range(0,na):
    plt.plot(range(1,nv+1),array_rms[:,i],linestyle="-",linewidth='2.0',label=files[i+1][0:3].upper())
    plt.legend()
#    ax.bar(range(1,nv+1),array_rms[:,i],color='b')
    plt.ylabel("(ns)",fontsize=16)
    plt.xticks(range(1,nv+1),prns,rotation='vertical',fontsize=16)
    plt.title('GPS clock RMS',fontsize=16)


plt.gca().grid(which='major',axis='y',linestyle='--') 
plt.show()
plt.savefig('rms',bbox_inches='tight')

# plot std
plt.figure()
fg, ax = plt.subplots()
for i in range(0,na):
    ax.bar(range(1,nv+1),array_std[:,i],color='b')
    #plt.plot(range(1,nv+1),array_std[:,i],linestyle="-",linewidth='2.0',label=files[i+1][0:3].upper())
    #plt.legend()
    plt.ylim(0.0,0.5)
    plt.ylabel("(ns)",fontsize=14)
    plt.xticks(range(1,nv+1),prns,rotation='vertical',fontsize=14)
    plt.yticks(fontsize=14)
    plt.title('GPS clock STD',fontsize=14)

plt.gca().grid(which='major',axis='y',linestyle='--') 
plt.show()
plt.savefig('std',bbox_inches='tight')


