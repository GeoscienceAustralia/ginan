import cartopy.crs as ccrs
import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
from math import floor
import numpy as np

filename = input("Input filename: ")

tinp = input("Starting time (hhmmss): ")
hour=int(tinp[:2])
mint=int(tinp[2:4])
secd=int(tinp[4:])
tstart=(hour-1)*3600+mint*60+secd

tinp = input("Stoping time (hhmmss): ")
hour=int(tinp[:2])
mint=int(tinp[2:4])
secd=int(tinp[4:])
tstop=(hour-1)*3600+mint*60+secd

if tstart>tstop:
    print("Error, Start time is higher than stop time")
    exit()

finp=open(filename)
header=1
tepoc=-1
tecmap=0
outmap=0
latmin=-90.0
latmax=90.0
latint=5.0
lonmin=-180.0
lonmax=180.0
lonint=5.0
hgt1=0.0
fact=0.1
outname=' '
nmaps=0

for line in finp:
    #if len(line)<60:
    #    continue
    if header==1:                                           #printing the file header
       print(line)
       if 'HGT1 / HGT2 / DHGT' in line:
           llst=line.split()
           hgt1=float(llst[0])                              # will only map lowest layer
           continue
       if 'LAT1 / LAT2 / DLAT' in line:
           llst=line.split()
           latmin=float(llst[0])
           latmax=float(llst[1])
           latint=float(llst[2])
           latvct=np.arange(latmin,latmax+0.1,latint)
           continue
       if 'LON1 / LON2 / DLON' in line:
           llst=line.split()
           lonmin=float(llst[0])
           lonmax=float(llst[1])
           lonint=float(llst[2])
           lonvct=np.arange(lonmin,lonmax+0.1,lonint)
           continue
       if 'END OF HEADER' in line:
           if len(latvct)<=0:
               finp.close()
               exit()
           if len(lonvct)<=0:
               finp.close()
               exit()
           iondata=np.zeros((len(latvct),len(lonvct)))
           latind=len(latvct)
           header=0
       if 'EXPONENT' in line:
           llst=line.split()
           fact=10.0**float(llst[0])
           continue
       continue
   
    if 'START OF TEC MAP' in line:
        tecmap=1
        continue
    if 'END OF TEC MAP' in line:
        if outmap>0:
            print('Saving map: ' + outname)
            plt.clf()
            ax1 = plt.axes(projection=ccrs.PlateCarree())
            ax1.coastlines('50m')
            ax1.set_extent([lonmin,lonmax,latmin,latmax], ccrs.PlateCarree())
            plt.pcolormesh(lonvct,latvct,iondata)
            plt.colorbar(ax=ax1)
            plt.savefig(outname)
            plt.title(outname)
        tecmap=0
        nmaps=nmaps+1
        continue
    if tecmap==0:
        continue
   
    if 'EPOCH OF CURRENT MAP' in line:
        llst=line.split()
        tepoc=(int(llst[3])-1)*3600+int(llst[4])*60+int(llst[5])
        if tepoc>tstop:
            break
        if tepoc>tstart:
            outmap=1
            outname='IONEX_'+llst[0]+'-'+llst[1]+'-'+llst[2]+'_'+llst[3]+':'+llst[4]+':'+llst[5]
        else:
            outmap=0
        continue
    if outmap==0:
        continue
    
    if 'LAT/LON1/LON2/DLON/H' in line:
        if hgt1!=float(line[26:31]):
            latind=len(latvct)
            continue
        latind=0
        for lat in latvct:
            if lat==float(line[2:7]):
                break
            latind=latind+1
        lonind=0
        continue
    
    if latind>=len(latvct):
        continue
    if lonind>=len(lonvct):
        continue
    
    llst=line.split()
    for strg in llst:
        if lonind<len(lonvct):
            entry=float(strg)
            if entry>9900.0:
                entry=0.0
            if entry<1.0:
                entry=0.0
            iondata[latind,lonind]=entry*fact
        lonind=lonind+1

finp.close()