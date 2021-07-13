import os
import matplotlib
matplotlib.use("agg")
import matplotlib.pyplot as plt
from math import floor

default_file = "../../../PEA.SUM"

filename  = input("Input file (with full path): ")
#station = input("Select station (or type 'ALL'): ")
#satellt = input("Select satellite (or type 'ALL'): ")

satstring="G{:02d}"

os.system("grep '#ARES_ALL' " + filename + " >> logAmR.txt")
fpall=open("logAmR.txt")
time=[]
nall=[]
nwlf=[]
nnlf=[]
namb=[]
nepc=0
for line in fpall:
    llst=line.split()
    tim_=float(llst[1][:-1])
    if nepc==0:
        day=86400.0*floor(tim_/86400.0)
    time.append((tim_-day)/3600)
    n0=float(llst[ 2][:-1])
    n1=float(llst[ 3][:-1])
    n2=float(llst[ 4][:-1])
    n3=float(llst[ 5][:-1])
    nall.append(n0)
    namb.append(n3)
    nwlf.append(n1)
    nnlf.append(n2)
    nepc=nepc+1
fpall.close()
os.system("rm logAmR.txt")
if len(time)>10:
    nresol=plt.figure(num=0,dpi=1000.0)
    ax=nresol.add_subplot(1, 1, 1)
    ax.plot(time,nall,label=" Number of satellites")
    ax.plot(time,namb,label=" Number of pivot ambiguities")
    ax.plot(time,nwlf,label=" Number of fixed WL ambiguities")
    ax.plot(time,nnlf,label=" Number of fixed NL ambiguities")
    ax.legend(loc="best")
    nresol.savefig("Amb_all.png")
    plt.close(0)
    
    
os.system("grep '#ARES_SAT' " + filename + " >> logsat.txt")
for sat in range(1,32):
    satnam=satstring.format(sat)
    filenam = satnam + ".csv"
    os.system("grep " + satnam + " logsat.txt >> " + filenam)
    fpsat=open(filenam)
    time=[]
    wlfl=[]
    wlfx=[]
    nlfl=[]
    nlfx=[]
#    namball=[]
    nambWL=[]
    nambNL=[]
    nlfltint=0
    nlfixint=0
    nepc=0
    nlfltprev=0
    nlfixprev=0
    for line in fpsat:
        llst=line.split()
        tim_=float(llst[1][:-1])
        if nepc==0:
            day=86400.0*floor(tim_/86400.0)
        time.append((tim_-day)/3600)
        namball=float(llst[5][:-1])
        if namball==0:
            nambWL.append(0)
            nambNL.append(0)
        else:
            nambWL.append(float(llst[ 6][:-1])*100/namball)
            nambNL.append(float(llst[ 7][:-1])*100/namball)
        nlflt=float(llst[11][:-1])+nlfltint
        nlfix=float(llst[12][:-1])+nlfltint
        if nepc>0:
            fltdif=nlflt-nlfltprev
            fltdifint=round(fltdif)
            if fltdifint!=0:
                nlflt=nlflt-fltdifint
                nlfltint=nlfltint-fltdifint
#                print(str(tim_) + " NL flt bias jump of " + str(fltdif) + " in satellite " + satnam)
            fixdif =nlfix-nlfixprev
            fixdifint = round(fixdif)
            if fixdifint!=0:
                nlfix=nlfix-fixdifint
                nlfixint=nlfixint-fixdifint
#                print(str(tim_) + " NL fix bias jump of " + str(fixdif) + " in satellite " + satnam)
        
        wlfl.append(float(llst[ 9][:-1]))
        wlfx.append(float(llst[10][:-1]))
        nlfl.append(nlflt)
        nlfx.append(nlfix)
        nlfltprev=nlflt
        nlfixprev=nlfix
        nepc=nepc+1
    fpsat.close()
#    os.system("rm " + filenam)
    if len(time)>10:
        nresol=plt.figure(num=sat,dpi=1000.0)
        ax=nresol.add_subplot(1, 1, 1)
        ax.plot(time,wlfl,label=" WL bias (float)")
        ax.plot(time,wlfx,label=" WL bias (fixed)")
        ax.plot(time,nlfl,label=" NL bias (float)")
        ax.plot(time,nlfx,label=" NL bias (fixed)")
#        plt.ylim(-5,5)
        ax.legend(loc="best")
        nresol.savefig(satnam+"_biases.png")
        plt.close(sat)
        nresol=plt.figure(num=sat+100,dpi=1000.0)
        ax1=nresol.add_subplot(1, 1, 1)
#        ax1.plot(time,namball,label=" Val. Amb")
        ax1.plot(time,nambWL,label= " Fixed WL")
        ax1.plot(time,nambNL,label= " Fixed NL")
        ax1.legend(loc="best")
        nresol.savefig(satnam+"_ambiguities.png")
        plt.close(sat+100)

os.system("rm logsat.txt")