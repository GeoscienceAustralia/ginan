import os
import matplotlib
matplotlib.use("agg")
import matplotlib.pyplot as plt
from math import sqrt

conf_file='/home/ubuntu/environment/pea/config/Ex01-PPP.yaml'
data_directory='/data/acs/pea/proc/exs/data/'
outp_directory='/data/acs/pea/output/exs/Ex01/'

testname = 'Ex01'
maxruntim='55m'

fpsum=open("ppptest_sum.txt",mode="w")

for r,d,f in os.walk(data_directory):
    for file in f:
        if '.rnx' in file:
            #print(file)
            station=file[:4]
            outpfile= testname + "-"+station+".TRACE"
            errlook=1
            comm="timeout "+maxruntim+" ./pea --config " + conf_file
            os.system(comm + " --rnx " + data_directory + file + " > log/"+station+".log")
            os.system("grep PPP_DIFF " + outp_directory + outpfile + " > pppout.txt")
            os.system("grep SPP_DIFF " + outp_directory + outpfile + " > sppout.txt")
            
            pppt=[]
            pppe=[]
            pppn=[]
            pppu=[]
            pppae=[]
            pppan=[]
            pppau=[]
            ppphor=[]

            sppt=[]
            sppe=[]
            sppn=[]
            sppu=[]
            sppae=[]
            sppan=[]
            sppau=[]
            spphor=[]
            epoc=0
            tim0=0
            tim1=0

            fpspp=open("sppout.txt")
            for line in fpspp:
                llst=line.split()
                if len(llst)<6:
                    continue
                est1=float(llst[4])
                nrt1=float(llst[5])
                vrt1=float(llst[6])
                sppt.append(float(llst[3]))
                sppe.append(est1)
                sppn.append(nrt1)
                sppu.append(vrt1)
                sppae.append(abs(est1))
                sppan.append(abs(nrt1))
                sppau.append(abs(vrt1))
                spphor.append(sqrt(est1*est1+nrt1*nrt1))
            fpspp.close()
            if len(sppt)<10:
                continue
            sppae.sort()
            os.system("rm sppout.txt")
            sppan.sort()
            sppau.sort()
            spphor.sort()
            indspp=int(0.9525*len(sppt))

            fpppp=open("pppout.txt")
            for line in fpppp:
                llst=line.split()
                if epoc==0:
                     tim0=float(llst[3])
                epoc=epoc+1
                tim1=float(llst[3])
                est1=float(llst[4])
                nrt1=float(llst[5])
                vrt1=float(llst[6])
    
                if (errlook>0) and ((nrt1>20.0 or nrt1<-20.0) or (est1>20.0 or est1<-20.0)):
                    mess="Warning: large error at "+station +" " + llst[3] + " : east= " + llst[4] + " nort= "+ llst[5]
                    print(mess)
                    fpsum.write(mess)
                    errlook=-2
                if (errlook>0) and ((tim1-tim0)>1800):
                    if (nrt1>2.0 or nrt1<-2.0) or (est1>2.0 or est1<-2.0):
                        mess="Warning: large error at "+station +" " + llst[3] + " : east= " + llst[4] + " nort= "+ llst[5]
                        print(mess)
                        fpsum.write(mess)
                        errlook=-1
                pppt.append(tim1)
                pppe.append(est1)
                pppn.append(nrt1)
                pppu.append(vrt1)
                pppae.append(abs(est1))
                pppan.append(abs(nrt1))
                pppau.append(abs(vrt1))
                ppphor.append(sqrt(est1*est1+nrt1*nrt1))
            fpppp.close()
            os.system("rm pppout.txt")
            pppae.sort()
            pppan.sort()
            pppau.sort()
            ppphor.sort()
            indppp=int(0.9525*len(pppt))


            plt.subplot(2,1,1)
            labl='spp east ({:.3}@95%)'.format(sppae[indspp])
            plt.plot(sppt, sppe, label=labl)
            labl='spp nort ({:.3}@95%)'.format(sppan[indspp])
            plt.plot(sppt, sppn, label=labl)
            labl='spp vert ({:.3}@95%)'.format(sppau[indspp])
            plt.plot(sppt, sppu, label=labl)
            plt.legend(loc="best")

            plt.subplot(2,1,2)
            labl='ppp east ({:.3}@95%)'.format(pppae[indppp])
            plt.plot(pppt, pppe, label=labl)
            labl='ppp nort ({:.3}@95%)'.format(pppan[indppp])
            plt.plot(pppt, pppn, label=labl)
            labl='ppp vert ({:.3}@95%)'.format(pppau[indppp])
            plt.plot(pppt, pppu, label=labl)
            #plt.ylim(-0.3,0.3)
            plt.legend(loc="best")

            plt.savefig(station+"_poserr.png")
            plt.close()
            os.system("mv "+station+"_poserr.png fig")
            mess="{} SPP: hor(95%)= {:.3}; ver(95%)= {:.3}. PPP: hor(95%)= {:.3};   ver(95%)= {:.3}".format(station,spphor[indspp],sppau[indspp],ppphor[indppp],pppau[indppp])
            print(mess)
            fpsum.write(mess)
fpsum.close()
#os.system("mv ppptest_sum_"+date+".txt ~/environment/")