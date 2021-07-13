#!/usr/bin/env python
# coding: utf-8

# In[1]:


pea1 = '/ubuntu/data/acs/pea/output/2019/199ex01/ALIC201919900.TRACE'
ppp1 = '/ubuntu/data/acs/output/ALIC.trace'
import seaborn as sns
import re
import math
import numpy as np
import matplotlib.pyplot as plt

sns.set()


# In[ ]:





# In[2]:


index = 0
poses = {}
trops = {}
tropsstd = {}
cloks = {}
cloksstd = {}
pos0 = {}
pos1 = {}
pos2 = {}
pos0std = {}
pos1std = {}
pos2std = {}
ambsssg = {}
ambsssg2 = {}
ambsss = {}
ambsss2 = {}
ambsssstd = {}

for [amb, ambg, file] in [[ambsss,ambsssg,ppp1], [ambsss2,ambsssg2,pea1]]:
    with open(file, 'r') as tracefile:
        tracefile = tracefile.read()
        poses[index] = re.findall(r'\$POS,\d+,\d+\.\d+,\d+,(-?\d+\.\d+),(-?\d+\.\d+),(-?\d+\.\d+),(-?\d+\.\d+),(-?\d+\.\d+),(-?\d+\.\d+)', tracefile)
        trops[index] = re.findall(r'\$TROP,\d+,\d+\.\d+,\d+,\d+,(\d+\.\d+)', tracefile)
        tropsstd[index] = re.findall(r'\$TROP,\d+,\d+\.\d+,\d+,\d+,\d+\.\d+,(\d+\.\d+)', tracefile)
        cloks[index] = re.findall(r'\$CLK,\d+,\d+\.\d+,\d+,\d+,(\d+\.\d+)', tracefile)
        cloksstd[index] = re.findall(r'\$CLK,\d+,\d+\.\d+,\d+,\d+,\d+\.\d+,\d+\.\d+,(\d+\.\d+)', tracefile)
        for sat in range(1,10):
            amb[sat] = {}
            ambg[sat] = {}
            ambsssstd[sat] = {}
            amb[sat] = re.findall(r'\$AMB,\d+,\d+\.\d+,\d+,G0' + str(sat) + ',\d+,(\d+\.\d+)', tracefile)
            ambg[sat] = re.findall(r'\$AMB,\d+,\d+\.\d+,\d+,R0' + str(sat) + ',\d+,(\d+\.\d+)', tracefile)
            ambsssstd[sat][index] = re.findall(r'\$AMB,\d+,\d+\.\d+,\d+,G22,\d+,\d+\.\d+,\d+\.\d+,(\d+\.\d+)', tracefile)

        for sat in range(10,33):
            amb[sat] = {}
            ambg[sat] = {}
            ambsssstd[sat] = {}
            amb[sat] = re.findall(r'\$AMB,\d+,\d+\.\d+,\d+,G' + str(sat) + ',\d+,(\d+\.\d+)', tracefile)
            ambg[sat] = re.findall(r'\$AMB,\d+,\d+\.\d+,\d+,R' + str(sat) + ',\d+,(\d+\.\d+)', tracefile)
            ambsssstd[sat][index] = re.findall(r'\$AMB,\d+,\d+\.\d+,\d+,G22,\d+,\d+\.\d+,\d+\.\d+,(\d+\.\d+)', tracefile)

        for sat in range(1,33):
            amb[sat] = [float(strngs) for strngs in amb[sat]]
            ambg[sat] = [float(strngs) for strngs in ambg[sat]]
            ambsssstd[sat][index] = [float(strngs) for strngs in ambsssstd[sat]]

        trops[index] = [float(strngs) for strngs in trops[index]]
        tropsstd[index] = [float(strngs) for strngs in tropsstd[index]]
        cloks[index] = [float(strngs) for strngs in cloks[index]]
        cloksstd[index] = [float(strngs) for strngs in cloksstd[index]]

        pos0[index] = [float(strngs[0]) for strngs in poses[index]]
        pos1[index] = [float(strngs[1]) for strngs in poses[index]]
        pos2[index] = [float(strngs[2]) for strngs in poses[index]]

        pos0std[index] = [float(strngs[3]) for strngs in poses[index]]
        pos1std[index] = [float(strngs[4]) for strngs in poses[index]]
        pos2std[index] = [float(strngs[5]) for strngs in poses[index]]

        pos0m = np.mean(pos0[index])
        pos0[index] = [pos - pos0m for pos in pos0[index]]
        pos1m = np.mean(pos1[index])
        pos1[index] = [pos - pos1m for pos in pos1[index]]
        pos2m = np.mean(pos2[index])
        pos2[index] = [pos - pos2m for pos in pos2[index]]

        index = index + 1


# In[3]:


#print(trops)
#print(tropsstd)


# In[4]:


#print(cloks)


# In[5]:


fig, axs = plt.subplots(1,1,figsize=(20, 10))
fig.suptitle("PPP - TROP", fontsize=20)
for i in [0,1]:
    axs.plot(trops[i], label='trop' + str(i))
    axs.plot(tropsstd[i], label='trops' + str(i))
    axs.legend(loc="upper right")

fig, axs = plt.subplots(1,1,figsize=(20, 10))
fig.suptitle("PPP - CLK", fontsize=20)
for i in [0,1]:
    axs.plot(cloks[i], label='clk' + str(i))
    axs.plot(cloksstd[i], label='clk' + str(i))
    axs.legend(loc="upper right")

fig, axs = plt.subplots(1,1,figsize=(20, 10))
fig.suptitle("PPP - POSX", fontsize=20)
for i in [0,1]:
    axs.plot(pos0[i], label='x' + str(i))
    axs.plot(pos0std[i], label='x' + str(i))
    axs.legend(loc="upper right")

fig, axs = plt.subplots(1,1,figsize=(20, 10))
fig.suptitle("PPP - POSY", fontsize=20)
for i in [0,1]:
    axs.plot(pos1[i], label='Y' + str(i))
    axs.plot(pos1std[i], label='Y' + str(i))
    axs.legend(loc="upper right")

fig, axs = plt.subplots(1,1,figsize=(20, 10))
fig.suptitle("PPP - POSZ", fontsize=20)
for i in [0,1]:
    axs.plot(pos2[i], label='Z' + str(i))
    axs.plot(pos2std[i], label='Z' + str(i))
    axs.legend(loc="upper right")

for sat in range(1,33):
    fig2, axs2 = plt.subplots(1,1,figsize=(20, 10))
    fig2.suptitle("PPP - ambsG" + str(sat), fontsize=20)
    axs2.plot(ambsss[sat])
    axs2.plot(ambsss2[sat])
    axs2.plot(ambsssg[sat])
    axs2.plot(ambsssg2[sat])
    axs.legend(loc="upper right")


# In[ ]:




