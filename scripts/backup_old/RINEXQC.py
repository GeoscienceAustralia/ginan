#!/usr/bin/env python
# coding: utf-8

# In[1]:



import seaborn as sns
import re
import math
import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict
from random import randint

sns.set()

OBS = defaultdict(lambda: defaultdict(list))


# In[2]:



rinex = '/ubuntu/data/acs/pea/example/EX03/data/ANKR00TUR_R_20191990000_01D_30S_MO.19O'

rinex = open(rinex, 'r')
rinex = rinex.read()

matches = re.findall(r'(\w).{6}((?:\w{3} )+)*\s+SYS \/ # \/ OBS TYPES', rinex)
       
typestrings = {}
for match in matches:
    
    sys = match[0]
    #print(sys)
    #print(match[1])
    codes = re.findall(r'(?:(\w{3})\s)', match[1])
    
    #print(codes)
    typestrings[sys] = {}
    index = 0
    for code in codes:
        typestrings[sys][index] = code
        index = index + 1
    
#print(typestrings)

matches = re.findall(r'(>) (\d+)\s(\d+)\s(\d+)\s(\d+)\s(\d+)\s+(\d+.\d+)|\n(\w{3})((?:\d?\d?\s+[0-9\.]+\.\d{3})+)', rinex)

epoch = 0
for match in matches:
    if match[0] == '>':
        epoch = epoch + 1
        continue
    
    sat = match[7]
    
    #print(sat)
    
    numbers = np.array(re.findall(r'(\d+.\d{3}|\s{15})', match[8]))
    numbers[numbers=='               '] = '0'
    numbers = [float(strngs) for strngs in numbers]
    
    #print(sat, "-",numbers)
    
    index = -1
    for number in numbers:
        index = index + 1
        #print(index)
        type = typestrings[sat[0]][index]
        if number != 0:
            OBS[type][sat].append([epoch, 1])


    


# In[3]:



for attr, values in sorted(OBS.items()):
    #print(attr)
    #print(values)

    fig, axs = plt.subplots(1,1,figsize=(20, 20))
    fig.suptitle("OBS " + attr, fontsize=20)
    
    for subattr, subvalues in sorted(values.items()):
        #print(attr)
        #print(subattr)
        #print(subvalues)
        a = []
        b = []

        for subvalue in subvalues:
            a.append(subvalue[0])
            b.append(subattr)
        
        axs.scatter(a, b, label=subattr, s = 0.1)
        #axs.legend(loc="upper right")


# In[ ]:




