#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
from helper import *
from itertools import chain
import os
import numpy as np
import pandas as pd

# input data
input_folder = sys.argv[1]
output_folder = sys.argv[2]
series = sys.argv[3:]
index = 0
width = 0.17
xlabel = [1,2,3,4] 
xlabel_minor = ['new-order', 'payment', 'order-status'] 
#colors = ['#EC5B56', '#EC5B56', '#EC5B56']
#colors = ['#7352DE', '#7352DE', '#7352DE']
colors = ['#D3E54E', '#D3E54E', '#D3E54E']
hatches = ['/', '//', 'x', 'xx']
# if it is not specula
ops=['new-order', 'payment', 'order-status']
serie_index=0
xlabels=[]
xminorlabels=[]
series.sort()
max_latency=0
ytitle, new_name = get_title(series)
for serie in series:
    folder = os.path.join(input_folder, serie)
    name = new_name[serie_index].replace('true','t').replace('false','f').replace('0000','0k')
    xminorlabels.extend(ops)
    xlabels.append(name)
    op_index=0
    for op in ops:
        path = os.path.join(folder, op+'-latency')
        print(path)
        if os.path.isfile(path) == False:
            data=[0,0,0,0,0,0,0]
        else:
            data = np.loadtxt(path, skiprows=1, usecols=range(1, 8))
        
        data = [x/1000 for x in data]
        #min_lat = plt.bar(serie_index+op_index*width, data[0], width, color=colors[op_index], hatch=hatches[0])
        med_lat = plt.bar(serie_index+op_index*width, data[2], width, color=colors[op_index])
        #p95_lat = plt.bar(serie_index+op_index*width, (data[3]-data[2]), width, color=colors[op_index], hatch=hatches[1],  bottom=data[2])
        #p99_lat = plt.bar(serie_index+op_index*width, (data[4]-data[3]), width, color=colors[op_index], hatch=hatches[2], bottom=data[3])
        #max_lat = plt.bar(serie_index+op_index*width, (data[6]-data[2]), width, color=colors[op_index], hatch=hatches[2], bottom=data[2])
        max_latency = max(max_latency, data[2])
        op_index += 1
    serie_index += 1


ylim = 1.3*max_latency 
#plt.title('Latency decomposition')
plt.ylabel('Latency')
plt.title('Latency decomposition:'+ytitle, fontsize=11)
plt.ylim([1,ylim])
ax = plt.axes()
plt.xlim([-0.5,len(series)])
ax.set_xticks([x+1.5*width for x in np.arange(len(xlabels))])
ax.set_xticks(list(chain.from_iterable((x-0.5*width, x+width, x+2.5*width) for x in np.arange(len(xlabels)))), minor=True)
ax.set_xticklabels(xminorlabels, minor=True, rotation=20)
ax.set_xticklabels(xlabels, minor=False)
ax.tick_params(axis = 'x', which = 'major', labelsize = 9, pad=30)
ax.tick_params(axis = 'x', which = 'minor', labelsize = 8)
plt.grid(True)
plt.savefig(output_folder+'/'+ytitle+'_op_latency.png')
