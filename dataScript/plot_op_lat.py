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
bench_type = sys.argv[3]
series = sys.argv[4:]
index = 0
width = 0.17
xlabel = [1,2,3,4] 
xlabel_minor = ['no commit', 'no abort', 'p commit', 'p abort', 'order-status'] 
#colors = ['#EC5B56', '#EC5B56', '#EC5B56']
#colors = ['#7352DE', '#7352DE', '#7352DE']
colors = ['#D3E54E', '#D3E54E', '#D3E54E']
hatches = ['/', '//', 'x', 'xx']
# if it is not specula
ops=['no commit', 'no abort', 'p commit', 'p abort', 'order-status']
serie_index=0
xlabels=[]
xminorlabels=[]
max_latency=0
ytitle, new_name = get_title(series, bench_type)
width=0.1
no_commit=None
no_abort=None
p_commit=None
p_abort=None
for serie in series:
    folder = os.path.join(input_folder, serie)
    name = new_name[serie_index].replace('true','t').replace('false','f').replace('0000','0k')
    xminorlabels.extend(ops)
    xlabels.append(name)
    op_index=serie_index
    path = os.path.join(folder, 'total_duration')
    print(path)
    if os.path.isfile(path) == False:
        data=[0,0,0,0,0,0,0]
    else:
        data = np.loadtxt(path, skiprows=1, usecols=range(1, 25))
        
    csa = ['#D3E54E', '#33CCB3', ('#EC5B56', 'xx')]
    (op_index, no_commit)= draw_bar_if_need(plt, op_index, width, [(data[0][0], data[1][0]), (data[0][12], data[1][12]), (data[0][13], data[1][13])], csa, op_index, no_commit)
    (op_index, no_abort)= draw_bar_if_need(plt, op_index, width, [(data[0][0], data[1][0]), (data[0][14], data[1][14]), (data[0][15], data[1][15])], csa, op_index, no_abort)
    (op_index, p_commit)= draw_bar_if_need(plt, op_index, width, [(data[0][6], data[1][6]), (data[0][16], data[1][16]), (data[0][17], data[1][17])], csa, op_index, p_commit)
    (op_index, p_abort)= draw_bar_if_need(plt, op_index, width, [(data[0][6], data[1][6]), (data[0][18], data[1][18]), (data[0][19], data[1][19])], csa, op_index, p_abort)
    #print(data)
    max_latency = max(max_latency, np.amax(data[0]))
    #print("Data is"+str(data[0]))
    #print("**********Max late is "+str(max_latency)+"************")
    serie_index += 1


#print("**********Final max late is "+str(max_latency)+"************")

ylim = 1.3*max_latency 
#plt.title('Latency decomposition')
plt.ylabel('Latency')
plt.title('Latency decomposition:'+ytitle, fontsize=11)
plt.ylim([1,ylim])
ax = plt.axes()
plt.xlim([-0.5,len(series)])
ax.set_xticks([x+1.5*width for x in np.arange(len(xlabels))])
ax.set_xticks(list(chain.from_iterable((x-0.5*width, x+width, x+2.5*width) for x in np.arange(len(xlabels)))), minor=True)
ax.set_xticklabels(xminorlabels, minor=True, rotation=30)
ax.set_xticklabels(xlabels, minor=False)
ax.set_yscale('log')
ax.tick_params(axis = 'x', which = 'major', labelsize = 9, pad=30)
ax.tick_params(axis = 'x', which = 'minor', labelsize = 8)
plt.grid(True)
plt.savefig(output_folder+'/'+ytitle+'_op_latency.png')
