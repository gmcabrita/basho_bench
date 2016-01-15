#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
import os
import numpy as np

def draw_bar_if_need(index, width, val_list, color, handler):
    base = 0 
    i = 0
    for v in val_list:
        if v[0] != 0.0:
            if base == 0:
                if isinstance(color[i], tuple):
                    (col, hat) = color[i]
                    hr = plt.bar(index, v[0], width, color=col, hatch=hat)
                else:
                    hr = plt.bar(index, v[0], width, color=color[i])
                base = v[0]
                handler[i+1] = hr
            else:
                if isinstance(color[i], tuple):
                    (col, hat) = color[i]
                    hr = plt.bar(index, v[0], width, color=col, hatch=hat, bottom=base, yerr=v[1])
                else:
                    hr = plt.bar(index, v[0], width, color=color[i], bottom=base, yerr=v[1])
                base += v[0]
                handlers[i+1] = hr
            i += 1
        else:
            i += 1
    return handler 

# input data
input_folder = sys.argv[1]
output_folder = sys.argv[2]
data_list = sys.argv[3:]
plt.figure()
index = 0
width = 0.35
xlabel = list() 
local_abort = 0
local_commit = 0
remote_abort = 0
remote_commit = 0
specula_abort = 0
specula_commit = 0
print(data_list)
handlers = [None, None, None, None]
for f in data_list:
    path = os.path.join(input_folder, f+'/total_throughput')
    data = np.loadtxt(path, skiprows=1, usecols=range(1,5))
    f = f.replace('true','t').replace('false','f').replace('0000','0k')
    xlabel.append(f[:14])
    colors = ['#EC5B56', ('#EC5B56', 'xx'), ('#EC5B56', '//')]
    # if it is not specula
    h = plt.bar(index, data[0,0], width, yerr=data[1,0], color='#79E026')
    handlers[0] = h
    handlers = draw_bar_if_need(index+width, width, [(data[0,1], data[1,1]), (data[0,2], data[1,2]), (data[0,3], data[1,3])], colors, handlers)
    index += 1

max = data.max()
ylim = max * 1.4
handler_idx = [i for i, x in enumerate(handlers) if x == None]
labels = ['committed', 'cert_abort', 'read_abort', 'cascade_abort']
handlers = [i for j, i in enumerate(handlers) if j not in handler_idx]
labels = [i for j, i in enumerate(labels) if j not in handler_idx]
plt.ylabel('Throughput')
plt.title('Throughput')
plt.ylim([1,ylim])
plt.xlim([-0.5,len(data_list)])
plt.xticks([x+2*width for x in np.arange(len(xlabel))], xlabel, fontsize=7)
plt.legend(handlers, labels, fontsize=10)
#plt.legend(('local_abort', 'local_commit', 'remote_abort', 'remote_commit', 'remote_specula_abort', 'remote_specula_commit'))
plt.grid(True)
plt.savefig(output_folder+'/bar_throughput.png')
