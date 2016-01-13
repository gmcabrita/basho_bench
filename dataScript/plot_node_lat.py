#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
import os
import numpy as np
import pandas as pd

def draw_bar_if_need(index, width, val_list, color):
    base = 0 
    i = 0
    labels = [] 
    if val_list[-1][0] != 0.0:
        for v in val_list:
            if base == 0:
                if isinstance(color[i], tuple):
                    (col, hat) = color[i]
                    hr = plt.bar(index, v[0], width, color=col, hatch=hat)
                else:
                    hr = plt.bar(index, v[0], width, color=color[i])
                base = v[0]
                labels.append(hr)
            else:
                if isinstance(color[i], tuple):
                    (col, hat) = color[i]
                    hr = plt.bar(index, v[0], width, color=col, hatch=hat, bottom=base, yerr=v[1])
                else:
                    hr = plt.bar(index, v[0], width, color=color[i], bottom=base, yerr=v[1])
                base += v[0]
                labels.append(hr)
            i += 1
        return labels 
    else:
        return labels 

# input data
input_folder = sys.argv[1]
output_folder = sys.argv[2]
plt.figure()
index = 0
width = 0.17
print(input_folder)
config = input_folder.split('/')[-2]
print(config)
path = os.path.join(input_folder, 'latency')
data = np.loadtxt(path, skiprows=1, usecols=range(1, 8))
xlabel = pd.read_csv(path, sep=' ')['ip']
colors = [('#EC5B56', 'xx'), ('#EC5B56', '//'), ('#EC5B56', '**'), '#EC5B56']
# if it is not specula
for row in data:
    min_lat = plt.bar(index, row[0]/1000, width, color='#A4E57B')
    med_lat = plt.bar(index+width, row[2]/1000, width, color='#7EC94C')
    p95_lat = plt.bar(index+width*2, row[3]/1000, width, color='#CEE24D')
    p99_lat = plt.bar(index+width*3, row[4]/1000, width, color='#F4FF32')
    max_lat = plt.bar(index+width*4, row[5]/1000, width, color='#877A30')
    index += 1

max = data.max() 
ylim = max * 1.44/1000
(nrows, whatever) = data.shape

plt.ylabel('Latency (in ms)')
plt.title('Latency per node')
plt.ylim([1,ylim])
plt.xlim([-0.5, nrows])
plt.xticks([x+3*width for x in np.arange(len(xlabel))], xlabel, fontsize=10)
plt.legend((min_lat, med_lat, p95_lat, p99_lat, max_lat), ('Min', 'Meidum', '95%', '99%', 'Max'), fontsize=10)
#plt.legend(('local_abort', 'local_commit', 'remote_abort', 'remote_commit', 'remote_specula_abort', 'remote_specula_commit'))
plt.grid(True)
plt.savefig(output_folder+'/'+config+'_latency.png')
