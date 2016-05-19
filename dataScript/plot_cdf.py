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
def plot_cdf(specula_folders, nospecula_folders, max_factor, output_folder, output_name):
    sumv=0
    numpt=0
    sl_index=0
    colors=['#397fb8', '#ed7e7e', '#944fa1', '#4fb04c', '#000000', '#e31b1b', '#e37f1b']
    markers=["^", "8", "s", "h", ".", "1", "v", 'p', 'd']
    legends=[]
    handlers=[]
    lines_to_plot=1+len(specula_folders)*2
    postfixes=['-latency_bench', '-latency_ant']
    nospecula_postfix='-latency_bench'
    nodes=['52.50.96.232', '52.51.100.143', '52.50.93.10']
    #nodes=['52.58.192.183', '52.58.171.238', '52.58.1.28']
    #nodes=['54.153.110.236', '52.53.239.93', '54.153.27.71']
    #nodes=['54.169.174.170', '54.169.206.147', '54.169.170.71']
    #nodes=['54.175.90.88', '54.174.146.21', '54.173.41.4']
    #nodes=['52.68.242.101', '52.196.64.46', '52.192.10.101']
    #nodes=['52.68.242.101']
    #nodes=['52.37.133.145','52.39.150.38','52.38.124.81']
    #nodes=['52.63.122.187', '52.63.109.133', '52.64.57.168']
    legends=['S1: perceived', 'S1: real', 'S2: perceived', 'S2: real', 'S4: perceived', 
                'S4: real', 'S8: perceived', 'S8: real','Nospecula']
    lat_list=[[] for i in range(lines_to_plot)]
    maxv_list=[0 for i in range(lines_to_plot)]
    plt.clf()

    for sub_folders in specula_folders:
        for folder in sub_folders:
            maxv=0
           #%print("Folder is "+str(folder))
            for index in range(2):
                sub_files = [glob.glob(folder+'/'+n+postfixes[index]) for n in nodes]
                for file_arr in sub_files:
                    file=file_arr[0] 
                    with open(file) as f:
                        for line in f:
                            lat= int(line[:-1])
                            maxv_list[sl_index*2+index]=max(maxv_list[sl_index*2+index], lat)
                            sumv+=lat
                            numpt+=1
                            lat_list[sl_index*2+index].append(lat)
                print(maxv_list[index])
        sl_index += 1

    print(nospecula_folders)
    for folder in nospecula_folders:
        sub_files = [glob.glob(folder+'/'+n+ nospecula_postfix) for n in nodes]
        for file_arr in sub_files:
            file=file_arr[0] 
            with open(file) as f:
                for line in f:
                    lat= int(line[:-1])
                    maxv_list[sl_index*2]=max(maxv_list[sl_index*2], lat)
                    sumv+=lat
                    numpt+=1
                    lat_list[sl_index*2].append(lat)

    width=2
    for i, lats in enumerate(lat_list):
        #print(len(lats))
        stride = max( int(len(lats) / 10), 1)
        if i == lines_to_plot -1:
            colori = math.ceil(i/2)
            hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[colori], linewidth=width)
        else:
            if i % 2 == 1:
                #print("Here!!!!")
                hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[i//2], marker=markers[i//2], markersize=7, markevery=stride, linestyle='--', linewidth=width)
            else:
                hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[i//2], marker=markers[i//2], markersize=7, markevery=stride, linewidth=width)
        handlers.append(hld)

    plt.xlim([0, sumv/numpt*max_factor])
    plt.legend(handlers, legends, loc=4)

    plt.savefig(output_folder+'/'+output_name+'.pdf', format='pdf', bbox_inches='tight')
