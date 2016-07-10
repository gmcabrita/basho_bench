#!/usr/bin/env python

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
from helper import *
from itertools import chain
import os
import numpy as np
import pandas as pd
import matplotlib as mpl

def parse_line(line):
    if line[0]=='[':
        arr=line.split('[')[1]
        arr=arr.split(',')
        return int(arr[-1][:-2])/1000
    elif line[0]=='a':
        return ''
    else:
        return int(line[:-1])/1000

# input data
def plot_cdf(specula_folders, nospecula_folders, max_factor, output_folder, output_name):
    sumv=0
    numpt=0
    width=4.5
    marksize=12
    fsize=21
    lsize=20
    labsize=20
    mpl.rcParams['xtick.labelsize'] = fsize 
    mpl.rcParams['ytick.labelsize'] = fsize 
    ax = plt.axes()        
    ax.yaxis.grid()
    sl_index=0
    #colors=['#397fb8', '#ed7e7e', '#944fa1', '#4fb04c', '#000000', '#e31b1b', '#e37f1b']
    colors=['#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#000000']
    markers=["^", "8", "s", "h", "p", "v", "d", '1', 'd']
    legends=[]
    handlers=[]
    lines_to_plot=1+len(specula_folders)*2
    #postfixes=['-latency_bench', '-latency_ant']
    postfixes=['-latency_percv', '-latency_final']
    nospecula_postfix='-latency_final'
        
    #legends=['SL1: specula', 'SL1: final', 'SL2: specula', 'SL2: final', 'SL4: specula', 
    #            'SL4: final', 'SL8: specula', 'SL8: final','No specula']
    legends=['16: specula', '16: final', '32: specula', '32: final', '64: specula', 
                '64: final', '96: specula', '96: final', '128: specula', '128: final']
    lat_list=[[] for i in range(lines_to_plot)]
    maxv_list=[0 for i in range(lines_to_plot)]

    for folder in specula_folders:
        maxv=0
        print("Folder is "+folder)
        for index in range(2):
            sub_files = glob.glob(folder+'/*'+postfixes[index])
            print(sub_files)
            for file in sub_files:
                with open(file) as f:
                    for line in f:
                        lat= parse_line(line)
                        if lat == '':
                            continue
                        else:
                            maxv_list[sl_index*2+index]=max(maxv_list[sl_index*2+index], lat)
                            sumv+=lat
                            numpt+=1
                            lat_list[sl_index*2+index].append(lat)
        sl_index += 1

    for folder in nospecula_folders:
            ##Hack here!! Change back!!!!
        sub_files = glob.glob(folder+'/*'+ nospecula_postfix)
        for file in sub_files:
            with open(file) as f:
                for line in f:
                    lat = parse_line(line)
                    if lat == '':
                        continue
                    else:
                        maxv_list[sl_index*2]=max(maxv_list[sl_index*2], lat)
                        sumv+=lat
                        numpt+=1
                        lat_list[sl_index*2].append(lat)

    for i, lats in enumerate(lat_list):
        stride = max( int(len(lats) / 10), 1)
        if i == lines_to_plot -1:
            if lats != []:
                colori = math.ceil(i/2)
                hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[colori], marker=markers[colori], linewidth=width,  markersize=marksize, markevery=stride)
        else:
            if i % 2 == 1:
                hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[i//2], marker=markers[i//2], markersize=marksize, markevery=stride, linestyle='--', linewidth=width)
            else:
                hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[i//2], marker=markers[i//2], markersize=marksize, markevery=stride, linewidth=width)
        handlers.append(hld)

        #plt.xlim([0, sumv/numpt*max_factor])
    plt.xlim([0, 3000])
        #plt.xlim([0, maxv_list[sl_index]])
    plt.legend(handlers, legends, loc=4, labelspacing=0.2, borderpad=0.2, fontsize=lsize)
    plt.grid(True)

    plt.ylabel('Percentage Transactions', fontsize=labsize)
    plt.xlabel('Latency (ms)', fontsize=labsize)

    plt.savefig(output_folder+'/'+ output_name+'.pdf', format='pdf', bbox_inches='tight')


specula_folders1=['./specula_tests/stress/newstress/2016-06-20-220323', './specula_tests/stress/newstress/2016-06-20-221333', './specula_tests/stress/newstress/2016-06-20-222343', 'specula_tests/stress/newstress/2016-06-20-223404/', 'specula_tests/stress/newstress/2016-06-20-224434/']
specula_folders2=['./specula_tests/stress/newstress/2016-06-20-220826', './specula_tests/stress/newstress/2016-06-20-221836/', './specula_tests/stress/newstress/2016-06-20-222848/', './specula_tests/stress/newstress/2016-06-20-223915/', './specula_tests/stress/newstress/2016-06-20-224943/']
plot_cdf(specula_folders1, [], 10, './figures/simple_cdf', 'lowlow')
plt.clf()
plot_cdf(specula_folders2, [], 10, './figures/simple_cdf', 'highlow')
