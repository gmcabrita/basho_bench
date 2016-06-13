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

def get_throughput(folder):
    stat_file = os.path.join(folder, 'total_throughput')
    data = np.loadtxt(stat_file, skiprows=1, usecols=range(1,7))
    return data[0,0]

def load_nodes(path): 
    allnodes=[]
    nodes=[]
    with open(path) as f:
        for i, line in enumerate(f):
            if i % 27 == 26:
                nodes.append(line[:-1])
                allnodes.append(nodes[:])
                nodes = []
            else:
                nodes.append(line[:-1])
    return allnodes

# input data
def plot_stress(specula_folders, nospecula_folders, input_folder, output_folder, output_name):
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
    #postfixes=['-latency_percv', '-latency_final']
    #nospecula_postfix='-latency_final'
        
    legends=['SL2: specula', 'SL2: final', 'SL4: specula', 
                'SL4: final', 'SL8: specula', 'SL8: final','No specula']

    plt.clf()
    spec_list=[[[] for i in specula_folders] for j in range(2)] 
    spec_th_list=[[[] for i in specula_folders] for j in range(2)] 
    nospec_list=[[] for i in specula_folders]
    nospec_th_list=[[] for i in specula_folders]
    print(specula_folders)
    for j, folder in enumerate(specula_folders):
        maxv=0
        #### Get spec latency
        folder = input_folder + folder
        file = folder + '/real_latency'
        spec_th_list[0][j] = get_throughput(folder)
        with open(file) as f:
            linenum = 0
            for line in f:
                if linenum == 1:
                    spec_list[0][j] = line.split(' ')[1][:-1]
                elif linenum == 2:
                    spec_list[1][j] = line.split(' ')[1][:-1]
                linenum += 1

    print(nospecula_folders)
    for i, folder in enumerate(nospecula_folders):
        #### No spec latency
        folder = os.path.join(input_folder, folder)
        file = os.path.join(folder, '/real_latency')
        print("File is "+ file)
        spec_th_list[i] = get_throughput(folder)
        with open(file) as f:
            linnum = 0
            for line in f:
                if linenum == 1:
                    nospec_list[0][j] = line.split(' ')[1][:-1]
                elif linenum == 2:
                    nospec_list[1][j] = line.split(' ')[1][:-1]
            linenum += 1

    hld, = plt.plot(spec_th_list[0], spec_list[0], color=colors[0], marker=markers[0], linewidth=width,  markersize=marksize)
    handlers.append(hld)
    hld, = plt.plot(spec_th_list[0], spec_list[1], color=colors[1], marker=markers[1], linewidth=width,  markersize=marksize)
    handlers.append(hld)
    if nospecula_folders != []:
        hld, = plt.plot(nospec_th_list, nospec_list, color=colors[2], marker=markers[2], linewidth=width,  markersize=marksize)
        handlers.append(hld)

    plt.xlim([0, 5000])
    plt.legend(handlers, legends, loc=4, labelspacing=0.2, borderpad=0.2, fontsize=lsize)
    plt.grid(True)

    plt.ylabel('Latency', fontsize=labsize)
    plt.xlabel('Throughput (txs/s)', fontsize=labsize)

    plt.savefig(output_folder+'/'+output_name+'.pdf', format='pdf', bbox_inches='tight')
