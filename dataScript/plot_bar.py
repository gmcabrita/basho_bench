#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
from helper import *
import sys
import random
import os
import numpy as np

# input data
def plot_multi_bars(input_folders, output_folder, specula_list, nospecula_list, legends, types, name):
    width = 0.35
    maxv=0
    handlers = []
    legend = list() 
    plt.figure()
    data_l = []
    markers=["^", "8", "s", "h", ".", "1", "v"]
    line_index=0
    #colors=['b','g','r']
    #colors=['c','m','y', 'k', 'r', 'g', 'b']
    colors=['#397fb8', '#ed7e7e', '#944fa1', '#4fb04c', '#000000', '#e31b1b', '#e37f1b']
    #legend_type=plot_dict['legend_type']
    left_max = -0.1
    line_points = 0
    length = len(specula_list)
    for index, input_folder in enumerate(input_folders):
        bar_width=0.8/length
        bar_start=index+1-0.4
        for i in range(length):
            pos=bar_width*i + bar_start
            specula = specula_list[i]
            nospecula = nospecula_list[i]
            sppath = os.path.join(input_folder, specula+'/total_throughput')
            spdata = np.loadtxt(sppath, skiprows=1, usecols=range(1,7))

            nosppath = os.path.join(input_folder, nospecula+'/total_throughput')
            nospdata = np.loadtxt(nosppath, skiprows=1, usecols=range(1,7))
            nospthroughput = nospdata[0,0]
            spthroughput = spdata[0,0]
            plt.bar(pos, spthroughput/nospthroughput, bar_width, color=colors[i])

    plt.legend(handlers, legends, loc=3)
    plt.xticks([x+1 for x in range(len(types))], types, fontsize=10)
    plt.xlim([0, length+1])
    plt.savefig(output_folder+'/'+name+'.pdf', format='pdf', bbox_inches='tight')

