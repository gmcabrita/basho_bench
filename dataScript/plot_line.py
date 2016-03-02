#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
from helper import *
import sys
import random
import os
import numpy as np


# input data
def plot_multi_lines(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    plt.figure()
    width = 0.35
    maxv=0
    handlers = []
    legend = list() 
    data_l = []
    markers=["^", "1", "2", "3"]
    line_index=0
    #colors=['b','g','r']
    colors=['c','m','y', 'k', 'r', 'g', 'b']
    legend_type=plot_dict['legend_type']
    offset_width = 0.2/len(data_multi_list)
    left_max = -0.1
    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
        x=[]
        index = 0
        committed=[]
        committed_e=[]
        aborted=[]
        aborted_e=[]
        #xlabel.append(name)
        minvalue = 1000000000 
        for f in data_list:
            path = os.path.join(input_folder, f+'/total_throughput')
            data = np.loadtxt(path, skiprows=1, usecols=range(1,7))
            newmaxv = max(data[0,0], data[0,5])
            maxv=max(maxv, newmaxv)
            x.append(index)
            committed.append(data[0,0])
            minvalue = min(minvalue, float(data[0,0]))
            committed_e.append(data[1,0])
            aborted.append(data[0,5])
            aborted_e.append(data[1,5])
            index += 1

        l = get_legend(f.split('_')[int(legend_index)], legend_type, '')
        legend.append(l)
        print("Plotting "+str(committed))
        h = plt.errorbar(x, committed, committed_e, color=colors[line_index], marker='^')
        if 'add_num' in plot_dict: 
            for i, y in enumerate(committed):
                xc = x[i]
                txt = str(int(y))+"("+str(int(y/minvalue*10) / 10)+")" 
                plt.annotate(txt, (xc+offset_width*line_index+left_max, y), fontsize=8)
            
        handlers.append(h)
        plt.errorbar(x, aborted, aborted_e, linestyle='--', color=colors[line_index], marker='1')
        #l = get_legend(f.split('_')[int(legend_index)], legend_type, 'aborts')
        #legend.append(l)
        #handlers.append(h)
        data_l = data_list
        line_index += 1

    ylim = maxv * 1.5
    handler_idx = [i for i, x in enumerate(handlers) if x == None]
    labels = ['committed', 'cert_abort', 'read_abort', 'read_invalid', 'cascade_abort']
    handlers = [i for j, i in enumerate(handlers) if j not in handler_idx]
    labels = [i for j, i in enumerate(labels) if j not in handler_idx]

    if plot_dict['y_labels'] != False:
        #plt.set_ylabel(plot_dict['y_labels'])
        plt.ylabel(plot_dict['y_labels'], fontsize=17) 

    if plot_dict['x_labels'] != False:
        plt.xlabel(plot_dict['x_labels'], fontsize=17)
        #plt.xlabel(plot_dict['x_labels'])

    if  'no_title' not in plot_dict:
        plt.title(plot_dict['title'], fontsize=17)


    if plot_dict['y_lim'] == False:
        plt.ylim([1,ylim])
    else:
        plt.ylim([1,plot_dict['y_lim']])
    
    plt.yticks(fontsize=17)
        
    plt.xlim([-0.1,len(data_l)-0.9])
    xlabel=['0','1','2','4','8', '16', '32']
    plt.xticks([x for x in np.arange(len(xlabel))], xlabel, fontsize=17)
    plt.legend(handlers, legend, fontsize=17, loc=2)
    #plt.legend(('local_abort', 'local_commit', 'remote_abort', 'remote_commit', 'remote_specula_abort', 'remote_specula_commit'))
    plt.grid(True)
    plt.tight_layout()
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','')
    plt.savefig(output_folder+'/'+name+legend_type+'.png')

