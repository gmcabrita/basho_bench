#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
from helper import *
import sys
import random
import os
import numpy as np
import matplotlib.gridspec as gridspec

def swap(l, i1, i2):
    tmp = l[i1]
    l[i1] = l[i2]
    l[i2] = tmp

def get_path(input_folders, name):
    if type(input_folders) == type([]) :
        for fl in input_folders:
            path = os.path.join(fl, name)
            if os.access(path, os.R_OK):
                return path
    else:
        return os.path.join(input_folders, name)

# input data
def plot_bars(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    plt.clf()
    ncols=7
    fig = plt.figure()
    ax = plt.axes()
    ax.yaxis.grid(True)
    hatches = ['+++', '///', '\\\\\\', '...', '---', 'xxx']
    #ax1.set_xticklabels([])
    
    fsize=15
    lsize=24
    labsize=18
    maxv=0
    handlers = []
    legend_type = plot_dict['legend_type'] 
    markers=["^", "8", "s", "h", ".", "1", "v"]
    line_index=0
    barwidth = max(0.2, 0.6/len(data_multi_list))
    #colors=['#000000', '#397fb8', '#4fb04c', '#ed7e7e','#944fa1', '#e31b1b', '#e37f1b']
    colors=['#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#ffffcc']
    num_xticks = 0
    start_pos = 0
    ## Draw baseline
    #print(data_multi_list)
    if 'base_line' in plot_dict and plot_dict['base_line']:
        base_line=data_multi_list[0] 
        print(base_line)
        path = get_path(input_folder, base_line+'/total_throughput')
        data = np.loadtxt(path, skiprows=1, usecols=range(1,ncols))
        if len(data) < 8:
            sa = (data[0,5]-data[0,1])/(data[0,0]+data[0,5])
        else:
            sa = data[0,7]/(data[0,0]+data[0,5])
        ar = data[0,5]/(data[0,0]+data[0,5])
        h, = plt.bar(-barwidth/2, data[0,0], barwidth, color=colors[0])
        handlers.append(h)

        xx = -barwidth/2
        
        line_index = 1
        data_multi_list = data_multi_list[1:]
        num_xticks = 1+ len(data_multi_list[0])
        offset = len(data_multi_list)/2*barwidth
        offset -= 1 - barwidth
    else:
        num_xticks = len(data_multi_list[0])
        offset = len(data_multi_list)/2*barwidth

    if 'not_reverse' not in plot_dict:
        data_multi_list.reverse()
    #print(data_multi_list)
    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
            
        index = 0
        data1=[]
        data1_e=[]
        minvalue = 1000000000 
        for f in data_list:
            if 'type' in plot_dict:
                path = get_path(input_folder, f+'/total_duration')
                data = np.loadtxt(path, skiprows=1, usecols=range(1,23))
                if plot_dict['type'] == 'commit':
                    maxv=max(maxv, data[0,18]+data[0,19])
                    minvalue = min(minvalue, float(data[0,0]))
                    data1.append(data[0,18]+data[0,19])
                    data1_e.append(data[1,18]+data[1,19])
                else:
                    maxv=max(maxv, data[0,20]+data[0,21])
                    minvalue = min(minvalue, float(data[0,20]+data[0,21]))
                    data1.append(data[0,20]+data[0,21])
                    data1_e.append(data[1,20]+data[1,21])
            else:
                path = get_path(input_folder, f+'/total_throughput')
                data = np.loadtxt(path, skiprows=1, usecols=range(1,ncols))
                maxv=max(maxv, data[0,0], data[0,5])
                minvalue = min(minvalue, float(data[0,0]))
                data1.append(data[0,0])
                data1_e.append(data[1,0])
            index += 1

        s = [10 for num in data1]
        h = []
        for i, v in enumerate(data1):
            h, = plt.bar(i+line_index*barwidth-offset, v, barwidth, color=colors[line_index])
        handlers.append(h)

        line_index += 1

    ylim = maxv * 1.5
    handler_idx = [i for i, x in enumerate(handlers) if x == None]
    labels = ['data1', 'cert_abort', 'read_abort', 'read_invalid', 'cascade_abort']
    handlers = [i for j, i in enumerate(handlers) if j not in handler_idx]
    labels = [i for j, i in enumerate(labels) if j not in handler_idx]
    if 'big' in plot_dict:
        fsize=21
        lsize=18
        labs=0.1
    else:
        fsize=17
        lsize=17
        labs=True

    #plt.yticks(fontsize=fsize)
    #plt.subplot(211)
    ### For plt1
    #if plot_dict['x_labels'] != False:
    #    ax1.xlabel(plot_dict['x_labels'], fontsize=fsize)

    if  'no_title' not in plot_dict:
        plt.title(plot_dict['title'], fontsize=fsize)

    if plot_dict['y_lim'] == False:
        plt.ylim([0,ylim])
    else:
        plt.ylim([0,plot_dict['y_lim']])

    ### For plt2
    #plt.subplot(212)
    xlabels=['NOSPEC', 'SL1','SL2','SL4','SL8', 'SL16']

    plt.xlim([-0.5,num_xticks-1+0.5])

    length = len(data_multi_list[0])
    if plot_dict['y_labels'] != False:
        plt.ylabel(plot_dict['y_labels'], fontsize=labsize) 

    plt.xticks([i for i in range(length)], xlabels, fontsize=labsize)

    #plt.grid(True)
    #mpl.rcParams['ytick.labelsize'] = fsize
    plt.tick_params(labelsize=fsize)
    if 'y_ticks' in plot_dict and plot_dict['y_ticks'] == False:
        plt.yaxis.set_major_formatter(NullFormatter())
        
    if 'has_legend' in plot_dict and plot_dict['has_legend']:
        plt.legend(handlers, plot_dict['commit_legend'], fontsize=lsize, loc=2, labelspacing=0.0, borderpad=0.2)
        if 'legend_loc' in  plot_dict:
            location = plot_dict['legend_loc']
        else:
            location = 2
        a_legends = plot_dict['abort_legend']
    
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    legend_type= legend_type.replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    #fig = matplotlib.pyplot.gcf()
    fig.set_size_inches(8.5, 6)
    #plt.tight_layout()
    fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_inches='tight')

