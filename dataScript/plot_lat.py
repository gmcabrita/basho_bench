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
def plot_latency(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    plt.clf()
    ncols=7
    fig = plt.figure()
    hatches = ['+++', '///', '\\\\\\', '...', '---', 'xxx']
    
    fsize=15
    lsize=20
    labsize=18
    maxv=0
    handlers = []
    legend_type = plot_dict['legend_type'] 
    markers=["^", "8", "s", "h", ".", "1", "v"]
    line_index=0
    barwidth = max(0.15, 0.6/len(data_multi_list))
    #colors=['#000000', '#397fb8', '#4fb04c', '#ed7e7e','#944fa1', '#e31b1b', '#e37f1b']
    #colors=['#000000', '#9E9FDB', '#01c07c', '#8D1010','#944fa1', '#e31b1b', '#e37f1b']
    #colors=['#000000','#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494']
    colors=['#000000', '#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#ffffcc']
    num_xticks = 0
    start_pos = 0
    offset = len(data_multi_list)/2*barwidth
    ## Draw baseline
    #print(data_multi_list)

    if 'not_reverse' not in plot_dict:
        data_multi_list.reverse()
    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
            
        percv_lats=[]
        final_lats=[]
        minvalue = 1000000000 
        for f in data_list:
            path = get_path(input_folder, f+'/real_latency')
            data = np.loadtxt(path, skiprows=1, usecols=range(1,2))
            percv_lat = data[0]
            final_lat = data[1]
            percv_lats.append(percv_lat)
            final_lats.append(final_lat)

        print(percv_lats)
        print(final_lats)
        for i in range(len(percv_lats)):
            h1, = plt.bar(i+line_index*barwidth-offset, percv_lats[i], barwidth, bottom=0, color=colors[0])
            h2, = plt.bar(i+line_index*barwidth-offset, final_lats[i]-percv_lats[i], barwidth, bottom=percv_lats[i], color=colors[1])
            handlers.append(h1)
            handlers.append(h2)
            #h, = plt.bar(i+line_index*barwidth-offset, v/1000, barwidth, color=colors[line_index])

        line_index += 1

    ylim = maxv * 1.5
    handler_idx = [i for i, x in enumerate(handlers) if x == None]
    handlers = [i for j, i in enumerate(handlers) if j not in handler_idx]
    if 'big' in plot_dict:
        fsize=21
        lsize=18
        labs=0.1
    else:
        fsize=17
        lsize=17
        labs=True

    if plot_dict['y_lim'] == False:
        plt.ylim([0,ylim])
    else:
        plt.ylim([0,plot_dict['y_lim']])

    ### For plt2
    #plt.subplot(212)
    #xlabels=['NOSPEC', 'SL1','SL2','SL4','SL8', 'SL16']
    if 'x_labels' in plot_dict:
        xlabels=plot_dict['x_labels']
    else:
        xlabels=['16 cls', '32 cls', '64 cls', '128 cls']
    #plt.xlabel(xlabels, fontsize=fsize)

    if plot_dict['y_labels'] != False:
        plt.ylabel(plot_dict['y_labels'], fontsize=labsize) 

    ax = plt.axes()        
    ax.yaxis.grid()
    #mpl.rcParams['ytick.labelsize'] = fsize
    #plt.tick_params(xlabels, labelsize=fsize)
    plt.xticks([i for i in range(len(xlabels))], xlabels, fontsize=labsize)
        
    if 'has_legend' in plot_dict and plot_dict['has_legend']:
        plt.legend(handlers, plot_dict['commit_legend'], fontsize=lsize, loc=2, labelspacing=0.0, borderpad=0.2)
        if 'legend_loc' in  plot_dict:
            location = plot_dict['legend_loc']
        else:
            location = 2
    
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    legend_type= legend_type.replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    if 'size' in plot_dict:
        (w,h) = plot_dict['size']
        fig.set_size_inches(w, h)
    #fig.set_size_inches(8.5, 6)
    #plt.tight_layout()
    #fig.savefig(output_folder+'/latency'+name+legend_type+'.pdf', format='pdf', bbox_inches='tight')
    fig.savefig(output_folder+'/latency'+name+legend_type+'.png', bbox_inches='tight')

