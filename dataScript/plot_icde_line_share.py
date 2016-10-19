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


def plot_multi_lines(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict, ax1, ax2, ax3):
    xticks_entry = dict() 
    
    fsize=15
    labsize=18
    ylabsize=15
    maxv=0
    max_latency = 0
    handlers = []
    abort_handlers = []
    legend_type = plot_dict['legend_type'] 
    markers=["^", "v", "s", "o", "D", "v"]
    #colors=['#253494', '#2c7fb8', '#41b6c4', '#1111cc', '#1111cc', '#0000cc']
    colors=['#31a354', '#e34a33', '#045a8d', '#a6bddb', '#d0d1e6', '#f6eff7']
    dashed_ls = ['--', '-.', ':']
    line_index=0
    barwidth = 0.6/len(data_multi_list)
    #colors=['#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#ffffcc']
    marker_size=9
    line_width=2
    num_xticks = 0
    start_pos = 0

    for data_list in data_multi_list: 
        for f in data_list:
            threads = int(f.split('_')[0])
            xticks_entry[threads] = 'exist'
    xticks_entry = xticks_entry.keys()
    xticks_entry.sort()
        
    dash_line=0
    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
        print('Sorted to list '+''.join(data_list))
            
        index = 0
        specula_abort=[]
        data1=[]
        data1_e=[]
        data2=[]
        data2_e=[]
        specula_lat=[]
        specula_lat_e=[]
        final_lat=[]
        final_lat_e=[]
        for i in xticks_entry:
            specula_abort.append(0)
            data1.append(0)
            data1_e.append(0)
            data2.append(0)
            data2_e.append(0)
            specula_lat.append(0)
            specula_lat_e.append(0)
            final_lat.append(0)
            final_lat_e.append(0)
            
        minvalue = 1000000000 
        num_xticks = len(xticks_entry)
        offset = len(data_multi_list)/2*barwidth

        for f in data_list:
            print(f)
            num_threads = int(f.split('_')[0])
            t_index = xticks_entry.index(num_threads)

            path = get_path(input_folder, f+'/total_throughput')

            ncols = np.loadtxt(path, dtype='str').shape[1]

            data = np.loadtxt(path, skiprows=1, usecols=range(1,ncols))
            maxv=max(maxv, data[0,0]/1000)
            minvalue = min(minvalue, float(data[0,0]))
            if ncols == 11:
                maxv=max(maxv, data[0,0], data[0,5])
                minvalue = min(minvalue, float(data[0,0]))
                data1[t_index] = data[0,0]
                data1_e[t_index] = data[1,0]
                data2[t_index] = data[0,8]
                data2_e[t_index] = data[1,8]
                specula_abort[t_index] = data[0,9]
            else:
                maxv=max(maxv, data[0,0], data[0,1])
                minvalue = min(minvalue, float(data[0,0]))
                data1[t_index] = data[0,0]
                data1_e[t_index] = data[1,0]
                data2[t_index] = data[0,4]
                data2_e[t_index] = data[1,4]
                specula_abort[t_index] = data[0,5]
            path = get_path(input_folder, f+'/real_latency')
            data = np.loadtxt(path, skiprows=1, usecols=range(1,2))
            specula_lat[t_index] = data[0]
            final_lat[t_index] = data[1]
            specula_lat_e[t_index] = data[2]
            final_lat_e[t_index] = data[3]
            max_latency = max(max_latency, data[1])
            index += 1

        s = [10 for num in data1]
        #for i, v in enumerate(data1):
        #    h, = ax1.bar(i+line_index*barwidth-offset, v/1000,  barwidth, yerr= data1_e[i]/1000, color=colors[line_index])
        data1 = [x/1000 for x in data1]
        data1_e = [x/1000 for x in data1_e]
             
        #print("Plotting "+str(data1))
        data_len = 0
        for i in range(len(data1)):
            if data1[i] == 0:
                break
            else:
                data_len += 1
        print(xticks_entry)
        print(data_len)
        print(data1)
        h = ax1.errorbar(xticks_entry, data1[:data_len], color=colors[line_index], marker=markers[line_index], markersize=marker_size, linewidth=line_width)
        
        handlers.append(h)

        ax2.errorbar(xticks_entry, data2[:data_len], color=colors[line_index], marker=markers[line_index], markersize=marker_size, linewidth=line_width)

        ax3.errorbar(xticks_entry, final_lat[:data_len], color=colors[line_index], marker=markers[line_index], markersize=marker_size, ls='-', linewidth=line_width)
        if specula_lat[0] != 0:
            for i, sv in enumerate(specula_lat):
                if sv > final_lat[i]:
                    specula_lat[i] = final_lat[i]
            #ax3.errorbar([i for i in range(0, data_len)], specula_lat[:data_len], color=colors[line_index], marker=markers[line_index], markersize=marker_size, ls=dashed_ls[dash_line], linewidth=line_width)
            ax3.errorbar(xticks_entry, specula_lat[:data_len], color=colors[line_index], marker=markers[line_index], markersize=marker_size, ls=dashed_ls[dash_line], linewidth=line_width)
            dash_line+=1

        line_index += 1

    ylim = maxv * 1.1
    handler_idx = [i for i, x in enumerate(handlers) if x == None]
    labels = ['data1', 'cert_abort', 'read_abort', 'read_invalid', 'cascade_abort']
    handlers = [i for j, i in enumerate(handlers) if j not in handler_idx]
    labels = [i for j, i in enumerate(labels) if j not in handler_idx]
    if 'big' in plot_dict:
        fsize=21
        lsize=15
        olsize=20
        labs=0.1
    else:
        fsize=17
        lsize=15
        olsize=20
        labs=True

    if  'no_title' not in plot_dict:
        fig.suptitle(plot_dict['title'], fontsize=fsize)

    ax1.set_xlim([0,xticks_entry[-1]])
    ax1.set_xticks([])
    ax2.set_xlim([0,xticks_entry[-1]])
    ax2.set_xticks([])
    ax3.set_xlim([0,xticks_entry[-1]])
    #ax2.set_xlim([-0.2,num_xticks-1+0.2])
    #ax3.set_xlim([-0.2,num_xticks-1+0.2])

    #ax3.set_xticks([i for i in range(num_xticks)])
    #ax3.set_xticklabels(xticks_entry, minor=False, fontsize=fsize)

    if plot_dict['ax1_labels'] != False:
        ax1.set_ylabel(plot_dict['ax1_labels'], fontsize=ylabsize) 
    if plot_dict['ax2_labels'] != False:
        ax2.set_ylabel(plot_dict['ax2_labels'], fontsize=ylabsize) 
    if plot_dict['ax3_labels'] != False:
        ax3.set_ylabel(plot_dict['ax3_labels'], fontsize=ylabsize) 

    if plot_dict['under_labels'] != False:
        ax3.set_xlabel(plot_dict['under_labels'], fontsize=labsize) 

    ax1.set_ylim([0,plot_dict['th_lim']])
    ax2.set_ylim([0.01,0.99])
    ax3.set_ylim([100,plot_dict['lat_lim']])
    ax3.set_yscale('log')
    ax1.yaxis.grid(True)
    ax2.yaxis.grid(True)
    ax3.yaxis.grid(True)
    #mpl.rcParams['ytick.labelsize'] = fsize
    ax1.tick_params(labelsize=fsize)
    ax2.tick_params(labelsize=fsize)
    ax3.tick_params(labelsize=fsize)
    if 'y_ticks' in plot_dict and plot_dict['y_ticks'] == False:
        ax1.yaxis.set_major_formatter(NullFormatter())
        ax2.yaxis.set_major_formatter(NullFormatter())
        ax3.yaxis.set_major_formatter(NullFormatter())
        
    lgd=0

    if 'has_legend' in plot_dict and plot_dict['has_legend']:
        if 'out_legend' in plot_dict and plot_dict['out_legend']:
            commit_legend = plot_dict['commit_legend']
            lgd = ax1.legend(handlers, commit_legend, fontsize=olsize, loc=9, labelspacing=0.1, handletextpad=0.15, borderpad=0.26, bbox_to_anchor=(1,1.35), ncol=len(handlers))
        else:
            pass
            #extra = Rectangle((0, 0), 0, 0, fc="w", fill=False, edgecolor='none', linewidth=0)
            #lgd1 = ax1.legend([extra], ['Commits'], fontsize=lsize, loc=6, labelspacing=0., columnspacing=0.2, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.6,1))
            #lgd1.get_texts()[0].set_weight('bold')

            #lgd2 = ax2.legend([extra], ['Aborts'], fontsize=lsize, loc=6, labelspacing=0., columnspacing=0.2, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.6,1))
            #lgd2.get_texts()[0].set_weight('bold')

            #lgd3 = ax3.legend([extra], ['Latency'], fontsize=lsize, loc=6, labelspacing=0., columnspacing=0.2, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.6,1))
            #lgd3.get_texts()[0].set_weight('bold')
        
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    return lgd
