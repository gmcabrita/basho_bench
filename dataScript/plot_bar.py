#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
from helper import *
import sys
import random
import os
import numpy as np


# input data
def plot_multi_bars(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    plt.figure()
    width = 0.35
    maxv=0
    handlers = []
    legend = list() 
    data_l = []
    markers=["^", "8", "s", "h", ".", "1", "v"]
    line_index=0
    #colors=['b','g','r']
    #colors=['c','m','y', 'k', 'r', 'g', 'b']
    colors=['#397fb8', '#ed7e7e', '#944fa1', '#4fb04c', '#000000', '#e31b1b', '#e37f1b']
    legend_type=plot_dict['legend_type']
    offset_width = 0.2/len(data_multi_list)
    left_max = -0.1
    line_points = 0
    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
        if isinstance(legend_index, int):
            l = get_legend(data_list[-1].split('_')[int(legend_index)], legend_type, '')
        else:
            l = legend_index[line_index]
        if 'draw_line' in plot_dict:
            max_list= data_list[-(plot_dict['draw_line']):]
            max_list.append(data_list[0])
            data_list = data_list[0:len(data_list) - plot_dict['draw_line']]
            maxvalue = 0 
            for f in max_list:
                path = os.path.join(input_folder, f+'/total_throughput')
                data = np.loadtxt(path, skiprows=1, usecols=range(1,7))
                maxvalue = max(maxvalue, data[0,0])
            #plt.axhline(y=maxvalue, color=colors[line_index], marker=markers[line_index], linewidth=1.1)    
            plt.plot([-0.09, 2.5, 5.09], [maxvalue, maxvalue, maxvalue], color=colors[line_index], marker=markers[line_index], markersize=7, linewidth=1.5)
            #plt.annotate(l+' '+plot_dict['line_name'], (3.5, max(maxvalue*0.9, maxvalue-50)), fontsize=12)
        x=[]
        index = 0
        data1=[]
        data1_e=[]
        data2=[]
        data2_e=[]
        #xlabel.append(name)
        minvalue = 1000000000 
        line_points = len(data_list)
        for f in data_list:
            if 'type' in plot_dict:
                path = os.path.join(input_folder, f+'/total_duration')
                data = np.loadtxt(path, skiprows=1, usecols=range(1,23))
                if plot_dict['type'] == 'commit':
                    maxv=max(maxv, data[0,18]+data[0,19])
                    x.append(index)
                    minvalue = min(minvalue, float(data[0,0]))
                    data1.append(data[0,18]+data[0,19])
                    data1_e.append(data[1,18]+data[1,19])
                else:
                    maxv=max(maxv, data[0,20]+data[0,21])
                    x.append(index)
                    minvalue = min(minvalue, float(data[0,20]+data[0,21]))
                    data1.append(data[0,20]+data[0,21])
                    data1_e.append(data[1,20]+data[1,21])
            else:
                path = os.path.join(input_folder, f+'/total_throughput')
                data = np.loadtxt(path, skiprows=1, usecols=range(1,7))
                maxv=max(maxv, data[0,0], data[0,5])
                x.append(index)
                minvalue = min(minvalue, float(data[0,0]))
                data1.append(data[0,0])
                data1_e.append(data[1,0])
                data2.append(data[0,5])
                data2_e.append(data[1,5])
            index += 1

        legend.append(l)
        print("Legend is"+l)
        print("Plotting "+str(data1))
        s = [10 for num in data1]
        h = plt.errorbar(x, data1, data1_e, color=colors[line_index], marker=markers[line_index], markersize=10, linewidth=3.5)
        if 'add_num' in plot_dict: 
            for i, y in enumerate(data1):
                xc = x[i]
                txt = str(int(y))+"("+str(int(y/minvalue*10) / 10)+")" 
                plt.annotate(txt, (xc+offset_width*line_index+left_max, y), fontsize=8)
            
        handlers.append(h)
        if data2 != []:
            plt.errorbar(x, data2, data2_e, linestyle='--', color=colors[line_index], marker=markers[line_index], markersize=10, linewidth=3.5)
        #l = get_legend(f.split('_')[int(legend_index)], legend_type, 'aborts')
        #legend.append(l)
        #handlers.append(h)
        data_l = data_list
        line_index += 1

    ylim = maxv * 1.5
    handler_idx = [i for i, x in enumerate(handlers) if x == None]
    labels = ['data1', 'cert_abort', 'read_abort', 'read_invalid', 'cascade_abort']
    handlers = [i for j, i in enumerate(handlers) if j not in handler_idx]
    labels = [i for j, i in enumerate(labels) if j not in handler_idx]
    if 'big' in plot_dict:
        fsize=21
        lsize=18
        labs=0.2
    else:
        fsize=17
        lsize=17
        labs=True

    if plot_dict['y_labels'] != False:
        #plt.set_ylabel(plot_dict['y_labels'])
        plt.ylabel(plot_dict['y_labels'], fontsize=fsize) 

    if plot_dict['x_labels'] != False:
        plt.xlabel(plot_dict['x_labels'], fontsize=fsize)
        #plt.xlabel(plot_dict['x_labels'])

    if  'no_title' not in plot_dict:
        plt.title(plot_dict['title'], fontsize=fsize)


    if plot_dict['y_lim'] == False:
        plt.ylim([1,ylim])
    else:
        plt.ylim([1,plot_dict['y_lim']])

    
    plt.yticks(fontsize=fsize)
    if 'y_axis' in plot_dict:
        plt.axes().yaxis.set_ticklabels([])
        
    plt.xlim([-0.1,len(data_l)-0.9])
    xlabel=['no spec','1','2','4','8', '16']
    xlabel=xlabel[:line_points]
    plt.xticks([x for x in np.arange(len(xlabel))], xlabel, fontsize=fsize)
    #if len(data_list) >= 5:
    #    plt.legend(handlers, legend, fontsize=15, loc=2)
    #else:
    plt.legend(handlers, legend, fontsize=lsize, loc=2, labelspacing=labs)
    #plt.legend(('local_abort', 'local_commit', 'remote_abort', 'remote_commit', 'remote_specula_abort', 'remote_specula_commit'))
    plt.grid(True)
    plt.tight_layout()
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    legend_type= legend_type.replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    fig = matplotlib.pyplot.gcf()
    fig.set_size_inches(8.5, 4.5)
    fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_inches='tight')

