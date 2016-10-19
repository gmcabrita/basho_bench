#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
from helper import *
import sys
import math
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
def plot_three(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    plt.clf()
    fig = plt.figure()
    gs = gridspec.GridSpec(3, 1)
    gs.update(hspace=0.001)
    ax = plt.axes()
    ax.yaxis.grid(True)
    ax1 = plt.subplot(gs[0, 0])
    ax2 = plt.subplot(gs[1, 0]) 
    ax3 = plt.subplot(gs[2, 0]) 


    markers=["^", "v", "s", "o", "D", "v"]
    #colors=['#253494', '#2c7fb8', '#41b6c4', '#1111cc', '#1111cc', '#0000cc']
    colors=['#31a354', '#e34a33', '#045a8d', '#a6bddb', '#d0d1e6', '#f6eff7']
    dashed_ls = ['--', '-.', ':']
    #if 'base_line' in plot_dict:
    #    colors=['#000000', '#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#ffffcc']
    #else:
    #    colors=['#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#ffffcc']
    #markers=["^", "8", "s", "h", ".", "1", "v"]
    #hatches = ['+++', '///', '\\\\\\', '...', '---', 'xxx']
    ax1.set_xticklabels([])
    
    fsize=15
    ticksize=14
    lsize=20
    labsize=18
    maxv=0
    handlers = []
    abort_handlers = []
    latency_handlers = []
    legend_type = plot_dict['legend_type'] 
    line_index=0
    barwidth = max(0.15, 0.6/len(data_multi_list))
    start_pos = 0
    ## Draw baseline
    #print(data_multi_list)
    num_xticks = len(max(data_multi_list, key=len))
    offset = len(data_multi_list)/2*barwidth

    print(data_multi_list)
    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
            
        index = 0
        specula_abort=[]
        data1=[]
        data1_e=[]
        s_abort_e=[]
        data2=[]
        data2_e=[]
        final_lat=[]
        percv_lat=[]
        final_error=[]
        percv_error=[]
        minvalue = 1000000000 
        for f in data_list:
            path = get_path(input_folder, f+'/total_throughput')
            ncols = np.loadtxt(path, dtype='str').shape[1]
            data = np.loadtxt(path, skiprows=1, usecols=range(1,ncols))
            if ncols == 11:
                maxv=max(maxv, data[0,0], data[0,5])
                minvalue = min(minvalue, float(data[0,0]))
                data1.append(data[0,0])
                data1_e.append(data[1,0])
                data2.append(data[0,8])
                data2_e.append(data[1,8])
                specula_abort.append(data[0,9])
                s_abort_e.append(data[1,9])
            else:
                maxv=max(maxv, data[0,0], data[0,1])
                minvalue = min(minvalue, float(data[0,0]))
                data1.append(data[0,0])
                data1_e.append(data[1,0])
                data2.append(data[0,4])
                data2_e.append(data[1,4])
                specula_abort.append(data[0,5])
                s_abort_e.append(data[1,5])
                

            lat_path = get_path(input_folder, f+'/real_latency')
            lat_data = np.loadtxt(lat_path, skiprows=1, usecols=range(1,2))

            percv_lat.append(lat_data[0])
            final_lat.append(lat_data[1])
            if len(lat_data) == 2:
                percv_error.append(0)
                final_error.append(0)
            else:
                percv_error.append(lat_data[2])
                final_error.append(lat_data[3])
            index += 1

        s = [10 for num in data1]
        h = []
        #for i, v in enumerate(data1):
            #h, = ax1.bar(i+line_index*barwidth-offset, v/1000, barwidth, color=colors[line_index], yerr=data1_e[i]/1000)
        x = [i for i in range(len(data1))]
        data1 = [i/1000 for i in data1]
        h = ax1.errorbar(x, data1, color=colors[line_index], yerr=[i/1000 for i in data1_e], marker=markers[line_index], markersize=10, linewidth=3.5)
        handlers.append(h)

        x = [i for i in range(len(final_lat))]
        h1 = ax3.errorbar(x, final_lat, color=colors[line_index], yerr=final_error, marker=markers[line_index], markersize=10, linewidth=3.5)
        h2 = ax3.errorbar(x, percv_lat, color=colors[line_index], yerr=percv_error, marker=markers[line_index], markersize=10, linewidth=3.5, ls='dashed')
        latency_handlers.append(h1)
        if percv_lat and percv_lat[0] != 0:
            latency_handlers.append(h2)

        #if data2 != []:
        #    for i, v in enumerate(data2):
                #h, = ax2.bar(i+line_index*0.2-0.2, v, 0.2, yerr=data2_e[i], color=colors[line_index])
        #        xx = i+line_index*barwidth-offset
        #        sv = specula_abort[i]
                #hlt1 = ax2.add_patch(Polygon([[xx,sv], [xx,v], [xx+barwidth,v], [xx+barwidth, sv]], hatch=hatches[line_index], color=colors[line_index], fill=False))
                #hlt2 = ax2.add_patch(Polygon([[xx,0], [xx,sv], [xx+barwidth,sv], [xx+barwidth, 0]], color=colors[line_index]))
                #hlt1, = ax2.bar(xx, v-sv, barwidth, yerr=data2_e[i], bottom=sv,color=colors[line_index]) 
                #hlt2, = ax2.bar(xx, sv, barwidth, yerr=s_abort_e[i], bottom=0,color=colors[line_index-1]) 
        hlt1 = ax2.errorbar(x, data2, color=colors[line_index], yerr=data2_e, marker=markers[line_index], markersize=10, linewidth=3.5)
        hlt2 = ax2.errorbar(x, specula_abort, color=colors[line_index], yerr=s_abort_e, marker=markers[line_index], markersize=10, linewidth=3.5, ls='dashed')

        #hlt1, = ax2.bar(xx, v-sv, barwidth, yerr=data2_e[i], bottom=sv,color=colors[line_index]) 
        #hlt2, = ax2.bar(xx, sv, barwidth, yerr=s_abort_e[i], bottom=0,color=colors[line_index-1]) 
        abort_handlers.append(hlt1)
        if specula_abort and specula_abort[-1] != 0:
            abort_handlers.append(hlt2)
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

    if  'no_title' not in plot_dict:
        fig.suptitle(plot_dict['title'], fontsize=fsize)

    if plot_dict['y_lim'] == False:
        ax1.set_ylim([0.05,ylim])
    else:
        ax1.set_ylim([0.05,plot_dict['y_lim']])

    ### For plt2
    #plt.subplot(212)
    #xlabels=['NOSPEC', 'SL1','SL2','SL4','SL8', 'SL16']
    if 'x_ticks' in plot_dict:
        xticks=plot_dict['x_labels']
    else:
        xticks=['16 cls', '32 cls', '64 cls', '128 cls']
    ax3.set_xticks([i for i in range(num_xticks)])
    ax3.set_xticklabels(xticks, minor=False, fontsize=labsize)

    ax1.set_xlim([-0.5,num_xticks-1+0.5])
    ax2.set_xlim([-0.5,num_xticks-1+0.5])
    ax3.set_xlim([-0.5,num_xticks-1+0.5])

    if plot_dict['y_labels'] != False:
        ax1.set_ylabel(plot_dict['y_labels'], fontsize=labsize) 
        ax2.set_ylabel('Abort rate', fontsize=labsize) 
        ax3.set_ylabel('Latency (s)', fontsize=labsize) 
    if 'x_label' and plot_dict['x_label'] != False:
        ax3.set_xlabel(plot_dict['x_label'], fontsize=labsize) 

    ax2.set_ylim([0.01,0.99])
    if '3y_lim' in plot_dict:
        ax3.set_ylim([100, plot_dict['3y_lim']])
    else:
        ax3.set_ylim([100, 10])
    ax3.set_yscale('log')

    ax1.yaxis.grid(True)
    ax2.yaxis.grid(True)
    ax3.yaxis.grid(True)
    #mpl.rcParams['ytick.labelsize'] = fsize
    ax1.tick_params(labelsize=ticksize)
    ax2.tick_params(labelsize=ticksize)
    ax3.tick_params(labelsize=ticksize)
    if 'y_ticks' in plot_dict and plot_dict['y_ticks'] == False:
        ax1.yaxis.set_major_formatter(NullFormatter())
        ax2.yaxis.set_major_formatter(NullFormatter())
        ax3.yaxis.set_major_formatter(NullFormatter())
        
    if 'has_legend' in plot_dict and plot_dict['has_legend']:
        extra = Rectangle((0, 0), 0, 0, fc="w", fill=False, edgecolor='none', linewidth=0)
        if 'out_legend' in plot_dict:
            lgd = ax1.legend([extra]+handlers, plot_dict['commit_legend'], fontsize=lsize, loc=6, labelspacing=0.0, borderpad=0.2, bbox_to_anchor=(-0.51, 0.78))
            commit_fonts = lgd.get_texts()
            t0 = commit_fonts[0]
            t0._fontproperties = commit_fonts[1]._fontproperties.copy()
            t0.set_weight('bold')
        else:
            lgd = ax1.legend(handlers, plot_dict['commit_legend'], fontsize=lsize, loc=2, labelspacing=0.0, borderpad=0.2)
            

        if 'legend_loc' in  plot_dict:
            location = plot_dict['legend_loc']
        else:
            location = 2
        a_legends = plot_dict['abort_legend']

        if 'out_legend' in plot_dict:
            lgd2=ax2.legend([extra]+abort_handlers, a_legends, fontsize=lsize, loc=6, labelspacing=0.2, columnspacing=0.2, handletextpad=0.2, borderpad=0.2,  bbox_to_anchor=(-0.51,0.7))
            abort_fonts = lgd2.get_texts()
            t0 = abort_fonts[0]
            t0._fontproperties = abort_fonts[1]._fontproperties.copy()
            t0.set_weight('bold')
        else:
            lgd2=ax2.legend(abort_handlers, a_legends, fontsize=lsize, labelspacing=0.2, loc=3, columnspacing=0.2, handletextpad=0.2, borderpad=0.2)
            

        if 'out_legend' in plot_dict:
            lgd3=ax3.legend([extra]+latency_handlers, plot_dict['latency_legend'], fontsize=lsize, loc=6, labelspacing=0.2, columnspacing=0.2, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.51,0.7))
            abort_fonts = lgd3.get_texts()
            t0 = abort_fonts[0]
            t0._fontproperties = abort_fonts[1]._fontproperties.copy()
            t0.set_weight('bold')
        else:
            lgd3=ax3.legend(latency_handlers, plot_dict['latency_legend'], fontsize=lsize, loc=0, labelspacing=0.2, columnspacing=0.2, handletextpad=0.2, borderpad=0.2)
    
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    legend_type= legend_type.replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    #fig = matplotlib.pyplot.gcf()
    if 'size' in plot_dict:
        (w,h) = plot_dict['size']
        fig.set_size_inches(w, h)
    #fig.set_size_inches(8.5, 6)
    #plt.tight_layout()
    if 'has_legend' in plot_dict and plot_dict['has_legend']:
        fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
    else:
        fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_inches='tight')
    #fig.savefig(output_folder+'/'+name+legend_type+'.png', bbox_inches='tight')

