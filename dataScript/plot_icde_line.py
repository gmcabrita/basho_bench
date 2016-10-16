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
def plot_multi_bars(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    plt.clf()
    ncols=7
    fig = plt.figure()
    gs = gridspec.GridSpec(3, 1)
    gs.update(hspace=0.001)
    ax = plt.axes()
    ax.yaxis.grid(True)
    ax1 = plt.subplot(gs[0, 0])
    ax2 = plt.subplot(gs[1, 0]) 
    ax3 = plt.subplot(gs[2, 0]) 
    hatches = ['+++', '///', '\\\\\\', '...', '---', 'xxx']
    xticks_entry = dict() 
    
    fsize=15
    lsize=20
    labsize=18
    maxv=0
    max_latency = 0
    handlers = []
    abort_handlers = []
    legend_type = plot_dict['legend_type'] 
    markers=["^", "8", "s", "h", ".", "1", "v"]
    line_index=0
    barwidth = 0.6/max(len(data_multi_list), 1)
    colors=['#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#eeeecc', '#000000']
    num_xticks = 0
    start_pos = 0

    for data_list in data_multi_list: 
        for f in data_list:
            threads = int(f.split('_')[0])
            xticks_entry[threads] = 'exist'
    xticks_entry = xticks_entry.keys()
    xticks_entry.sort()
        

    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
        #print(data_list)
            
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
        num_xticks = len(data_multi_list[0])
        offset = len(data_multi_list)/2*barwidth

        for f in data_list:
            num_threads = int(f.split('_')[0])
            t_index = xticks_entry.index(num_threads)

            path = get_path(input_folder, f+'/total_throughput')
            data = np.loadtxt(path, skiprows=1, usecols=range(1,ncols))
            maxv=max(maxv, data[0,0]/1000)
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
        h = []
        for i, v in enumerate(data1):
            h, = ax1.bar(i+line_index*barwidth-offset, v/1000,  barwidth, yerr= data1_e[i]/1000, color=colors[line_index])
        handlers.append(h)

        if data2 != []:
            for i, v in enumerate(data2):
                #h, = ax2.bar(i+line_index*0.2-0.2, v, 0.2, yerr=data2_e[i], color=colors[line_index])
                xx = i+line_index*barwidth-offset
                sv = specula_abort[i]
                #hlt1 = ax2.add_patch(Polygon([[xx,sv], [xx,v], [xx+barwidth,v], [xx+barwidth, sv]], hatch=hatches[line_index], color=colors[line_index], fill=False))
                #hlt2 = ax2.add_patch(Polygon([[xx,0], [xx,sv], [xx+barwidth,sv], [xx+barwidth, 0]], color=colors[line_index]))
                hlt2 = ax2.bar(xx, sv, width=barwidth, hatch=hatches[line_index], color=colors[line_index])
                hlt1 = ax2.bar(xx, v-sv, width=barwidth, bottom=sv, yerr=data2_e[i], color=colors[line_index])
        abort_handlers.append(hlt1)
        abort_handlers.append(hlt2)
        if final_lat != []:
            for i, v in enumerate(final_lat):
                #h, = ax2.bar(i+line_index*0.2-0.2, v, 0.2, yerr=data2_e[i], color=colors[line_index])
                xx = i+line_index*barwidth-offset
                sv = specula_lat[i]
                hlt2 = ax3.bar(xx, sv, width=barwidth, yerr=specula_lat_e[i], hatch=hatches[line_index], color=colors[line_index])
                hlt1 = ax3.bar(xx, v-sv, width=barwidth, bottom=sv, yerr=final_lat_e[i], color=colors[line_index])

        line_index += 1

    ylim = maxv * 1.1
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
        fig.suptitle(plot_dict['title'], fontsize=fsize)

    ax1.set_xlim([-0.5,num_xticks-1+0.5])
    ax2.set_xlim([-0.5,num_xticks-1+0.5])
    ax3.set_xlim([-0.5,num_xticks-1+0.5])

    ax3.set_xticks([i for i in range(num_xticks)])
    ax3.set_xticklabels(xticks_entry, minor=False, fontsize=fsize)

    if plot_dict['y_labels'] != False:
        ax1.set_ylabel(plot_dict['y_labels'], fontsize=labsize) 
        ax2.set_ylabel('Abort rate', fontsize=labsize) 
        ax3.set_ylabel('Latency', fontsize=labsize) 

    ax1.set_ylim([0,ylim])
    ax2.set_ylim([0,0.99])
    ax3.set_ylim([0,int(max_latency*1.1)])
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

        
    if 'has_legend' in plot_dict and plot_dict['has_legend']:
        if 'out_legend' in plot_dict:
            extra = Rectangle((0, 0), 0, 0, fc="w", fill=False, edgecolor='none', linewidth=0)
            commit_legend = ['Commits']+plot_dict['commit_legend']
            lgd = ax1.legend([extra]+handlers, commit_legend, fontsize=lsize, loc=6, labelspacing=0.0, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.47,0.7))
            commit_fonts = lgd.get_texts()
            t0 = commit_fonts[0]
            t0._fontproperties = commit_fonts[1]._fontproperties.copy()
            #t1.set_size('medium')
            t0.set_weight('bold')

            even_hlt=[]
            odd_hlt=[]
            for i,l in enumerate(abort_handlers[1:]):
                if i%2 == 0:
                    even_hlt.append(l)
                else:
                    odd_hlt.append(l)
            abort_handlers = [abort_handlers[0]] + even_hlt+odd_hlt
            abort_legend = plot_dict['abort_legend']
            extra = Rectangle((0, 0), 0, 0, fc="w", fill=False, edgecolor='none', linewidth=0)
            abort_legend.insert(0, 'I. Abort')
            abort_handlers.insert(0, extra) 
            abort_legend.insert(5, 'S. Abort')
            abort_handlers.insert(5, extra) 
            lgd2 = ax2.legend(abort_handlers, abort_legend, fontsize=lsize, loc=6, labelspacing=0., columnspacing=0.2, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.47,0.5))
            abort_fonts = lgd2.get_texts()
            t0 = abort_fonts[0]
            t5 = abort_fonts[5]
            t0._fontproperties = abort_fonts[1]._fontproperties.copy()
            t5._fontproperties = abort_fonts[1]._fontproperties.copy()
            #t1.set_size('medium')
            t0.set_weight('bold')
            t5.set_weight('bold')
        else:
            if 'legend_loc' in  plot_dict:
                location = plot_dict['legend_loc']
            else:
                location = 2
    
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    legend_type= legend_type.replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    #fig = matplotlib.pyplot.gcf()
    if 'size' in plot_dict:
        (w,h) = plot_dict['size']
        fig.set_size_inches(w, h)
    #fig.set_size_inches(8.5, 6)
    #plt.tight_layout()
    #if 'out_legend' in plot_dict:
    #    fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
    #else:
    #    fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_inches='tight')
    fig.savefig(output_folder+'/'+name+legend_type+'.png')

def plot_multi_lines(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    plt.clf()
    ncols=7
    fig = plt.figure()
    gs = gridspec.GridSpec(3, 1)
    gs.update(hspace=0.001)
    ax = plt.axes()
    ax.yaxis.grid(True)
    ax1 = plt.subplot(gs[0, 0])
    ax2 = plt.subplot(gs[1, 0]) 
    ax3 = plt.subplot(gs[2, 0]) 
    xticks_entry = dict() 
    
    fsize=15
    labsize=18
    maxv=0
    max_latency = 0
    handlers = []
    abort_handlers = []
    legend_type = plot_dict['legend_type'] 
    markers=["^", "8", "s", "h", ".", "1", "v", "v", "v"]
    colors=['#253494', '#2c7fb8', '#41b6c4', '#0000cc', '#0000cc', '#0000cc', '#a1dab4']
    ls = ['-','-','-',':', '-.', '--', '-']
    line_index=0
    barwidth = 0.6/len(data_multi_list)
    #colors=['#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#ffffcc']
    marker_size=10
    line_width=3
    num_xticks = 0
    start_pos = 0

    for data_list in data_multi_list: 
        for f in data_list:
            threads = int(f.split('_')[0])
            xticks_entry[threads] = 'exist'
    xticks_entry = xticks_entry.keys()
    xticks_entry.sort()
        

    for data_list in data_multi_list: 
        data_list = sort_by_num(data_list)
            
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
            num_threads = int(f.split('_')[0])
            t_index = xticks_entry.index(num_threads)

            path = get_path(input_folder, f+'/total_throughput')
            data = np.loadtxt(path, skiprows=1, usecols=range(1,ncols))
            maxv=max(maxv, data[0,0]/1000)
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
             
        print("Plotting "+str(data1))
        h = ax1.errorbar([i for i in range(0, len(data1))], data1, yerr=data1_e, color=colors[line_index], marker=markers[line_index], ls=ls[line_index], markersize=marker_size, linewidth=line_width)
        
        handlers.append(h)

        ax2.errorbar([i for i in range(0, num_xticks)], data2, yerr=data2_e, color=colors[line_index], marker=markers[line_index], markersize=marker_size, ls=ls[line_index], linewidth=line_width)

        ax3.errorbar([i for i in range(0, num_xticks)], final_lat, yerr=final_lat_e[i], color=colors[line_index], marker=markers[line_index], markersize=13, ls=ls[line_index], linewidth=4)

        line_index += 1

    ylim = maxv * 1.1
    handler_idx = [i for i, x in enumerate(handlers) if x == None]
    labels = ['data1', 'cert_abort', 'read_abort', 'read_invalid', 'cascade_abort']
    handlers = [i for j, i in enumerate(handlers) if j not in handler_idx]
    labels = [i for j, i in enumerate(labels) if j not in handler_idx]
    if 'big' in plot_dict:
        fsize=21
        lsize=12
        labs=0.1
    else:
        fsize=17
        lsize=12
        labs=True

    if  'no_title' not in plot_dict:
        fig.suptitle(plot_dict['title'], fontsize=fsize)

    ax1.set_xlim([-0.2,num_xticks-1+0.2])
    ax1.set_xticks([])
    ax2.set_xlim([-0.2,num_xticks-1+0.2])
    ax.set_xticks([])
    ax3.set_xlim([-0.2,num_xticks-1+0.2])

    ax3.set_xticks([i for i in range(num_xticks)])
    ax3.set_xticklabels(xticks_entry, minor=False, fontsize=fsize)

    if plot_dict['y_labels'] != False:
        ax1.set_ylabel(plot_dict['y_labels'], fontsize=labsize) 
        ax2.set_ylabel('Abort rate', fontsize=labsize) 
        ax3.set_ylabel('Latency', fontsize=labsize) 

    ax1.set_ylim([0,ylim])
    ax2.set_ylim([0,0.99])
    ax3.set_ylim([0,int(max_latency*1.1)])
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

    #ax1.legend(handlers, plot_dict['commit_legend'], fontsize=lsize, loc=2, labelspacing=0.0, handletextpad=0.2, borderpad=0.2)
    ax3.legend(handlers, plot_dict['commit_legend'], fontsize=lsize, loc=2, labelspacing=0.0, handletextpad=0.2, borderpad=0.2)
        
    if 'has_legend' in plot_dict and plot_dict['has_legend']:
        if 'out_legend' in plot_dict:
            extra = Rectangle((0, 0), 0, 0, fc="w", fill=False, edgecolor='none', linewidth=0)
            commit_legend = ['Commits']+plot_dict['commit_legend']
            lgd = ax1.legend([extra]+handlers, commit_legend, fontsize=lsize, loc=6, labelspacing=0.0, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.47,0.7))
            commit_fonts = lgd.get_texts()
            t0 = commit_fonts[0]
            t0._fontproperties = commit_fonts[1]._fontproperties.copy()
            #t1.set_size('medium')
            t0.set_weight('bold')

            even_hlt=[]
            odd_hlt=[]
            for i,l in enumerate(abort_handlers[1:]):
                if i%2 == 0:
                    even_hlt.append(l)
                else:
                    odd_hlt.append(l)
            abort_handlers = [abort_handlers[0]] + even_hlt+odd_hlt
            abort_legend = plot_dict['abort_legend']
            extra = Rectangle((0, 0), 0, 0, fc="w", fill=False, edgecolor='none', linewidth=0)
            abort_legend.insert(0, 'I. Abort')
            abort_handlers.insert(0, extra) 
            abort_legend.insert(5, 'S. Abort')
            abort_handlers.insert(5, extra) 
            lgd2 = ax2.legend(abort_handlers, abort_legend, fontsize=lsize, loc=6, labelspacing=0., columnspacing=0.2, handletextpad=0.2, borderpad=0.2, bbox_to_anchor=(-0.47,0.5))
            abort_fonts = lgd2.get_texts()
            t0 = abort_fonts[0]
            t5 = abort_fonts[5]
            t0._fontproperties = abort_fonts[1]._fontproperties.copy()
            t5._fontproperties = abort_fonts[1]._fontproperties.copy()
            #t1.set_size('medium')
            t0.set_weight('bold')
            t5.set_weight('bold')
        else:
            if 'legend_loc' in  plot_dict:
                location = plot_dict['legend_loc']
            else:
                location = 2
    
    name=plot_dict['title'].replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    legend_type = legend_type.replace(' ','').replace('%','').replace(',','').replace('_','').replace('-','').replace(':','')
    #fig = matplotlib.pyplot.gcf()
    if 'size' in plot_dict:
        (w,h) = plot_dict['size']
        fig.set_size_inches(w, h)
    #fig.set_size_inches(8.5, 6)
    #plt.tight_layout()
    #if 'out_legend' in plot_dict:
    #    fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
    #else:
    #    fig.savefig(output_folder+'/'+name+legend_type+'.pdf', format='pdf', bbox_inches='tight')
    fig.savefig(output_folder+'/'+name+legend_type+'.png')

