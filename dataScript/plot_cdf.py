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
    elif line[0]=='t':
        return ''
    elif line[0]=='A':
        return ''
    elif ',' in line:
        return ''
    else:
        return int(line[:-1])/1000

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
def plot_cdf(specula_folders, nospecula_folders, max_factor, output_folder, output_name, allnodes, nospeculanodes, has_legend, y_ticks, y_label):
    plt.clf()
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
    nsl_index=0
    #colors=['#397fb8', '#ed7e7e', '#944fa1', '#4fb04c', '#000000', '#e31b1b', '#e37f1b']
    colors=['#253494', '#2c7fb8', '#41b6c4', '#a1dab4', '#000000']
    markers=["^", "8", "s", "h", "p", "v", "d", '1', 'd']
    legends=[]
    handlers=[]
    lines_to_plot=len(specula_folders)*2
    postfixes=['percv_latency-', 'final_latency-']
    nospecula_postfix='-latency_final'
        
    sp_lat_list={}
    nsp_lat_list={}
    threads=[]

    for nodei in range(len(allnodes)):
        nodes = allnodes[nodei]
        plt.clf()
        for sub_folders in specula_folders:
            for folder in sub_folders:
                tag = get_tag(0, folder+'/config')
                maxv=0
                for index in range(2):
                    sub_files = [glob.glob(folder+'/'+postfixes[index]+n) for n in nodes]
                    for file_arr in sub_files:
                        file=file_arr[0] 
                        with open(file) as f:
                            for line in f:
                                lat= parse_line(line)
                                if lat == '':
                                    continue
                                else:
                                    sumv+=lat
                                    numpt+=1
                                    #sp_lat_list[sl_index*2+index].append(lat)
                                    if tag not in sp_lat_list:
                                        print("Adding key "+str(tag))
                                        sp_lat_list[tag] = {} 
                                        sp_lat_list[tag][postfixes[0]] = [] 
                                        sp_lat_list[tag][postfixes[1]] = [] 
                                    sp_lat_list[tag][postfixes[index]].append(lat)

        for sub_folders in nospecula_folders:
            for folder in sub_folders:
                maxv=0
                sub_files = [glob.glob(folder+'/'+n+nospecula_postfix) for n in nodes]
                for file_arr in sub_files:
                    file=file_arr[0] 
                    with open(file) as f:
                        for line in f:
                            lat= parse_line(line)
                            if lat == '':
                                continue
                            else:
                                sumv+=lat
                                numpt+=1
                                nsp_lat_list[nsl_index].append(lat)
            nsl_index += 1

        i = 0
        lat_type='percv_latency-'
        key_list = sp_lat_list.keys()
        key_list.sort()
        print(key_list)
        for key in key_list:
            lats = sp_lat_list[key][lat_type]
            stride = max( int(len(lats) / 10), 1)
            hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[i], marker=markers[i], markersize=marksize, markevery=stride, linewidth=width)
            handlers.append(hld)
            i += 1

        i = 0
        lat_type='final_latency-'
        for key in key_list:
            lats = sp_lat_list[key][lat_type]
            stride = max( int(len(lats) / 10), 1)
            hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[i], marker=markers[i], markersize=marksize, markevery=stride, linestyle='--', linewidth=width)
            handlers.append(hld)
            i += 1

        for i, lats in enumerate(nsp_lat_list):
            colori = i+len(sp_lat_list)/2 
            hld, =plt.plot(np.sort(lats), np.linspace(0, 1, len(lats), endpoint=False), color=colors[colori], marker=markers[colori], linewidth=width,  markersize=marksize, markevery=stride)

        #plt.xlim([0, sumv/numpt*max_factor])
        plt.xlim([0, 2000])
        if has_legend:
            legends=['Observed']
            legends.extend(key_list) 
            legends.append('Final') 
            legends.extend(key_list) 

            #even_hlt=[]
            #odd_hlt=[]
            #for i,l in enumerate(handlers[:-1]):
            #    if i%2 == 0:
            #        even_hlt.append(l)
            #    else:
            #        odd_hlt.append(l)
            #handlers = even_hlt + [handlers[-1]]  +odd_hlt 
            extra = Rectangle((0, 0), 0, 0, fc="w", fill=False, edgecolor='none', linewidth=0)
            handlers.insert(0, extra)
            handlers.insert(1+len(key_list), extra)
            lgd = plt.legend(handlers, legends, fontsize=lsize, loc=4, labelspacing=0.0, handletextpad=0.2, borderpad=0.2)
            commit_fonts = lgd.get_texts()
            #t0 = commit_fonts[0]
            #t5 = commit_fonts[5]
            #t0._fontproperties = commit_fonts[1]._fontproperties.copy()
            #t5._fontproperties = commit_fonts[1]._fontproperties.copy()
            #t0.set_weight('bold')
            #t5.set_weight('bold')
        else:
            plt.legend(handlers, legends, loc=4, labelspacing=0.2, borderpad=0.2, fontsize=lsize)
        plt.grid(True)
        if y_ticks == False:
            ax.set_yticks([])

        if y_label == True:
            plt.ylabel('Percentage Transactions', fontsize=labsize)
        plt.xlabel('Latency (ms)', fontsize=labsize)

        if has_legend == True:
            plt.savefig(output_folder+'/'+str(nodei)+ output_name+'.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
        else:
            plt.savefig(output_folder+'/'+str(nodei)+ output_name+'.pdf', format='pdf', bbox_inches='tight')

def get_tag(num, config_file):
    f = open(config_file)
    data = f.read()
    return int(data.split(' ')[0])
