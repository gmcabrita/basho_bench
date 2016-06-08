#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import matplotlib as mpl
from helper import *
import sys
import random
import os
import numpy as np
import re

def setHatchThickness(value):
    libpath = matplotlib.__path__[0]
    backend_pdf = libpath + "/backends/backend_pdf.py"
    with open(backend_pdf, "r") as r:
        code = r.read()
        code = re.sub(r'self\.output\((\d+\.\d+|\d+)\,\ Op\.setlinewidth\)',
                   "self.output(%s, Op.setlinewidth)" % str(value), code)
        with open('/tmp/hatch.tmp', "w") as w:
            w.write(code)
        #print backend_pdf
        os.system('sudo mv /tmp/hatch.tmp %s' % backend_pdf)

def get_path(input_folders, name):
    if type(input_folders) == type([]) :
        for fl in input_folders:
            path = os.path.join(fl, name)
            if os.access(path, os.R_OK):
                return path
    else:
        return os.path.join(input_folders, name)

def output_label(legends, colors, hatches):
    fig = plt.figure()
    figlegend = plt.figure(figsize=(5,2))
    ax = fig.add_subplot(111)
    handlers=[]
    for i in range(len(colors)):
        hlt, = ax.bar(0, 0, 0, hatch=hatches[i], color=colors[i])
        handlers.append(hlt)
    figlegend.legend(handlers, legends, 'center')
    fig.show()
    figlegend.show()
    figlegend.savefig(output_folder+'/'+'legend.pdf', format='pdf', bbox_inches='tight')

# input data
def plot_multi_bars(input_folder, output_folder, specula_list, legends, types, name, colors, hatches):
    setHatchThickness(2)
    width = 0.35
    maxv=0
    handlers = []
    legend = list() 
    marksize=12
    fsize=21
    lsize=20
    mpl.rcParams['ytick.labelsize'] = fsize
    fig, ax = plt.subplots()
    data_l = []
    line_index=0
    length = len(specula_list[0])
    print("Length is "+str(length))
    handlers=[]
    for index, bar_folders in enumerate(specula_list):
        bar_width=0.9/length
        bar_start=index+1-0.4
        #bar_start = bar_width*index+1-0.4
        bar_folders = sort_by_num(bar_folders)
        nospecula_folder = bar_folders[0]
        bar_folders = bar_folders[1:]
        nosppath = get_path(input_folder, nospecula_folder+'/total_throughput')
        nospdata = np.loadtxt(nosppath, skiprows=1, usecols=range(1,7))
        nospthroughput = nospdata[0,0]
        print(nospecula_folder)
        for i in range(length-1):
            pos= i*bar_width + bar_start
            specula_folder = bar_folders[i]
            #print(specula_folder)
            sppath = get_path(input_folder, specula_folder+'/total_throughput')
            print(sppath)
            print(pos)
            spdata = np.loadtxt(sppath, skiprows=1, usecols=range(1,7))

            #nosppath = os.path.join(input_folder, nospecula+'/total_throughput')
            spthroughput = spdata[0,0]
            print("No spec")
            print(nospthroughput)
            print("Spec")
            print(spthroughput)
            if hatches[i] == '':
                hlt, = plt.bar(pos, spthroughput/nospthroughput, bar_width, color=colors[i])
            else:
                hlt = ax.add_patch(Polygon([[pos,0], [pos,spthroughput/nospthroughput], [pos+bar_width,spthroughput/nospthroughput], [pos+bar_width,0]], hatch=hatches[i], color=colors[i], fill=False))
            if index == 0:
                handlers.append(hlt)

    plt.legend(handlers, legends, loc=2, fontsize=lsize,labelspacing=0.2, columnspacing=0.2, handletextpad=0.2, borderpad=0.2, ncol=3)
    plt.xticks([x+1 for x in range(len(types))], types, fontsize=fsize)
    plt.xlim([0.5, length+0.5])
    plt.ylim([0, 8])
    plt.gca().yaxis.grid(True)
    plt.savefig(output_folder+'/'+name+'.pdf', format='pdf', bbox_inches='tight')

