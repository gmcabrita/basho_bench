#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
from helper import *
import sys
import random
import os
import numpy as np


# input data
input_folder = sys.argv[1]
output_folder = sys.argv[2]
bench_type = sys.argv[3]
data_list = sys.argv[4:]
plt.figure()
ind = 0
width = 0.15
xlabel = list() 
local_abort = 0
local_commit = 0
remote_abort = 0
remote_commit = 0
specula_abort = 0
specula_commit = 0
max_lat=0
ytitle, new_name = get_title(data_list, bench_type)
print("*****************Callig me**************")
for f in data_list:
    path = os.path.join(input_folder, f+'/total_duration')
    data = np.loadtxt(path, skiprows=1, usecols=range(1,9))
    read_lat = data[0,0]
    read_err = data[1,0]
    inter_ind = ind
    name = new_name[ind].replace('true','t').replace('false','f').replace('0000','0k')
    xlabel.append(name)
    cla = ['#D3E54E', '#EC5B56']
    clc = ['#D3E54E', '#79E026']
    cra = ['#D3E54E', ('#EC5B56', '//')]
    crc = ['#D3E54E', ('#79E026', '//')]
    csa = ['#D3E54E', '#33CCB3', ('#EC5B56', 'xx')]
    csc = ['#D3E54E', '#33CCB3', ('#79E026', 'xx')]
    # if it is not specula
    if f.find('false') != -1:
        # has local abort
        (inter_ind, local_abort)= draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,1], data[1,1])], cla, inter_ind, local_abort)
        # has local_commit 
        (inter_ind, local_commit) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,3], data[1,3])], clc, inter_ind, local_abort)
        # has remote abort
        (inter_ind, remote_abort) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,2], data[1,2])], cra, inter_ind, remote_abort)
        # has remote commit 
        (inter_ind, remote_commit) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,4], data[1,4])], crc, inter_ind, remote_commit)
        max_lat = max(max_lat, data[0,0]+data[0,3])
        max_lat = max(max_lat, data[0,0]+data[0,4])
    else:
        # has local abort
        (inter_ind, local_abort) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,1], data[1,1])], cla, inter_ind, local_abort)
        # has local_commit 
        (inter_ind, local_commit) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,3], data[1,3])], clc, inter_ind, local_abort)
        # has remote abort
        (inter_ind, remote_abort) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,2], data[1,2])], cra, inter_ind, remote_abort)
        # has remote commit 
        (inter_ind, remote_commit) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err), (data[0,4], data[1,4])], crc, inter_ind, remote_commit)
        # has remote specula abort
        (inter_ind, specula_abort) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err),(data[0,5],data[1,5]),(data[0,6], data[1,6])], csa, inter_ind, specula_abort)
        # has remote specula commit 
        (inter_ind, specula_commit) = draw_bar_if_need(plt, inter_ind, width, [(read_lat, read_err),(data[0,5],data[1,5]),(data[0,7], data[1,7])], csc, inter_ind, specula_commit)
        max_lat = max(max_lat, data[0,0]+data[0,3])
        max_lat = max(max_lat, data[0,0]+data[0,4])
        max_lat = max(max_lat, data[0,0]+data[0,5]++data[0,7])

    ind += 1

ymax = max_lat*1.3
ylim = ymax
plt.ylabel('Latency')
#plt.title('Latency decomposition')
plt.title('Latency decomposition:'+ytitle, fontsize=11)
plt.ylim([1,ylim])
plt.xlim([-0.5,len(data_list)])
plt.xticks([x+2*width for x in np.arange(len(xlabel))], xlabel, fontsize=7)
#plt.yticks(list(plt.yticks()[0]) + [10])
plt.legend((local_abort, local_commit, remote_abort, remote_commit, specula_abort, specula_commit), ('local_abort', 'local_commit', 'remote_abort', 'remote_commit', 'remote_specula_abort', 'remote_specula_commit'), fontsize=10)
#plt.legend(('local_abort', 'local_commit', 'remote_abort', 'remote_commit', 'remote_specula_abort', 'remote_specula_commit'))
plt.grid(True)
plt.savefig(output_folder+'/'+ytitle+'-latency.png')
