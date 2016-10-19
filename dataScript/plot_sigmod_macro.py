#!/usr/bin/env python

import matplotlib.pyplot as plt
from pylab import *
import sys
from copy import deepcopy
import random
#from plot_stress import *
#from plot_speedup_abort import *
from plot_th_abort_lat import *
from itertools import chain
import os
import numpy as np
import pandas as pd
import re
from plot_icde_line_share import *
from datetime import datetime

def list_folders(path):
    files=glob.glob(path+"/*")
    specula = []
    for f in  files[:-1]:
        specula.append([f])
    nospecula = [files[-1]]
    return specula, nospecula

def get_field(l, num):
    ll = l.split('_')
    return ll[num]

def get_lists(root_folder, config_str):
    folders = glob.glob(root_folder)
    pattern = re.compile(config_str)
    config_list = []
    for f in folders:
        config_file = os.path.join(f, "config")
        with open(config_file) as fl:
            for line in fl:
                if re.match(pattern, line): 
                    config_list.append((line, f)) 

    return config_list

#s_ss1 = sort_by_num([val for sublist in ss1 for val in sublist])
#s_ns1 = sort_by_num([val for sublist in ns1 for val in sublist])
#plot_stress(s_ss1, s_ns1, input_folder, './figures/macro_stress/', '80,10,10')

#input_folder='./stat/2016-07-20-210337/'
#input_folder='./stat/2016-07-22-024749/'
#input_folder='./stat/2016-07-22-120825/'
#input_folder='./stat/2016-07-22-120825new/'
fig = plt.figure()
ax1 = plt.subplot2grid((3,3), (0,0))
ax2 = plt.subplot2grid((3,3), (1,0))
ax3 = plt.subplot2grid((3,3), (2,0))
ax1.yaxis.labelpad = 22
ax2.yaxis.labelpad = 11
input_folder='./stat/2016-10-18-021503/'
time=datetime.now().strftime("%Y%m%d-%H:%M:%S")
output_folder='./figures/eurosys/macro_test/' + time
os.mkdir(output_folder)
bench_type='tpcc'
dict1={'y_labels':'K txs/s', 'x_ticks':['300 cls', '600 cls', '900 cls', '1200 cls'], 'y_lim':5, 'legend_type':'warehouse', 'commit_legend':['Committed ', 'Baseline', 'Planet', 'STR'], 'ax1_labels':'Thousand txs/sec', 'ax2_labels':'Abort rate', 'ax3_labels':'Latency(ms) in log', 'abort_legend':['Abort rate  ', 'Baseline', 'STR: i. abort', 'STR: s. abort'], 'latency_legend':['Latency', 'Baseline', 'STR: observed', 'STR: final'], 'has_legend':True, 'no_title':True, 'out_legend':True, 'x_label': 'Client number', 'th_lim':5, 'lat_lim':100000}
dict1['x_labels']=['300 cls', '600 cls', '900 cls', '1200 cls', '1500 cls']
dict2=deepcopy(dict1)

dict1['title']='tpcc5,83'
dict1['y_lim']=3.2
dict1['3y_lim']=100000
dict1['x_labels']=['5K', '11K', '16K', '21K', '27K']
dict1['under_labels']='(a) 5%, 83%'
ns1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'false', 0, 5, 83, 2])
planet1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 1, 5, 83, 2])
ss1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 0, 5, 83, 2])
print(ss1)
print(ns1)
#ss1 = sort_by_num([val for sublist in ss1 for val in sublist])
#ns1 = sort_by_num([val for sublist in ns1 for val in sublist])
#planet1 = sort_by_num([val for sublist in planet1 for val in sublist])
#plot_three(input_folder, output_folder, bench_type, [ns1, planet1, ss1], 4, dict1)
plot_multi_lines(input_folder, output_folder, bench_type, ns1+planet1+ss1, 4, dict1, ax1, ax2, ax3)

dict1['title']='tpcc45,43'
dict1['x_labels']=['16K', '32K', '48K', '64K', '80K']
dict1['y_lim']=3.2
#dict1['3y_lim']=53
dict1['y_ticks']=False
dict1['has_legend']=False
dict1['y_labels']=False
dict1['under_labels']='(b) 45%, 43%'
dict1['ax1_labels']=False
dict1['ax2_labels']=False
dict1['ax3_labels']=False
ax1 = plt.subplot2grid((3,3), (0,1))
ax2 = plt.subplot2grid((3,3), (1,1))
ax3 = plt.subplot2grid((3,3), (2,1))
#ss1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 8, 45, 43, 2])
planet2=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 1, 45, 43, 2])
ss2=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 0, 45, 43, 2])
ns2=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'false', 0, 45, 43, 2])
#ss2 = sort_by_num([val for sublist in ss2 for val in sublist])
#ns2 = sort_by_num([val for sublist in ns2 for val in sublist])
#planet2 = sort_by_num([val for sublist in planet2 for val in sublist])
#plot_three(input_folder, output_folder, bench_type, [ns2, planet2, ss2], 4, dict1)
lgd=plot_multi_lines(input_folder, output_folder, bench_type, ns2+planet2+ss2, 4, dict1, ax1, ax2, ax3)

dict1['title']='tpcc5,43'
dict1['y_lim']=3.2
dict1['x_labels']=['8K', '16K', '24K', '32K', '40K']
dict1['under_labels']='(c) 5%, 43%'
ax1 = plt.subplot2grid((3,3), (0,2))
ax2 = plt.subplot2grid((3,3), (1,2))
ax3 = plt.subplot2grid((3,3), (2,2))
#dict1['3y_lim']=53
#ss1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 8, 5, 43, 2])
planet3=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 1, 5, 43, 2])
ss3=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 0, 5, 43, 2])
ns3=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'false', 0, 5, 43, 2])
#ss3 = sort_by_num([val for sublist in ss3 for val in sublist])
#ns3 = sort_by_num([val for sublist in ns3 for val in sublist])
#planet3 = sort_by_num([val for sublist in planet3 for val in sublist])
#plot_three(input_folder, output_folder, bench_type, [ns3, planet3, ss3], 4, dict1)
plot_multi_lines(input_folder, output_folder, bench_type, ns3+planet3+ss3, 4, dict1, ax1, ax2, ax3)

fig.set_size_inches(16, 7)
#fig.subplots_adjust(hspace = -1)
plt.tight_layout(pad=2.8, w_pad=1, h_pad=-0.7)
#plt.tight_layout()
#fig.savefig(output_folder+'/micro.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
fig.savefig(output_folder+'/tpcc.pdf', format='pdf', bbox_extra_artists=(lgd,))


########################### Something else

dict2['x_labels']=['1K', '2K', '3K', '4K', '5K']
dict2['commit_legend']= ['Baseline', 'SL0']
#dict2['abort_legend'] = ['Baseline', 'SL0: i. abort', 'SL0: s. abort'] 
#dict2['latency_legend'] = ['Baseline', 'SL0: observed', 'SL0: final']
dict2['title']='rubislatency'
dict2.pop('out_legend', None)
dict2['y_lim']=30
dict2['3y_lim']=100000
input_folder=input_folder+'rubis'
ss4=get_matching_series([input_folder, 'rubis', 3, 5, 'true', 0, 2])
ns4=get_matching_series([input_folder, 'rubis', 3, 5, 'false', 0, 2])
planet4=get_matching_series([input_folder, 'rubis', 3, 5, 'true', 1, 2])
print(ss4)
print(ns4)
print(planet4)

ss4 = sort_by_num([val for sublist in ss4 for val in sublist])
ns4 = sort_by_num([val for sublist in ns4 for val in sublist])
planet4 = sort_by_num([val for sublist in planet4 for val in sublist])
plot_three(input_folder, output_folder, 'rubis', [ns4, planet4, ss4], 4, dict2)
