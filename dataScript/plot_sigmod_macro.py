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

input_folder='./stat/2016-07-19-155011/'
output_folder='./figures/sigmod/'
bench_type='tpcc'
dict1={'y_labels':'K txs/s', 'x_labels':['100 cls', '500 cls', '1000 cls', '2500 cls', '3000 cls', '4000 cls'], 'y_lim':5, 'legend_type':'warehouse', 'commit_legend':['Default think'], 'abort_legend':[], 'has_legend':True}
dict2=deepcopy(dict1)
dict2['x_labels']=['300 cls', '600 cls', '900 cls', '1200 cls', '1500 cls']
dict2['y_labels']='Latency (ms)',
dict2['title']='rubislatency'
dict2['y_lim']=5000

dict1['title']='tpcc5,83'
dict1['y_lim']=0.2
ss1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 8, 5, 83, 2])
ss2=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 0, 5, 83, 2])
ns1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'false', 0, 5, 83, 2])
s_ss1 = sort_by_num([val for sublist in ss1 for val in sublist])
s_ss2 = sort_by_num([val for sublist in ss2 for val in sublist])
s_ns1 = sort_by_num([val for sublist in ns1 for val in sublist])
print(s_ss1)
print(s_ss2)
print(s_ns1)
plot_three(input_folder, output_folder, bench_type, [s_ss1, s_ss2], 4, dict1)

dict1['title']='tpcc45,43'
dict1['y_lim']=0.2
ss1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 8, 45, 43, 2])
ss2=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 0, 45, 43, 2])
ns1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'false', 0, 45, 43, 2])
s_ss1 = sort_by_num([val for sublist in ss1 for val in sublist])
s_ss2 = sort_by_num([val for sublist in ss2 for val in sublist])
s_ns1 = sort_by_num([val for sublist in ns1 for val in sublist])
plot_three(input_folder, output_folder, bench_type, [s_ss1, s_ss2], 4, dict1)


dict1['title']='tpcc5,43'
dict1['y_lim']=0.4
ss1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 8, 5, 43, 2])
ss2=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'true', 0, 5, 43, 2])
ns1=get_matching_series([input_folder, 'tpcc', 3, 5, 7, 8, 'false', 0, 5, 43, 2])
s_ss1 = sort_by_num([val for sublist in ss1 for val in sublist])
s_ss2 = sort_by_num([val for sublist in ss2 for val in sublist])
s_ns1 = sort_by_num([val for sublist in ns1 for val in sublist])
plot_three(input_folder, output_folder, bench_type, [s_ss1, s_ss2], 4, dict1)
