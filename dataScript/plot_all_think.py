#!/usr/bin/env python

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
#from plot_stress import *
from plot_speedup_abort import *
from plot_line_abort import *
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

input_folder='./stat/2016-06-19-153051/tpcc/'
output_folder='./figures/think/'
bench_type='tpcc'
dict1={'y_labels':'K txs/s', 'x_labels':False, 'y_lim':1, 'legend_type':'warehouse', 'commit_legend':['T0', 'T250', 'T500', 'T1000', 'T2000'], 'abort_legend':[], 'has_legend':True}

dict1['title']='10% n, 80% p baseline'
dict1['y_lim']=0.2
base1=get_matching_series([input_folder, 'tpcc', 5, 7, 8, 10, 4])
plot_multi_lines(input_folder, output_folder, bench_type, base1, 4, dict1)

dict1['y_lim']=25
dict1['title']='10% n, 80% p, SL1'
dict1['y_labels']='speedup'
series1=get_matching_series([input_folder, 'tpcc', 5, 7, 1, 10, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series1, base1, 4, dict1)

dict1['title']='10% n, 80% p, SL2'
series2=get_matching_series([input_folder, 'tpcc', 5, 7, 2, 10, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series2, base1, 4, dict1)

#### 1% new-order, 9% payment
dict1['title']='1% n, 9% p baseline'
dict1['y_lim']=3
dict1['y_labels']='K txs/s'
base2=get_matching_series([input_folder, 'tpcc', 5, 7, 8, 1, 4])
plot_multi_lines(input_folder, output_folder, bench_type, base2, 4, dict1)

dict1['title']='1% n, 9% p, SL1'
dict1['y_lim']=10
dict1['y_labels']='speedup'
series3=get_matching_series([input_folder, 'tpcc', 5, 7, 1, 1, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series3, base2, 4, dict1)

dict1['title']='1% n, 9% p, SL2'
series4=get_matching_series([input_folder, 'tpcc', 5, 7, 2, 1, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series4, base2, 4, dict1)

#### 9% new-order, 1% payment
dict1['title']='9% n, 1% p baseline'
dict1['y_lim']=3
dict1['y_labels']='K txs/s'
base3=get_matching_series([input_folder, 'tpcc', 5, 7, 8, 9, 4])
plot_multi_lines(input_folder, output_folder, bench_type, base3, 4, dict1)

dict1['title']='9% n, 1% p, SL1'
dict1['y_lim']=3
dict1['y_labels']='speedup'
series5=get_matching_series([input_folder, 'tpcc', 5, 7, 1, 9, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series5, base3, 4, dict1)

dict1['title']='9% n, 1% p, SL2'
series6=get_matching_series([input_folder, 'tpcc', 5, 7, 2, 9, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series6, base3, 4, dict1)

#### 80% new-order, 10% payment
dict1['title']='80% n, 10% p baseline'
dict1['y_lim']=0.3
dict1['y_labels']='K txs/s'
base4=get_matching_series([input_folder, 'tpcc', 5, 7, 8, 80, 4])
plot_multi_lines(input_folder, output_folder, bench_type, base4, 4, dict1)

dict1['title']='80% n, 10% p, SL1'
dict1['y_lim']=5
dict1['y_labels']='speedup'
series7=get_matching_series([input_folder, 'tpcc', 5, 7, 1, 80, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series7, base4, 4, dict1)

dict1['title']='80% n, 10% p, SL2'
series8=get_matching_series([input_folder, 'tpcc', 5, 7, 2, 80, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series8, base4, 4, dict1)

input_folder='./stat/2016-06-19-153051/rubis/'
output_folder='./figures/think/'
bench_type='rubis'
dict1={'title':'Rubis SL1', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':5, 'legend_type':'warehouse', 'commit_legend':['T0', 'T250', 'T500', 'T1000', 'T2000'], 'abort_legend':[], 'has_legend':True}

dict1['title']='rubis baseline'
dict1['y_lim']=5
base1=get_matching_series([input_folder, 'rubis', 5, 8, 4])
plot_multi_lines(input_folder, output_folder, bench_type, base1, 4, dict1)

dict1['y_lim']=10
dict1['title']='rubis SL1'
series1=get_matching_series([input_folder, 'rubis', 5, 1, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series1, base1, 4, dict1)

dict1['title']='rubis SL2'
series2=get_matching_series([input_folder, 'rubis', 5, 2, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series2, base1, 4, dict1)
