#!/usr/bin/env python

import matplotlib.pyplot as plt
from pylab import *
import sys
from copy import deepcopy
import random
#from plot_stress import *
#from plot_speedup_abort import *
from plot_line_abort import *
from plot_lat import *
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

input_folder='./stat/2016-07-17-151555-mod/'
output_folder='./figures/macro_stress/rubis/'
bench_type='rubis'
dict1={'y_labels':'K txs/s', 'x_labels':['100 cls', '500 cls', '1000 cls', '2500 cls', '3000 cls', '4000 cls'], 'y_lim':5, 'legend_type':'warehouse', 'commit_legend':['Default think'], 'abort_legend':[], 'has_legend':True}
dict2=deepcopy(dict1)
dict2['x_labels']=['100 cls', '500 cls', '1000 cls', '2500 cls', '3000 cls', '4000 cls']
dict2['y_labels']='Latency (ms)',
dict2['title']='rubislatency'
dict2['y_lim']=5000

dict1['title']='rubis'
dict1['y_lim']=2.5
ss1=get_matching_series([input_folder, 'rubis', 3, 'true', 2])
ns1=get_matching_series([input_folder, 'rubis', 3, 'false', 2])
s_ss1 = sort_by_num([val for sublist in ss1 for val in sublist])
s_ns1 = sort_by_num([val for sublist in ns1 for val in sublist])
plot_multi_lines(input_folder, output_folder, bench_type, [s_ss1, s_ns1], 4, dict1)
plot_latency(input_folder, output_folder, bench_type, [s_ss1, s_ns1], 4, dict2)

input_folder='./stat/2016-07-17-131653/'
output_folder='./figures/macro_stress/tpcc/'
bench_type='tpcc'
dict1={'y_labels':'K txs/s', 'x_labels':['500 cls', '1000 cls', '2000 cls', '3000 cls', '4000 cls'], 'y_lim':1, 'legend_type':'warehouse', 'commit_legend':['Default think'], 'abort_legend':[], 'has_legend':True}
dict1['title']='1% n, 9% p baseline'
dict1['y_lim']=1.1
specula_base1=get_matching_series([input_folder, 'tpcc', 3, 7, 'true', 1, 2])
base1=get_matching_series([input_folder, 'tpcc', 3, 7, 'false', 1, 2])
s_ss1 = sort_by_num([val for sublist in specula_base1 for val in sublist])
s_ns1 = sort_by_num([val for sublist in base1 for val in sublist])
plot_multi_lines(input_folder, output_folder, bench_type, [s_ss1, s_ns1], 4, dict1)
dict2['title']='19latency'
dict2['y_lim']=15000
plot_latency(input_folder, output_folder, bench_type, [s_ss1, s_ns1], 4, dict2)

dict1['title']='9% n, 1% p baseline'
dict1['y_lim']=1.1
specula_base2=get_matching_series([input_folder, 'tpcc', 3, 7, 'true', 9, 2])
base2=get_matching_series([input_folder, 'tpcc', 3, 7, 'false', 9, 2])
s_ss1 = sort_by_num([val for sublist in specula_base2 for val in sublist])
s_ns1 = sort_by_num([val for sublist in base2 for val in sublist])
plot_multi_lines(input_folder, output_folder, bench_type, [s_ss1, s_ns1], 4, dict1)
dict2['y_lim']=1000
dict2['title']='91latency'
plot_latency(input_folder, output_folder, bench_type, [s_ss1, s_ns1], 4, dict2)
exit()

input_folder='./stat/2016-07-10-213002/tpcc/'
output_folder='./figures/macro_stress/'
bench_type='tpcc'
dict1={'y_labels':'K txs/s', 'x_labels':['64 cls', '128 cls', '256 cls', '384 cls', '512 cls'], 'y_lim':1, 'legend_type':'warehouse', 'commit_legend':['Default think'], 'abort_legend':[], 'has_legend':True}

dict1['title']='10% n, 80% p baseline'
dict1['y_lim']=0.05
base1=get_matching_series([input_folder, 'tpcc', 3, 7, 'false', 10, 1])
plot_multi_lines(input_folder, output_folder, bench_type, base1, 4, dict1)

dict1['y_lim']=5
dict1['title']='10% n, 80% p, SL8'
dict1['y_labels']='speedup'
series1=get_matching_series([input_folder, 'tpcc', 3, 7, 'true', 10, 1])
plot_speedup_abort(input_folder, output_folder, bench_type, series1, base1, 4, dict1)

#### 1% new-order, 9% payment
dict1['title']='1% n, 9% p baseline'
dict1['y_lim']=0.3
dict1['y_labels']='K txs/s'
base2=get_matching_series([input_folder, 'tpcc', 3, 7, 'false', 1, 1])
plot_multi_lines(input_folder, output_folder, bench_type, base2, 4, dict1)

dict1['title']='1% n, 9% p, SL8'
dict1['y_lim']=1.2
dict1['y_labels']='speedup'
series4=get_matching_series([input_folder, 'tpcc', 3, 7, 'true', 1, 1])
plot_speedup_abort(input_folder, output_folder, bench_type, series4, base2, 4, dict1)

#### 9% new-order, 1% payment
dict1['title']='9% n, 1% p baseline'
dict1['y_lim']=0.2
dict1['y_labels']='K txs/s'
base3=get_matching_series([input_folder, 'tpcc', 3, 7, 'false', 9, 1])
plot_multi_lines(input_folder, output_folder, bench_type, base3, 4, dict1)

dict1['title']='9% n, 1% p, SL8'
dict1['y_lim']=1.2
dict1['y_labels']='speedup'
series5=get_matching_series([input_folder, 'tpcc', 3, 7, 'true', 9, 1])
plot_speedup_abort(input_folder, output_folder, bench_type, series5, base3, 4, dict1)

#### 80% new-order, 10% payment
dict1['title']='80% n, 10% p baseline'
dict1['y_lim']=0.3
dict1['y_labels']='K txs/s'
base4=get_matching_series([input_folder, 'tpcc', 3, 7, 'false', 80, 4])
plot_multi_lines(input_folder, output_folder, bench_type, base4, 4, dict1)

dict1['title']='80% n, 10% p, SL8'
dict1['y_lim']=1.2
dict1['y_labels']='speedup'
series8=get_matching_series([input_folder, 'tpcc', 3, 7, 'true', 80, 4])
plot_speedup_abort(input_folder, output_folder, bench_type, series8, base4, 4, dict1)


input_folder='./stat/2016-07-10-213002/rubis/'
output_folder='./figures/macro_stress/'
bench_type='rubis'
dict1={'title':'Rubis SL1', 'no_title':True, 'x_labels':['64 cls', '128 cls', '256 cls', '384 cls', '512 cls'], 'y_labels':False, 'y_lim':5, 'legend_type':'warehouse', 'commit_legend':['Default think'], 'abort_legend':[], 'has_legend':True}

dict1['title']='rubis baseline'
dict1['y_lim']=0.3
base1=get_matching_series([input_folder, 'rubis', 3, 'false', 1])
plot_multi_lines(input_folder, output_folder, bench_type, base1, 4, dict1)

dict1['y_lim']=1.2
dict1['title']='rubis SL8'
dict1['y_labels']='speedup'
series1=get_matching_series([input_folder, 'rubis', 3, 'true', 1])
plot_speedup_abort(input_folder, output_folder, bench_type, series1, base1, 4, dict1)
