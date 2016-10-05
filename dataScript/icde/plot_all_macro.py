#!/usr/bin/env python
#######!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
sys.path.append("/Users/liz/Documents/MyDocument/repositories/basho_bench2/dataScript/")
import random
from plot_stress import *
from itertools import chain
from plot_icde_line import plot_multi_lines
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

#input_folder='./stat/2016-09-25-154550/'
#input_folder='./stat/2016-09-26-022800/'
#input_folder='./stat/2016-09-26-230722/'
#input_folder='./stat/2016-09-30-150417/'
#input_folder='./stat/2016-09-29-184315/'
input_folder='./stat/2016-10-03-235816/rubis/'
output_folder='./figures/icde/macro/'
ss2=get_matching_series([input_folder, 'rubis', 3, 'true', 5])
[ns2]=get_matching_series([input_folder, 'rubis', 3, 'false', 5])
dict1={'title':'rubis', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':1.5, 'legend_type':'warehouse', 'legend_loc':'upper center', 'commit_legend':['Baseline', 'SL0', 'SL4'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC'], 'has_legend':True}
dict1['title']='rubis'
ss2.reverse()
print(ss2)
print(ns2)
#ss2=ss2[:-1]
for i, ss in enumerate(ss2):
    break
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, output_folder, 'rubis'+str(spec_length))
plot_multi_lines(input_folder, output_folder, 'rubis', [ns2]+ss2, 5, dict1)


input_folder='./stat/2016-10-04-225001/tpcc/'
ss1=get_matching_series([input_folder, 'tpcc', 3, 7, 8, 'true', 10, 80, 5])
[ns1]=get_matching_series([input_folder, 'tpcc', 3, 7,8, 'false', 10, 80, 5])
dict1={'title':'tpcc1080', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':1.5, 'legend_type':'warehouse', 'legend_loc':'upper center', 'commit_legend':['Baseline', 'SL0', 'SL4'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC'], 'has_legend':True}
ss1.reverse()
plot_multi_lines(input_folder, output_folder, 'micro', [ns1]+ss1, 6, dict1)

ss2=get_matching_series([input_folder, 'tpcc', 3, 7, 8, 'true', 80, 10, 5])
[ns2]=get_matching_series([input_folder, 'tpcc', 3, 7,8, 'false', 80, 10, 5])
dict1['title']='tpcc8010'
ss2.reverse()
for i, ss in enumerate(ss2):
    break
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, output_folder, 'highhigh'+str(spec_length))
ns2=sort_by_num(ns2)[:]
dss2 = []
for s in ss2:
    sort_s = sort_by_num(s)
    dss2.append(sort_s[:])
plot_multi_lines(input_folder, output_folder, 'micro', [ns2]+dss2, 6, dict1)

ss2=get_matching_series([input_folder, 'tpcc', 3, 7, 8, 'true', 10, 10, 5])
[ns2]=get_matching_series([input_folder, 'tpcc', 3, 7,8, 'false', 10, 10, 5])
dict1['title']='tpcc1010'
ss2.reverse()
for i, ss in enumerate(ss2):
    break
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, output_folder, 'lowhigh'+str(spec_length))
ns2=sort_by_num(ns2)[:]
dss2 = []
for s in ss2:
    sort_s = sort_by_num(s)
    dss2.append(sort_s[:]) 
plot_multi_lines(input_folder, output_folder, 'micro', [ns2]+dss2, 6, dict1)

