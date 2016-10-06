#!/usr/bin/env python
#######!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
sys.path.append("/Users/liz/Documents/MyDocument/repositories/basho_bench2/dataScript/")
import random
from plot_stress import *
from itertools import chain
from plot_icde_line import * 
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
#input_folder='./stat/2016-10-05-130734/'
#input_folder='./stat/2016-10-05-165257/'
#input_folder='./stat/2016-10-05-232558/'
input_folder='./stat/2016-10-06-150759/'
#input_folder='./stat/2016-10-03-235824/'
output_folder='./figures/icde/stress_merged_test/'
ss1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 40000, 20000, 'true', 'true', 8])
ns1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 40000, 20000, 'false', 'false', 8])
print(ss1)
print(ns1)
dict1={'title':'low low abort', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':1.5, 'legend_type':'warehouse', 'legend_loc':'upper center', 'commit_legend':['Baseline', 'SL0', 'SL1', 'SL4', 'SL8'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC'], 'has_legend':True}
ss1.reverse()

plot_multi_bars(input_folder, output_folder, 'micro', ns1+ss1, 6, dict1)

ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 2000, 500, 'true', 'true', 8])
ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 2000, 500, 'false', 'false', 8])
dict1['title']='highhigh'
ss2.reverse()
for i, ss in enumerate(ss2):
    break
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, output_folder, 'highhigh'+str(spec_length))
plot_multi_bars(input_folder, output_folder, 'micro', ns2+ss2, 6, dict1)

ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 40000, 500, 'true', 'true', 8])
ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 40000, 500, 'false', 'false', 8])
dict1['title']='lowhigh'
ss2.reverse()
for i, ss in enumerate(ss2):
    break
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, output_folder, 'lowhigh'+str(spec_length))
plot_multi_bars(input_folder, output_folder, 'micro', ns2+ss2, 6, dict1)

ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 2000, 20000, 'true', 'true', 8])
ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 2000, 20000, 'false', 'false', 8])
dict1['title']='highlow'
ss2.reverse()
for i, ss in enumerate(ss2):
    break
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, output_folder, 'highlow'+str(spec_length))
plot_multi_bars(input_folder, output_folder, 'micro', ns2+ss2, 6, dict1)
