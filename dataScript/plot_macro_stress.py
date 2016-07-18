#!/usr/bin/env python
#######!/usr/bin/env python3


import matplotlib.pyplot as plt
from pylab import *
import sys
import random
from plot_stress import *
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

#input_folder='./stat/2016-06-18-115824/'
#input_folder='./stat/2016-06-20-231215/'
input_folder='./stat/2016-07-17-151555-mod/'
ss1=get_matching_series([input_folder, 'rubis', 3, 'true', 2])
ns1=get_matching_series([input_folder, 'rubis', 3, 'false', 2])
s_ss1 = sort_by_num([val for sublist in ss1 for val in sublist])
s_ns1 = sort_by_num([val for sublist in ns1 for val in sublist])
plot_stress(s_ss1, s_ns1, input_folder, './figures/macro_stress/rubis/', 'rubis')

exit()
comm='''
#ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 1000, 2000, 'true', 'true', 8])
ss2=[]
[ns2]=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 1000, 2000, 'false', 'false', 8])
for i, ss in enumerate(ss2):
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    #print(s_ss1)
    #print(s_ns1)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, './figures/stress/', 'highhigh'+str(spec_length))
'''

ss3=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 1000, 40000, 'true', 'true', 8])
#[ns3]=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 1000, 40000, 'false', 'false', 8])
ns3=[]
for i, ss in enumerate(ss3):
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns3)
    #print(s_ss1)
    #print(s_ns1)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, './figures/stress/', 'highlow'+str(spec_length))


#ss4=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 20000, 2000, 'true', 'true', 8])
comm='''
ss4=[]
[ns4]=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 20000, 2000, 'false', 'false', 8])
for i, ss in enumerate(ss4):
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns4)
    #print(s_ss1)
    #print(s_ns1)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, './figures/stress/', 'lowhigh'+str(spec_length))
'''
