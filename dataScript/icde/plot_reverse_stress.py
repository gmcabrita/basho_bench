#!/usr/bin/env python
#######!/usr/bin/env python3

import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from pylab import *
import sys
sys.path.append("/Users/liz/Documents/MyDocument/repositories/basho_bench2/dataScript/")
import random
from plot_stress import *
from itertools import chain
from plot_reverse_line import * 
import os
import numpy as np
import pandas as pd
import re
from datetime import * 

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
#input_folder='./stat/2016-10-12-160111/'
#input_folder='./stat/2016-10-15-144134/'

input_folder='./stat/2016-10-16-181658merge/'
time=datetime.now().strftime("%Y%m%d-%H:%M:%S")
output_folder='./figures/eurosys/transpose/' + time
os.mkdir(output_folder)
ss1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 15000, 'true', 'true', 'noauto', 8])
#dist1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 15000, 'true', 'false', 'distauto', 8])
dist1=[]
dict1={'title':'low low abort', 'no_title':True, 'x_labels':False, 'ax1_labels':'Thousand txs/sec', 'ax2_labels':'Abort rate', 'ax3_labels':'Latency(ms) in log', 'y_lim':1.5, 'legend_type':'warehouse', 'legend_loc':'upper center', 'commit_legend':['10', '20', '40', '80', '160'], 'has_legend':True, 'th_lim':5, 'lat_lim':50000}
dict1['under_labels']='(a) Low local, low remote'
ss1.reverse()
lgd=plot_reverse_lines(input_folder, output_folder, 'micro', ss1+dist1, 6, dict1)

dict1['has_legend']=True
dict1['ax1_labels']=False
dict1['ax2_labels']=False
dict1['ax3_labels']=False
ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 15000, 'true', 'true', 'noauto', 8])
#ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 5000, 10000, 'false', 'false', 8])
#dist2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 15000, 'true', 'false', 'distauto', 8])
dist2=[]
dict1['title']='highlow'
dict1['y_ticks']=False
dict1['under_labels']='(b) High local, low remote'
ss2.reverse()
#ss2[3]=sort_by_num(ss2[3])
#del ss2[3][-1]
plot_reverse_lines(input_folder, output_folder, 'micro', ss2+dist2, 6, dict1)

dict1['y_ticks']=False
dict1['has_legend']=False
ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 500, 'true', 'true', 'noauto', 8])
#dist2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 500, 'true', 'false', 'distauto', 8])
dist2=[]
dict1['title']='lowhigh'
dict1['under_labels']='(c) Low local, high remote'
ss2.reverse()
for i, ss in enumerate(ss2):
    break
    s_ss1 = sort_by_num(ss)
    s_ns1 = sort_by_num(ns2)
    spec_length = get_field(s_ss1[0], 8)
    plot_stress(s_ss1, s_ns1, input_folder, output_folder, 'lowhigh'+str(spec_length))

plot_reverse_lines(input_folder, output_folder, 'micro', ss2+dist2, 6, dict1)

ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 500, 'true', 'true', 'noauto', 8])
#dist2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 500, 'true', 'false', 'distauto', 8])
dist2=[]
dict1['title']='highhigh'
dict1['under_labels']='(d) High local, high remote'
ss2.reverse()
#ss2[2]=sort_by_num(ss2[2])
#ss2[3]=sort_by_num(ss2[3])
#del ss2[2][-1]
#del ss2[3][-1]
plot_reverse_lines(input_folder, output_folder, 'micro', ss2+dist2, 6, dict1)

