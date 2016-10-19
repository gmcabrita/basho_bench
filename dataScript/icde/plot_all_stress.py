#!/usr/bin/env python
#######!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
sys.path.append("/Users/liz/Documents/MyDocument/repositories/basho_bench2/dataScript/")
import random
from plot_stress import *
from itertools import chain
from plot_icde_line_share import *
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


fig = plt.figure()
ax1 = plt.subplot2grid((3,4), (0,0))
ax2 = plt.subplot2grid((3,4), (1,0))
ax3 = plt.subplot2grid((3,4), (2,0))
ax1.yaxis.labelpad = 22
ax2.yaxis.labelpad = 11
#gs = gridspec.GridSpec(3, 4)
#gs.update(hspace=0.001, wspace=0.05)
#ax1 = plt.subplot(gs[0, 0])
#ax2 = plt.subplot(gs[1, 0])
#ax3 = plt.subplot(gs[2, 0])

input_folder='./stat/2016-10-16-181658merge/'
time=datetime.now().strftime("%Y%m%d-%H:%M:%S")
output_folder='./figures/eurosys/test/' + time
os.mkdir(output_folder)
ss1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 15000, 'true', 'true', 'noauto', 8])
ns1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 15000, 'false', 'false', 'noauto', 8])
dist1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 15000, 'true', 'false', 'distauto', 8])
planet1=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 15000, 'true', 'false', 'noauto', 8])
dict1={'title':'low low abort', 'no_title':True, 'x_labels':False, 'ax1_labels':'Thousand txs/sec', 'ax2_labels':'Abort rate', 'ax3_labels':'Latency(ms) in log', 'y_lim':1.5, 'legend_type':'warehouse', 'legend_loc':'upper center', 'commit_legend':['ClockSI-Rep', 'PLANET', 'STR-Strong', 'STR-Tuning'], 'has_legend':True, 'th_lim':5, 'lat_lim':7000, 'out_legend':False}
dict1['under_labels']='(a) Low local, low remote'
ss1.reverse()
lgd=plot_multi_lines(input_folder, output_folder, 'micro', ns1+planet1+[ss1[0]]+dist1, 6, dict1, ax1, ax2, ax3)

dict1['has_legend']=True
dict1['out_legend']=True
dict1['ax1_labels']=False
dict1['ax2_labels']=False
dict1['ax3_labels']=False
ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 15000, 'true', 'true', 'noauto', 8])
#ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 5000, 10000, 'false', 'false', 8])
ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 15000, 'false', 'false', 'noauto', 8])
planet2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 15000, 'true', 'false', 'noauto', 8])
dist2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 15000, 'true', 'false', 'distauto', 8])
print(dist2)
dict1['title']='highlow'
dict1['y_ticks']=False
#ax1 = plt.subplot(gs[0, 1])
#ax2 = plt.subplot(gs[1, 1])
#ax3 = plt.subplot(gs[2, 1])
ax1 = plt.subplot2grid((3,4), (0,1))
ax2 = plt.subplot2grid((3,4), (1,1))
ax3 = plt.subplot2grid((3,4), (2,1))
dict1['under_labels']='(b) High local, low remote'
ss2.reverse()
plot_multi_lines(input_folder, output_folder, 'micro', ns2+planet2+[ss2[0]]+dist2, 6, dict1, ax1, ax2, ax3)

#ax1 = plt.subplot(gs[0, 2])
#ax2 = plt.subplot(gs[1, 2])
#ax3 = plt.subplot(gs[2, 2])
ax1 = plt.subplot2grid((3,4), (0,2))
ax2 = plt.subplot2grid((3,4), (1,2))
ax3 = plt.subplot2grid((3,4), (2,2))
dict1['y_ticks']=False
dict1['has_legend']=False
ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 500, 'true', 'true', 'noauto', 8])
#ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 50000, 1000, 'false', 'false', 8])
ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 500, 'false', 'false', 'noauto', 8])
planet2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 500, 'true', 'false', 'noauto', 8])
dist2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 30000, 500, 'true', 'false', 'distauto', 8])
dict1['title']='lowhigh'
dict1['under_labels']='(c) Low local, high remote'
ss2.reverse()
print(dist2)
plot_multi_lines(input_folder, output_folder, 'micro', ns2+planet2+[ss2[0]]+dist2, 6, dict1, ax1, ax2, ax3)

#ax1 = plt.subplot(gs[0, 3])
#ax2 = plt.subplot(gs[1, 3])
#ax3 = plt.subplot(gs[2, 3])
ax1 = plt.subplot2grid((3,4), (0,3))
ax2 = plt.subplot2grid((3,4), (1,3))
ax3 = plt.subplot2grid((3,4), (2,3))
ss2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 500, 'true', 'true', 'noauto', 8])
ns2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 500, 'false', 'false', 'noauto', 8])
planet2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 500, 'true', 'false', 'noauto', 8])
dist2=get_matching_series([input_folder, 'micro', 4, 6, 7, 9, 14, 1000, 500, 'true', 'false', 'distauto', 8])
dict1['title']='highhigh'
dict1['under_labels']='(d) High local, high remote'
ss2.reverse()
print(dist2)
plot_multi_lines(input_folder, output_folder, 'micro', ns2+planet2+[ss2[0]]+dist2, 6, dict1, ax1, ax2, ax3)

fig.set_size_inches(20, 7)
#fig.subplots_adjust(hspace = -1)
plt.tight_layout(pad=2.8, w_pad=1, h_pad=-0.7)
#plt.tight_layout()
#fig.savefig(output_folder+'/micro.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
fig.savefig(output_folder+'/micro.pdf', format='pdf', bbox_extra_artists=(lgd,))
