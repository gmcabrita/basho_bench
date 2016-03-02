#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import glob
import os
import matplotlib.gridspec as gridspec
from os.path import basename
import numpy as np
from time import gmtime, strftime
from plot_line import plot_multi_lines
from helper import get_matching_series

gs1 = gridspec.GridSpec(3, 3)
gs1.update(wspace=0.025, hspace=0.05)

input_folder='./stat/2016-02-01-115146/'
output_folder='./figures/2016-02-01-0518434_10_11_1/'
bench_type='micro'
dict1={'title':'Low contention', 'x_labels':False, 'y_labels':'Throughput (tps)', 'y_lim':2100, 'legend_type':'process'}
series1=get_matching_series(['./stat/2016-02-01-115146/', 'micro', 1, 4, 10, 12, 1000000, 3, 11])
plot_multi_lines(input_folder, output_folder, bench_type, series1, 11, dict1)

series2=get_matching_series(['./stat/2016-02-01-115146/','micro', 1, 4, 10, 12, 100000, 3, 11])
dict2={'title':'Medium contention', 'x_labels':False, 'y_labels':False, 'y_lim':2100, 'legend_type':'process'}
plot_multi_lines(input_folder, output_folder, bench_type, series2, 11, dict2)

series3=get_matching_series(['./stat/2016-02-01-115146/','micro',1,4,10,12,20000,3,11])
dict3={'title':'High contention', 'x_labels':False, 'y_labels':False, 'y_lim':2100, 'legend_type':'process'}
plt.subplot(gs1[2])
plot_multi_lines(input_folder, output_folder, bench_type, series3, 11, dict3)

series4=get_matching_series(['./stat/2016-02-01-115146/','micro',4,10,11,1000000,3,0,1])
dict4={'title':'Low contention', 'x_labels':False, 'y_labels':'Throughput (tps)', 'y_lim':2100, 'legend_type':'locality', 'no_title':True}
plt.subplot(gs1[3])
plot_multi_lines(input_folder, output_folder, bench_type, series4, 1, dict4) 

series5=get_matching_series(['./stat/2016-02-01-115146/','micro',4,10,11,100000,3,0,1])
dict5={'title':'Medium contention', 'x_labels':False, 'y_labels':False, 'y_lim':2100, 'legend_type':'locality', 'no_title':True}
plt.subplot(gs1[4])
plot_multi_lines(input_folder, output_folder, bench_type, series5, 1, dict5) 

series6=get_matching_series(['./stat/2016-02-01-115146/', 'micro', 4, 10, 11, 20000, 3, 0, 1])
dict6={'title':'High contention', 'x_labels':False, 'y_labels':False, 'y_lim':2100, 'legend_type':'locality', 'no_title':True}
plt.subplot(gs1[5])
plot_multi_lines(input_folder, output_folder, bench_type, series6, 1, dict6) 

series7=get_matching_series(['./stat/2016-02-01-115146/', 'micro', 1, 4, 3, 1000000, 10]) 
dict7={'title':'Low contention', 'x_labels':False, 'y_labels':'Throughput (tps)', 'y_lim':2100, 'legend_type':'range', 'no_title':True}
plt.subplot(gs1[6])
plot_multi_lines(input_folder, output_folder, bench_type, series7, 10, dict7) 

series8=get_matching_series(['./stat/2016-02-01-115146/', 'micro', 1, 4, 3, 100000, 10])
dict8={'title':'Medium contention', 'x_labels':False, 'y_labels':False, 'y_lim':2100, 'legend_type':'range', 'no_title':True}
plt.subplot(gs1[7])
plot_multi_lines(input_folder, output_folder, bench_type, series8, 10, dict8) 

series9=get_matching_series(['./stat/2016-02-01-115146/', 'micro', 1, 4, 3, 20000, 10])
dict9={'title':'High contention', 'x_labels':False, 'y_labels':False, 'y_lim':2100, 'legend_type':'range', 'no_title':True}
plt.subplot(gs1[8])
plot_multi_lines(input_folder, output_folder, bench_type, series9, 10, dict9) 

