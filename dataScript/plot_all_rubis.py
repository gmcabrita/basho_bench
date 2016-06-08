#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import glob
import os
import matplotlib.gridspec as gridspec
from os.path import basename
import numpy as np
from time import gmtime, strftime
from plot_line_abort import plot_multi_lines
from helper import * 

#input_folder='./stat/2016-05-20-132859/'
input_folder='./stat/2016-05-23-122839/'
output_folder='./figures/final_rubis/'
bench_type='tpcc'
dict1={'title':'Rubis', 'no_title':True, 'x_labels':True, 'y_labels':True, 'y_labels':'Thousand txs/sec', 'y_lim':20, 'legend_type':'', 'line_name':'max', 'commit_legend':['No spec: commits', 'Speculation: commits'], 'abort_legend':['No spec: UC', 'Speculation: UC', 'Speculation: AC'], 'has_legend':True, 'base_line':True, 'size':(8, 5)}
[series1]=get_matching_series([input_folder, 'tpcc', 1, 80, 0])
series1= sort_by_num(series1)
baseline = series1[0]
others = series1[1:]
res = [[baseline], others]
#print(res)
#plot_multi_lines(input_folder, output_folder, bench_type, series1, 0, dict1)
plot_multi_lines(input_folder, output_folder, bench_type, [baseline, others], 0, dict1)
