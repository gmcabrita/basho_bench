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
from helper import * 

#input_folder='./stat/2016-03-06-232257/'
input_folder='./stat/2016-03-08-023721/'
output_folder='./figures/final_micro/'
bench_type='micro'
dict1={'title':'Low local contention, low remote contention', 'big':True, 'no_title':True, 'x_labels':False, 'y_labels':'Transactions per second', 'y_lim':5000, 'legend_type':'read'}
series1=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 40000, 'true', 9])
[base_serie1]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 40000, 'false'])
series1 = [[base_serie1]+s  for s in series1]
print(series1)
plot_multi_lines(input_folder, output_folder, bench_type, series1, 9, dict1)

dict2={'title':'Low local contention, high remote contention','big':True, 'no_title':True, 'x_labels':False, 'y_axis':False, 'y_labels':False, 'y_lim':5000, 'legend_type':'read'}
series2=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 2000, 'true', 9])
[base_serie2]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 2000, 'false'])
series2 = [[base_serie2]+s  for s in series2]
plot_multi_lines(input_folder, output_folder, bench_type, series2, 9, dict2)

dict3={'title':'High local contention, low remote contention','big':True, 'no_title':True, 'x_labels':False, 'y_labels':'Transactions per second', 'y_lim':5000, 'legend_type':'read'}
series3=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 1000, 40000, 'true', 9])
[base_serie3]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 1000, 40000, 'false'])
series3 = [[base_serie3]+s  for s in series3]
plot_multi_lines(input_folder, output_folder, bench_type, series3, 9, dict3)

dict4={'title':'High local contention, high remote contention','big':True, 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_axis':False, 'y_lim':5000, 'legend_type':'read'}
series4=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 1000, 2000, 'true', 9])
[base_serie4]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 1000, 2000, 'false'])
series4 = [[base_serie4]+s  for s in series4]
plot_multi_lines(input_folder, output_folder, bench_type, series4, 9, dict4)

dict5={'title':'Remote read', 'no_title':True, 'x_labels':False, 'y_labels':'Transactions per second', 'big':True, 'y_lim':5000, 'legend_type':'remote read'}
series5=get_matching_series_delete([input_folder, 'micro', 1, 4, 6, 80, 20000, 40000, 3],  [(7, 'true'), (9, 'nospecula')], {'order':'ascend'})
plot_multi_lines(input_folder, output_folder, bench_type, series5, 3, dict5)

dict6={'title':'Number of updated servers', 'no_title':True, 'x_labels':False, 'big':'True', 'y_labels':'Transactions per second', 'y_lim':3000, 'legend_type':'remote servers'}
series6=get_matching_series_delete([input_folder, 'micro', 1, 4, 6, 20, 40000, 60000, 12], [(12, 'false')], {'order':'ascend'})
plot_multi_lines(input_folder, output_folder, bench_type, series6, 12, dict6)

dict7={'title':'Duration of committed transaction', 'no_title':True, 'x_labels':False, 'big':'True', 'y_labels':'Duration(ms)', 'y_lim':3500, 'legend_type':'remote servers', 'type':'commit'}
s1=get_matching_series_delete([input_folder, 'micro', 1, 2, 4, 6, 9, 80, 20, 20000, 40000, 'specula', 9],  [], {'order':'ascend'})
[base_s1]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 40000, 'false'])
s1=[[base_s1]+s  for s in s1]
s3=get_matching_series_delete([input_folder, 'micro', 1, 4, 6, 20, 40000, 60000, 12], [(12, 'false')], {'order':'ascend'})
series7=s1+s3
plot_multi_lines(input_folder, output_folder, bench_type, series7, 12, dict7)

dict8={'title':'Duration of aborted transaction', 'no_title':True, 'x_labels':False, 'big':'True', 'y_labels':'Duration(ms)', 'y_lim':2500, 'legend_type':'remote servers', 'type':'abort'}
plot_multi_lines(input_folder, output_folder, bench_type, series7, 12, dict8)
