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
dict1={'title':'Low local contention, low remote contention', 'big':True, 'no_title':True, 'x_labels':False, 'y_labels':'Transactions per second(tps)', 'y_lim':5000, 'legend_type':'read'}
series1=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 20000, 40000, 'true', 'specula', 'new', 9])
series12=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 40000, 60000, 'true', 'specula', 'old', 9])
series13=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 20000, 40000, 'true', 'nospecula', 'new', 9])
[base_serie1]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 40000, 'false'])
series1 = [[base_serie1]+s  for s in series1]
series12 = [[base_serie1]+s  for s in series12]
series13 = [[base_serie1]+s  for s in series13]
plot_multi_lines(input_folder, output_folder, bench_type, series1+series12+series13, ['SP3', 'SP2', 'SP1'], dict1)

dict2={'title':'Low local contention, high remote contention','big':True, 'no_title':True, 'x_labels':False, 'y_axis':False, 'y_labels':False, 'y_lim':5000, 'legend_type':'read'}
series2=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 20000, 2000, 'true', 'specula', 'new', 9])
series22=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 40000, 2000, 'true', 'specula', 'old', 9])
series23=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 40000, 2000, 'true', 'nospecula', 'old', 9])
[base_serie2]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 2000, 'false'])
series2 = [[base_serie2]+s  for s in series2]
series22 = [[base_serie2]+s  for s in series22]
series23 = [[base_serie2]+s  for s in series23]
plot_multi_lines(input_folder, output_folder, bench_type, series2+series22+series23, ['SP3', 'SP2', 'SP1'], dict2)

dict3={'title':'High local contention, low remote contention','big':True, 'no_title':True, 'x_labels':False, 'y_labels':'Transactions per second(tps)', 'y_lim':5000, 'legend_type':'read'}
series3=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 40000, 'true', 'specula', 'new', 9])
series32=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 60000, 'true', 'specula', 'old', 9])
series33=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 60000, 'true', 'nospecula', 'old', 9])
[base_serie3]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 1000, 40000, 'false'])
series3 = [[base_serie3]+s  for s in series3]
series32 = [[base_serie3]+s  for s in series32]
series33 = [[base_serie3]+s  for s in series33]
plot_multi_lines(input_folder, output_folder, bench_type, series3+series32+series33, ['SP3', 'SP2', 'SP1'], dict3)

dict4={'title':'High local contention, high remote contention','big':True, 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_axis':False, 'y_lim':5000, 'legend_type':'read'}
series4=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 2000, 'true', 'specula', 'new', 9])
series42=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 2000, 'true', 'specula', 'old', 9])
series43=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 2000, 'true', 'nospecula', 'old', 9])
[base_serie4]=get_matching_serie([input_folder, 'micro', 0, 1, 2, 4, 6, 7, 8, 80, 20, 1000, 2000, 'false'])
series4 = [[base_serie4]+s  for s in series4]
series42 = [[base_serie4]+s  for s in series42]
series43 = [[base_serie4]+s  for s in series43]
plot_multi_lines(input_folder, output_folder, bench_type, series4+series42+series43, ['SP3', 'SP2', 'SP1'], dict4)

dict5={'title':'Remote read', 'no_title':True, 'x_labels':False, 'y_labels':'Transactions per second(tps)', 'big':True, 'y_lim':5000, 'legend_type':'remote read'}
series5=get_matching_series_delete([input_folder, 'micro', 1, 4, 6, 80, 20000, 40000, 3],  [(7, 'true'), (9, 'nospecula')], {'order':'ascend'})
plot_multi_lines(input_folder, output_folder, bench_type, series5, 3, dict5)

#dict6={'title':'Number of updated servers', 'no_title':True, 'x_labels':False, 'big':'True', 'y_labels':'Transactions per second', 'y_lim':3000, 'legend_type':'remote servers'}
##series6=get_matching_series_delete([input_folder, 'micro', 1, 4, 6, 20, 40000, 60000, 12], [(12, 'false')], {'order':'ascend'})
#plot_multi_lines(input_folder, output_folder, bench_type, series6, 12, dict6)
dict6={'title':'Number of updated servers', 'no_title':True, 'x_labels':False, 'big':'True', 'y_labels':'Transactions per second(tps)', 'y_lim':5000, 'legend_type':'locality'}
series61=get_matching_series_delete([input_folder, 'micro', 3, 4, 6, 12, 13, 0, 20000, 40000, 'false', 'new', 2], [(7,'true'), (9, 'nospecula')], {'order':'ascend'})
print(series61)
series62=get_matching_series_delete([input_folder, 'micro', 0, 3, 4, 6, 12, 13, 8, 0, 40000, 60000, 'false', 'new', 2], [], {'order':'ascend'})
base_serie62=get_matching_serie([input_folder, 'micro', 0, 3, 4, 6, 7, 12, 13, 8, 0, 40000, 60000, 'false', 'false', 'old']) 
base_serie62=sort_by_num(base_serie62)
base_serie62.reverse()
for i, s in enumerate(series62):
    s.insert(0, base_serie62[i])
#base_serie62=get_matching_serie([input_folder, 'micro', 0, 3, 4, 6, 7, 12, 13, 8, 0, 40000, 60000, 'false', 'false', 'old'])
print(series62)
series6 = series61+series62
plot_multi_lines(input_folder, output_folder, bench_type, series6, 2, dict6)

dict7={'title':'Duration of committed transaction', 'no_title':True, 'x_labels':False, 'big':'True', 'y_labels':'Duration(ms)', 'y_lim':2000, 'legend_type':'locality', 'type':'commit'}
#s1=get_matching_series_delete([input_folder, 'micro', 3, 4, 6, 9, 80, 20, 20000, 40000, 'specula', 9],  [], {'order':'ascend'})
#[base_s1]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 40000, 'false'])
#s1=[[base_s1]+s  for s in s1]
#s3=get_matching_series_delete([input_folder, 'micro', 1, 4, 6, 20, 40000, 60000, 12], [(12, 'false')], {'order':'ascend'})
#series7=s1+s3
plot_multi_lines(input_folder, output_folder, bench_type, series6, 2, dict7)

dict8={'title':'Duration of aborted transaction', 'no_title':True, 'x_labels':False, 'big':'True', 'y_labels':'Duration(ms)', 'y_lim':2000, 'legend_type':'locality', 'type':'abort'}
plot_multi_lines(input_folder, output_folder, bench_type, series6, 2, dict8)
