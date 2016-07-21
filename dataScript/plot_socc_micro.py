#!/usr/bin/env python

import matplotlib.pyplot as plt
import sys
import glob
import os
import matplotlib.gridspec as gridspec
from os.path import basename
import numpy as np
from time import gmtime, strftime
from plot_line_abort import plot_multi_lines
from plot_all_bars import *
from plot_single_bar import *
from plot_line import *
from helper import *
import copy

input_folder='./stat/2016-03-08-023721/'
input_folder2='./stat/2016-05-19-112828/'
output_folder='./figures/final_micro/socc/'
bench_type='micro'

dict1={'title':'Low local contention, low remote contention', 'big':True, 'y_labels':'Thousand txs/sec', 'no_title':True, 'x_labels':'default', 'y_lim':4, 'legend_type':'read', 'y_ticks':True,  'commit_legend':['No spec', 'SP1', 'SP2', 'SP3'], 'abort_legend':['No spec', 'SP1', 'SP2', 'SP3', 'SP1', 'SP2', 'SP3'], 'has_legend':True, 'out_legend':True, 'base_line':True}
dict1['swap']=True
[series1]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 20000, 40000, 'true', 'specula', 'new', 9])
[series12]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 40000, 60000, 'true', 'specula', 'old', 9])
[series13]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 20000, 40000, 'true', 'nospecula', 'new', 9])
[base_serie1]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 40000, 'false'])
#series1 = [[base_serie1]+s  for s in series1]
#series12 = [[base_serie1]+s  for s in series42]
#series13 = [[base_serie1]+s  for s in series43]
plot_multi_lines(input_folder, output_folder, bench_type, [base_serie1, series1, series12, series13], ['SP3', 'SP2', 'SP1'], dict1)

dict1.pop('out_legend', None)
dict2=dict1
dict2['title']='Low local contention, high remote contention'
#dict2['y_labels']=True
dict2['has_legend']=False
dict2['y_ticks']=True
[series2]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 20000, 2000, 'true', 'specula', 'new', 9])
[series22]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 40000, 2000, 'true', 'specula', 'old', 9])
[series23]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 40000, 2000, 'true', 'nospecula', 'old', 9])
[base_serie2]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 20000, 2000, 'false'])
plot_multi_lines(input_folder, output_folder, bench_type, [base_serie2,series2,series22,series23], ['SP3', 'SP2', 'SP1'], dict2)

dict3=dict2
dict3['title']='High local contention, low remote contention'
[series3]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 40000, 'true', 'specula', 'new', 9])
[series32]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 60000, 'true', 'specula', 'old', 9])
[series33]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 60000, 'true', 'nospecula', 'old', 9])
[base_serie3]=get_matching_serie([input_folder, 'micro', 1, 2, 4, 6, 7, 80, 20, 1000, 40000, 'false'])
plot_multi_lines(input_folder, output_folder, bench_type, [base_serie3,series3,series32,series33], ['SP3', 'SP2', 'SP1'], dict3)

dict4=dict3
dict4['title']='High local contention, high remote contention'
[series4]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 2000, 'true', 'specula', 'new', 9])
[series42]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 2000, 'true', 'specula', 'old', 9])
[series43]=get_matching_series([input_folder, 'micro', 1, 2, 4, 6, 7, 9, 13, 80, 20, 1000, 2000, 'true', 'nospecula', 'old', 9])
[base_serie4]=get_matching_serie([input_folder, 'micro', 0, 1, 2, 4, 6, 7, 8, 80, 20, 1000, 2000, 'false'])
plot_multi_lines(input_folder, output_folder, bench_type, [base_serie4,series4,series42,series43], ['SP3', 'SP2', 'SP1'], dict4)

#dict5['abort_legend'] = ['No spec: UC', 'SP1: UC', 'SP1: AC', 'SP2: UC'],
dict5=copy.deepcopy(dict4)
dict5['speedup']=True
dict5['title']='Remote read'
dict5['noabort']=True
dict5['y_lim']=7
dict5['legend_type']='remote read'
dict5['y_labels']= 'Speedup'
dict5['x_labels']= 'Speculation length'
dict5['commit_legend']=['80:20:0', '80:19:1', '80:15:5', '80:10:10', '80:0:20', '50:0:50']
dict5.pop('base_line')
#types=['80:20:0', '80:19:1', '80:15:5', '80:10:10', '80:0:20', '50:0:50']
colors=['#397fb8', '#397fb8', '#397fb8', '#397fb8', '#397fb8', '#397fb8']
hatches=['xxx', '///', '...', '\\\\\\', '+++', '---', '|||']
series5=get_matching_series_delete([input_folder, 'micro', 1, 4, 6, 80, 20000, 40000, 3],  [(7, 'true'), (9, 'nospecula')], {'order':'ascend'})
#series5=series5[0:1]+series5[2:]
series52=get_matching_series_delete([input_folder2, 'micro', 4, 6, 20000, 40000, 3],  [(7, 'true'), (9, 'nospecula')], {'order':'ascend'})
#series52=[series52[0]]
#plot_bars([input_folder, input_folder2], output_folder, series5+series52, [], types, 'remote_read', colors, hatches)
#plot_m_lines([input_folder, input_folder2], output_folder, bench_type, series5+series52, 3, dict5)


dict6=copy.deepcopy(dict4)
dict6['commit_legend']=['80:20:0', '70:30:0', '50:50:0', '20:80:0']
dict6['has_legend'] = True
dict6['title']='Number of updated servers'
#dict6['type']='commit'
#dict6.pop('type')
dict6['base_line']=False
dict6['not_reverse']=True
#dict6.pop('not_reverse')
dict6['y_lim']=5
dict6['y_ticks']=True
dict6['swap']=True
dict6['legend_type']='locality'
dict6['y_labels']= 'Transaction latency (ms)'
dict6.pop('abort_legend', None)
#dict6['y_labels']= 'Throughput'
series61=get_matching_series_delete([input_folder, 'micro', 3, 4, 6, 12, 13, 0, 20000, 40000, 'false', 'new', 2], [(7,'true'), (9, 'nospecula')], {'order':'ascend'})
series62=get_matching_series_delete([input_folder, 'micro', 0, 3, 4, 6, 12, 13, 8, 0, 40000, 60000, 'false', 'new', 2], [], {'order':'ascend'})
base_serie62=get_matching_serie([input_folder, 'micro', 0, 3, 4, 6, 7, 12, 13, 8, 0, 40000, 60000, 'false', 'false', 'old'])
base_serie62=sort_by_num(base_serie62)
base_serie62.reverse()
for i, s in enumerate(series62):
    s.insert(0, base_serie62[i])
series6 = series61+series62
#plot_multi_lines(input_folder, output_folder, bench_type, series6, 2, dict6)
plot_multi_lines(input_folder, output_folder, bench_type, series6, 2, dict6)
