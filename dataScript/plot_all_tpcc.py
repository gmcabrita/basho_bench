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

input_folder='./stat/2016-03-07-092625/'
output_folder='./figures/final_tpcc_retry/'
bench_type='tpcc'
dict1={'title':'10% new-order, 80% payment, 10% order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':9000, 'legend_type':'warehouse', 'draw_line':2, 'line_name':'max'}
series1=get_matching_series([input_folder, 'tpcc', 7, 10, 6])
print(series1)
plot_multi_lines(input_folder, output_folder, bench_type, series1, 6, dict1)

dict3={'title':'80% new-order, 10% payment, 10% order-status','no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':4000, 'legend_type':'warehouse', 'draw_line':2, 'line_name':'max'}
series3=get_matching_series([input_folder, 'tpcc', 8, 10, 6])
print(series3)
plot_multi_lines(input_folder, output_folder, bench_type, series3, 6, dict3)

dict5={'title':'1% new-order, 9% payment, 90% order-status','no_title':True, 'x_labels':False, 'y_labels':'Transactions per second(tps)', 'y_lim':15000, 'legend_type':'warehouse', 'draw_line':2, 'line_name':'max'}
series5=get_matching_series([input_folder, 'tpcc', 7, 8, 1, 9, 6])
print(series5)
plot_multi_lines(input_folder, output_folder, bench_type, series5, 6, dict5)

dict7={'title':'9% new-order, 1% payment, 90% order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':13000, 'legend_type':'warehouse','draw_line':2, 'line_name':'max'}
series7=get_matching_series([input_folder, 'tpcc', 7, 8, 9, 1, 6])
print(series7)
plot_multi_lines(input_folder, output_folder, bench_type, series7, 6, dict7)

dict8={'title':'(e)25% new-order, 25% payment, 50% order-status', 'x_labels':False, 'y_labels':False, 'y_lim':6000, 'legend_type':'warehouse', 'draw_line':2, 'line_name':'max'}
series8=get_matching_series([input_folder, 'tpcc', 7, 8, 25, 25, 6])
print(series8)
plot_multi_lines(input_folder, output_folder, bench_type, series8, 6, dict8)

#dict2={'title':'1% new-order, 9% payment, 90%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':7500, 'legend_type':'warehouse', 'add_num':'true'}
#series2=get_matching_series([input_folder, 'tpcc', 7, 8, 1, 9, 6])
#plot_multi_lines(input_folder, output_folder, bench_type, series2, 6, dict2)

#dict3={'title':'80% new-order, 10% payment, 10%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':1400, 'legend_type':'warehouse', 'add_num':'true'}
#series3=get_matching_series([input_folder, 'tpcc', 7, 8, 80, 10, 6])
#plot_multi_lines(input_folder, output_folder, bench_type, series3, 6, dict3)
