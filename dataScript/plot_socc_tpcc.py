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
from helper import get_matching_series

#input_folder='./stat/2016-03-07-092625/'
input_folder='./stat/2016-05-20-232311/'
output_folder='./figures/final_tpcc/socc/'
bench_type='tpcc'
dict1={'title':'10% new-order, 80% payment, 10% order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':5.5, 'legend_type':'warehouse', 'legend_loc':'upper center', 'commit_legend':['2W: commits', '4W: commits'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC'], 'has_legend':True}#, 'base_line':True}
series1=get_matching_series([input_folder, 'tpcc', 0, 7, 8, 10, 6])
print(series1)
exit()
plot_multi_lines(input_folder, output_folder, bench_type, series1, 6, dict1)

dict3={'title':'80% new-order, 10% payment, 10% order-status','no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':2, 'legend_type':'warehouse', 'legend_loc':2, 'commit_legend':['2W: commits', '4W: commits'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC'], 'has_legend':True}#, 'base_line':True}
series3=get_matching_series([input_folder, 'tpcc', 0, 8, 8, 10, 6])
print(series3)
plot_multi_lines(input_folder, output_folder, bench_type, series3, 6, dict3)

dict5={'title':'1% new-order, 9% payment, 90% order-status','no_title':True, 'x_labels':False, 'y_labels':'Thousand txs/sec', 'y_lim':18, 'legend_type':'warehouse', 'legend_loc':2,  'commit_legend':['2W: commits', '4W: commits'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC'],'has_legend':True}#, 'base_line':True}
series5=get_matching_series([input_folder, 'tpcc', 0, 7, 8, 8, 1, 9, 6])
print(series5)
plot_multi_lines(input_folder, output_folder, bench_type, series5, 6, dict5)

dict7={'title':'9% new-order, 1% payment, 90% order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':15, 'legend_type':'warehouse', 'legend_loc':2,  'commit_legend':['2W: commits', '4W: commits'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC'], 'has_legend':True}#, 'base_line':True}
series7=get_matching_series([input_folder, 'tpcc', 0, 7, 8, 8, 9, 1, 6])
print(series7)
plot_multi_lines(input_folder, output_folder, bench_type, series7, 6, dict7)

exit()

dict8={'title':'(e)25% new-order, 25% payment, 50% order-status', 'x_labels':False, 'y_labels':False, 'y_lim':6000, 'legend_type':'warehouse', 'draw_line':2, 'line_name':'max',  'commit_legend':['2W: commits', '4W: commits'], 'abort_legend':['2W: UC', '2W: AC', '4W: UC', '4W: AC']}
series8=get_matching_series([input_folder, 'tpcc', 7, 8, 25, 25, 6])
print(series8)
plot_multi_lines(input_folder, output_folder, bench_type, series8, 6, dict8)

#dict2={'title':'1% new-order, 9% payment, 90%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':7500, 'legend_type':'warehouse', 'add_num':'true'}
#series2=get_matching_series([input_folder, 'tpcc', 7, 8, 1, 9, 6])
#plot_multi_lines(input_folder, output_folder, bench_type, series2, 6, dict2)

#dict3={'title':'80% new-order, 10% payment, 10%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':1400, 'legend_type':'warehouse', 'add_num':'true'}
#series3=get_matching_series([input_folder, 'tpcc', 7, 8, 80, 10, 6])
#plot_multi_lines(input_folder, output_folder, bench_type, series3, 6, dict3)
