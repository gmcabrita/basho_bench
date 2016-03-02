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

input_folder='./stat/2016-02-29-111139/'
output_folder='./figures/new_repl/'
bench_type='tpcc'
dict1={'title':'100% new-order 4warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':600, 'legend_type':'thread', 'add_num':True}
series1=get_matching_series([input_folder, 'tpcc', 6, 7, 4, 100, 0])
plot_multi_lines(input_folder, output_folder, bench_type, series1, 0, dict1)

dict2={'title':'100% new-order 8warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':600, 'legend_type':'thread', 'add_num':True}
series2=get_matching_series([input_folder, 'tpcc', 6, 7, 8, 100, 0])
plot_multi_lines(input_folder, output_folder, bench_type, series2, 0, dict2)

dict3={'title':'100% payment 4warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':1000, 'legend_type':'thread', 'add_num':True}
series3=get_matching_series([input_folder, 'tpcc', 6, 8, 4, 100, 0])
plot_multi_lines(input_folder, output_folder, bench_type, series3, 0, dict3)

dict4={'title':'100% payment 8warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':1000, 'legend_type':'thread', 'add_num':True}
series4=get_matching_series([input_folder, 'tpcc', 6, 8, 8, 100, 0])
plot_multi_lines(input_folder, output_folder, bench_type, series4, 0, dict4)

dict5={'title':'100% order-status 4warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':3000, 'legend_type':'thread'}
series5=get_matching_series([input_folder, 'tpcc', 6, 7, 8, 4, 0, 0, 1])
plot_multi_lines(input_folder, output_folder, bench_type, series5, 1, dict5)

dict6={'title':'100% order-status 8warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':3000, 'legend_type':'thread'}
series6=get_matching_series([input_folder, 'tpcc', 6, 7, 8, 8, 0, 0, 1])
plot_multi_lines(input_folder, output_folder, bench_type, series6, 1, dict6)

#dict2={'title':'1% new-order, 9% payment, 90%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':7500, 'legend_type':'warehouse'}
#series2=get_matching_series([input_folder, 'tpcc', 7, 8, 1, 9, 6])
#plot_multi_lines(input_folder, output_folder, bench_type, series2, 6, dict2)

#dict3={'title':'80% new-order, 10% payment, 10%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':1400, 'legend_type':'warehouse'}
#series3=get_matching_series([input_folder, 'tpcc', 7, 8, 80, 10, 6])
#plot_multi_lines(input_folder, output_folder, bench_type, series3, 6, dict3)
