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

input_folder='./stat/2016-02-13-011127/'
output_folder='./figures/clock_tpcc1'
bench_type='tpcc'
dict1={'title':'100% new-order 4warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':500, 'legend_type':'clock'}
series1=get_matching_series([input_folder, 'tpcc', 6, 7, 4, 100, 10])
print(str(series1))
plot_multi_lines(input_folder, output_folder, bench_type, series1, 10, dict1)
dict2={'title':'100% new-order 8warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':500, 'legend_type':'clock'}
series2=get_matching_series([input_folder, 'tpcc', 6, 7, 8, 100, 10])
print(str(series2))
plot_multi_lines(input_folder, output_folder, bench_type, series2, 10, dict2)

dict3={'title':'100% payment 4warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':1000, 'legend_type':'clock'}
series3=get_matching_series([input_folder, 'tpcc', 6, 8, 4, 100, 10])
print(str(series3))
plot_multi_lines(input_folder, output_folder, bench_type, series3, 10, dict3)
dict4={'title':'100% payment 8warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':1000, 'legend_type':'clock'}
series4=get_matching_series([input_folder, 'tpcc', 6, 8, 8, 100, 10])
print(str(series4))
plot_multi_lines(input_folder, output_folder, bench_type, series4, 10, dict4)

dict5={'title':'100%order-status 4warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':3000, 'legend_type':'clock'}
series5=get_matching_series([input_folder, 'tpcc', 6, 7, 8, 4, 0, 0, 10])
plot_multi_lines(input_folder, output_folder, bench_type, series5, 10, dict5)
dict6={'title':'100%order-status 8warehouse', 'x_labels':False, 'y_labels':False, 'y_lim':3000, 'legend_type':'clock'}
series6=get_matching_series([input_folder, 'tpcc', 6, 7, 8, 8, 0, 0, 10])
plot_multi_lines(input_folder, output_folder, bench_type, series6, 10, dict6)
