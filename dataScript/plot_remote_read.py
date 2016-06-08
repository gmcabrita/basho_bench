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

input_folder1='./stat/2016-03-06-232257/'
input_folder2='./stat/2016-05-19-112828/'
output_folder='./figures/final_micro/new_remote/'
bench_type='micro'

dict1={'title':'Remote read', 'no_title':True, 'x_labels':False, 'y_labels':'Transactions per second(tps)', 'big':True, 'y_lim':5000, 'legend_type':'remote read'}
series1=get_matching_series_delete([input_folder1, 'micro', 1, 4, 6, 80, 20000, 40000, 3],  [(7, 'true'), (9, 'nospecula')], {'order':'ascend'})
series2=get_matching_series_delete([input_folder2, 'micro', 4, 6, 20000, 40000, 3],  [(7, 'true'), (9, 'nospecula')], {'order':'ascend'})
plot_multi_lines([input_folder1,input_folder2], output_folder, bench_type, series1+series2, 3, dict1)
