#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import glob
import os
import matplotlib.gridspec as gridspec
from os.path import basename
import numpy as np
from time import gmtime, strftime
from get_data import get_data 
from helper import get_matching_series

input_folder='./stat/2016-02-01-110840/'
output_folder='./figures/2016-02-01-0518434_10_11_1/'
bench_type='tpcc'

dict3={'title':'80% new-order, 10% payment, 10%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':2000, 'legend_type':'warehouse'}
series3=get_matching_series(['./stat/2016-02-01-110840/', 'tpcc', 7, 8, 80, 10, 6])
get_data(input_folder, output_folder, bench_type, series3, 6, dict3)

dict4={'title':'10% new-order, 80% payment, 10%order-status', 'no_title':True, 'x_labels':False, 'y_labels':False, 'y_lim':2000, 'legend_type':'warehouse'}
series4=get_matching_series(['./stat/2016-02-01-110840/', 'tpcc', 7, 8, 10, 80, 6])
print("Outputing to "+output_folder)
get_data(input_folder, output_folder, bench_type, series4, 6, dict4)
