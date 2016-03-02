#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import glob
import os
from os.path import basename
import numpy as np
from time import gmtime, strftime
from plot_line import plot_multi_lines
from helper import get_matching_series



th_folder='./figures/2016-02-01-0518434_10_11_1'

# input data
input_folder = sys.argv[1]
bench_type = sys.argv[2]
output_root = './figures'
length = len(sys.argv)
field_len = int((length-6)/2)
field_end = 3 + field_len
value_begin = field_end
value_end = value_begin + field_len
diff_fields = sys.argv[3:field_end]
field_values = sys.argv[value_begin:value_end]
rotate_field = sys.argv[value_end]
diff_fields.append(rotate_field)
title=sys.argv[value_end+1]
legend_text=sys.argv[value_end+2]

to_plot_list = get_matching_series(sys.argv[1:-2])

d={'legend_type':legend_text, 'title':title, 'y_labels':'Throughput (tps)', 'x_labels': 'Speculation length', 'y_lim':False}
plot_multi_lines(input_folder, th_folder, bench_type, to_plot_list, rotate_field, d)

#os.system('./dataScript/plot_line.py %s %s %s %s' % (input_folder, th_folder, bench_type, to_plot_list))

#' '.join(map(str, flist))))
#str_series = ' '.join(map(str, series))
#for config in series:
#    print(config)
#    config_path = os.path.join(input_folder, config)
#    print(config_path)
#    os.system('./dataScript/plot_node_lat.py %s %s' % (config_path+'/', output_folder)) 
