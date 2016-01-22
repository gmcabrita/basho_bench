#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import os
import numpy as np
from time import gmtime, strftime

# input data
input_folder = sys.argv[1]
output_root = './figures'
fields = sys.argv[2:]
time = strftime("%Y-%m-%d-%H%M%S", gmtime())
output_folder = os.path.join(output_root, time)
print('output folder is'+output_folder)
os.mkdir(output_folder)
str_series = ' '.join(map(str, series))
os.system('./dataScript/plot_lat.py %s %s %s' % (input_folder, output_folder, str_series))
os.system('./dataScript/plot_bar_th.py %s %s %s' % (input_folder, output_folder, str_series))
for config in series:
    print(config)
    config_path = os.path.join(input_folder, config)
    print(config_path)
    os.system('./dataScript/plot_node_lat.py %s %s' % (config_path+'/', output_folder))
    
