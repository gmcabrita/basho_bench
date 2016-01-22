#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import glob
import os
from os.path import basename
import numpy as np
from time import gmtime, strftime

# input data
input_folder = sys.argv[1]
output_root = './figures'
diff_fields = sys.argv[2:]
time = strftime("%Y-%m-%d-%H%M%S", gmtime())
name=time+'_'.join(map(str, diff_fields))
output_folder = os.path.join(output_root, name)
print('output folder is'+output_folder)
os.mkdir(output_folder)
sub_folders = glob.glob(input_folder+'/*')
split_names={}
for f in sub_folders:
    bname = basename(f)
    split_names[bname] = bname.split("_")

field_dict={}
for key,value in split_names.items():
    fields=""
    for f in diff_fields:
        fields+="_"+str(value[int(f)]) 
    if fields in field_dict:
        field_dict[fields].append(key) 
    else:
        field_dict[fields] = [key]

lat_folder=os.path.join(output_folder, "latency")
th_folder=os.path.join(output_folder, "throughput")
os.mkdir(lat_folder)
os.mkdir(th_folder)
for key, flist in field_dict.items():
    os.system('./dataScript/plot_lat.py %s %s %s' % (input_folder, lat_folder, ' '.join(map(str, flist))))
    os.system('./dataScript/plot_bar_th.py %s %s %s' % (input_folder, th_folder, ' '.join(map(str, flist))))

#str_series = ' '.join(map(str, series))
#for config in series:
#    print(config)
#    config_path = os.path.join(input_folder, config)
#    print(config_path)
#    os.system('./dataScript/plot_node_lat.py %s %s' % (config_path+'/', output_folder))
    
