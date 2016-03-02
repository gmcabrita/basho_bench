#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
from helper import *
import sys
import csv
import random
import os
import numpy as np
import csv
from helper import get_legend

# input data
def get_data(input_folder, output_folder, bench_type, data_multi_list, legend_index, plot_dict):
    pat='./data.csv'
    legend_type=plot_dict['legend_type']
    fieldnames = ['config', 'total_aborted','cert_aborted', 'read_aborted', 'read_invalid', 'cascade_abort']
    if os.path.isfile(pat):
        with open('./data.csv', 'a') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            for data_list in data_multi_list: 
                data_list.sort()
                index = 0 
                #xlabel.append(name)
                for f in data_list:
                    path = os.path.join(input_folder, f+'/total_throughput')
                    data = np.loadtxt(path, skiprows=1, usecols=range(1,7))
                    l = get_legend(f.split('_')[int(legend_index)], legend_type, '')
                    writer.writerow({'config': l+str(index*2), 'total_aborted': data[0,5], 'cert_aborted':data[0,1], 'read_aborted':data[0,2], 'read_invalid':data[0,3], 'cascade_abort':data[0,4]})
                    index += 1
    else:
        with open('./data.csv', 'x') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            for data_list in data_multi_list: 
                data_list.sort()
                index = 0 
                for f in data_list:
                    path = os.path.join(input_folder, f+'/total_throughput')
                    data = np.loadtxt(path, skiprows=1, usecols=range(1,7))
                    l = get_legend(f.split('_')[int(legend_index)], legend_type, '')
                    writer.writerow({'config': l+str(index*2), 'total_aborted': data[0,5], 'cert_aborted':data[0,1], 'read_aborted':data[0,2], 'read_invalid':data[0,3], 'cascade_abort':data[0,4]})
                    index += 1
