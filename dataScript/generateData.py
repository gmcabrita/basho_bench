#!/usr/bin/env python

###!/usr/bin/env python3
from __future__ import division
import sys
import os
import glob
import numpy as np
from operator import add
from os import rename, listdir
from time import gmtime, strftime

def parse_line(line):
    if line[0]=='[':
        arr=line.split('[')[1]
        arr=arr.split(',')
        return int(arr[-1][:-2])/1000
    elif line[0]=='a':
        return ''
    elif line[0]=='t':
        return ''
    elif line[0]=='N':
        return ''
    else:
        return int(line[:-1])/1000
    
def init_dict():
    dict = {}
    dict['total_throughput'] = [] 
    dict['total_duration'] = [] 
    dict['percv_latency'] = []
    dict['final_latency'] = []
    dict['files'] = []
    return dict

def init_node_data(nodes):
    dict = {}
    for node in nodes:
        dict[node] = []
    return dict

def add_throughput(total_dict, folder):
    committed = 0
    immediate_abort = 0
    specula_abort = 0
    all_abort = 0
    duration=0
    
    throughput_file = os.path.join(folder, 'specula_out')
    SKIP_FIRST=2
    SKIP_LAST=2
    print(throughput_file)
    
    with open(throughput_file) as stream:
        lines = stream.read().splitlines() 
        lines = lines[SKIP_FIRST+1:-SKIP_LAST-1] ##Skip the header line        

        for line in lines:
            print(line)
            nums = line.split(",")
            #print words[3]+words[4]
            committed += int(nums[3])
            immediate_abort += int(nums[4])
            specula_abort += int(nums[5])
            all_abort = immediate_abort + specula_abort
            duration += 10

        total_dict.append([committed/duration, all_abort/duration, immediate_abort/duration, specula_abort/duration, all_abort/(committed+all_abort), specula_abort/(committed+all_abort)])
    

def update_counter(folder, length, key):
    if length == 1:
    	length_file = os.path.join(config_folder, '1')
    	file = open(length_file, 'w')
    	file.write(key+'\n')
    	file.close()
    else:
    	length_file = os.path.join(config_folder, str(length-1))
    	file = open(length_file, 'a')
    	file.write(key+'\n')
    	file.close()

    rename(length_file, os.path.join(config_folder, str(length)))

def add_real_latency(tag, list, folder):
    lat_files = glob.glob(folder+'/'+tag+"*") 
    tmp_list = []
    for file in lat_files:
        with open(file) as f:
            for line in f:
                lat= parse_line(line)
                if lat != '':
                    #list.append(lat)
                    tmp_list.append(lat)
    list.append([sum(tmp_list)/max(1, len(tmp_list))])

def write_to_file(file_name, dict, keys, title):
    file = open(file_name, 'w')
    file.write(title+'\n')
    for key in keys:
        if key in dict:
            data_list = dict[key]
            data_array = np.array(data_list).astype(np.float)
            #print(data_list)
            #print(data_array)
            if data_array.ndim == 2:
                data_avg = list(np.average(data_array, axis=0))
            else:
                data_avg = list(data_array)
            file.write(key+' '+' '.join(map(str, data_avg))+'\n')
    file.close()

def write_std(file_name, data_list):
    file = open(file_name, 'a')
    data_array = np.array(data_list).astype(np.float)
    if data_array.ndim == 2:
        data_std = list(np.std(data_array, axis=0))
    else:
        data_std = list(data_array)
    file.write('std '+' '.join(map(str, data_std))+'\n')
    file.close()
            
root = sys.argv[1]
output = sys.argv[2]
dict={}
sub_folders = glob.glob(root+'/20*') 
for f in sub_folders:
    input_folder = f #os.path.join(root, f)
    if os.path.isdir(input_folder):
        files = os.listdir(input_folder)
        
        config_path = os.path.join(input_folder, "config")
        o = open(config_path)
        config = o.read()
        config = config[:-1]
        config = config.replace(' ', '_')
        if config not in dict:
            print(config)
            dict[config] = init_dict()

        add_throughput(dict[config]['total_throughput'], input_folder)
        add_real_latency('percv_latency', dict[config]['percv_latency'], input_folder)
        add_real_latency('final_latency', dict[config]['final_latency'], input_folder)
        dict[config]['files'].append(input_folder)

time = strftime("%Y-%m-%d-%H%M%S", gmtime())
output_fold = os.path.join(output, time)
os.mkdir(output_fold)
for config in dict:
    print(config)
    entry = dict[config]
    config_folder = os.path.join(output_fold, config)
    os.mkdir(config_folder)
    files = entry['files']
    print(config_folder)
    print(files)
    print(len(files))
    nums_file = config_folder +'/' + str(len(files))
    file = open(nums_file, 'w')
    for f in files:
        file.write(f+'\n')
    file.close()

    throughput = os.path.join(config_folder, 'throughput')
    total_throughput = os.path.join(config_folder, 'total_throughput')
    real_latency = os.path.join(config_folder, 'real_latency')

    if len(entry['final_latency']) != 0:
        print(entry['percv_latency'])
        print(entry['final_latency'])
        write_to_file(real_latency, entry, ['percv_latency', 'final_latency'], 'percvlat finallat') 
        write_std(real_latency, entry['percv_latency'])
        write_std(real_latency, entry['final_latency'])

    write_to_file(total_throughput, entry, ['total_throughput'], 'N/A committed all_abort immediate_abort specula_abort abort_rate specula_abort_rate') 
    write_std(total_throughput, entry['total_throughput'])
    
