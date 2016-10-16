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
    elif line[0]=='A':
        return ''
    elif line[0]=='t':
        return ''
    elif line[0]=='N':
        return ''
    elif ',' in line:
        return ''
    else:
        return int(line[:-1])/1000
    
def init_dict(folder):
    dict = {}
    dict['total_throughput'] = [] 
    dict['total_duration'] = [] 
    dict['percv_latency'] = []
    dict['final_latency'] = []
    dict['files'] = []
    dict['nodes'] = {}
    nodefiles = glob.glob(folder+"/summary.csv-*")
    for f in nodefiles:
        node = f.split('/')[-1].split('-')[-1] 
        #print(node)
        dict['nodes'][node] = {}
    return dict

def init_node_data(nodes):
    dict = {}
    for node in nodes:
        dict[node] = []
    return dict

def add_throughput(my_dict, my_file):
    committed = 0
    immediate_abort = 0
    specula_abort = 0
    all_abort = 0
    duration=0
    
    with open(my_file) as f:
        for i, l in enumerate(f):
            pass
    line_num = i + 1
    if line_num > 15:
        SKIP_FIRST=10
    else:
        SKIP_FIRST=2
    SKIP_LAST=1
    
    with open(my_file) as stream:
        oldlines = stream.read().splitlines() 
        lines = oldlines[SKIP_FIRST+1:-SKIP_LAST-1] ##Skip the header line        
        if lines == []:
            lines = oldlines[SKIP_FIRST:-SKIP_LAST] ##Skip the header line        

        for line in lines:
            nums = line.split(",")
            #print words[3]+words[4]
            committed += int(nums[3])
            immediate_abort += int(nums[4])
            specula_abort += int(nums[5])
            all_abort = immediate_abort + specula_abort
            duration += 10

        my_dict.append([committed/duration, all_abort/duration, immediate_abort/duration, specula_abort/duration, all_abort/(committed+all_abort), specula_abort/(committed+all_abort)])
    

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

def add_real_latency(tag, total_entry, node_entry, folder):
    lat_files = glob.glob(folder+'/'+tag+"*") 
    total_sum = 0
    total_cnt = 0
    for file in lat_files:
        tmp_sum = 0
        tmp_cnt = 0
        with open(file) as f:
            for line in f:
                lat= parse_line(line)
                if lat != '':
                    tmp_sum += lat
                    tmp_cnt += 1
        #node = file.split('/')[-1].split('-')[-1] 
        #node_entry[node][tag] = tmp_sum / max(1, tmp_cnt) 
        total_sum += tmp_sum
        total_cnt += tmp_cnt
    total_entry.append([total_sum/max(1, total_cnt)])

def write_to_file(file_name, dict, keys, title):
    file = open(file_name, 'w')
    file.write(title+'\n')
    for key in keys:
        if key in dict:
            data_list = dict[key]
            data_array = np.array(data_list).astype(np.float)
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
        print(input_folder)
        
        config_path = os.path.join(input_folder, "config")
        o = open(config_path)
        config = o.read()
        config = config[:-1]
        config = config.replace(' ', '_')
        if config not in dict:
            print(config)
            dict[config] = init_dict(input_folder)

        add_throughput(dict[config]['total_throughput'], os.path.join(input_folder, 'specula_out'))
        #for node in dict[config]['nodes']: 
        #    dict[config]['nodes'][node]['throughput'] = []
        #    add_throughput(dict[config]['nodes'][node]['throughput'], os.path.join(input_folder, 'summary.csv-'+node))
        add_real_latency('percv_latency', dict[config]['percv_latency'], dict[config]['nodes'], input_folder)
        add_real_latency('final_latency', dict[config]['final_latency'], dict[config]['nodes'], input_folder)
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
    nums_file = config_folder +'/' + str(len(files))
    file = open(nums_file, 'w')
    for f in files:
        file.write(f+'\n')
    file.close()

    throughput = os.path.join(config_folder, 'throughput')
    total_throughput = os.path.join(config_folder, 'total_throughput')
    real_latency = os.path.join(config_folder, 'real_latency')

    if len(entry['final_latency']) != 0:
        write_to_file(real_latency, entry, ['percv_latency', 'final_latency'], 'percvlat finallat') 
        write_std(real_latency, entry['percv_latency'])
        write_std(real_latency, entry['final_latency'])

    write_to_file(total_throughput, entry, ['total_throughput'], 'N/A committed all_abort immediate_abort specula_abort abort_rate specula_abort_rate') 
    write_std(total_throughput, entry['total_throughput'])
    #node_file = open(output_fold+'/'+config+'/node_info', 'w')
    #node_file.write('nodes committed all_abort immediate_abort specula_abort abort_rate specula_abort_rate percv_lat real_lat\n')
    #for node in entry['nodes']:
    #    #print(entry['nodes'][node])
    #    throughput = entry['nodes'][node]['throughput']
    #    percv_lat = entry['nodes'][node]['percv_latency']
    #    final_lat = entry['nodes'][node]['final_latency']
    #    node_file.write(node+' '+' '.join(map(str, throughput))+' '+str(percv_lat)+' '+str(final_lat)+'\n')
    #node_file.close()
    
