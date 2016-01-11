#!/usr/bin/env python3
import sys
import os
import glob
import numpy as np
from os import rename, listdir
from time import gmtime, strftime

def get_nodes(str_list):
    nodes = []
    for file in str_list:
        if file[0].isdigit() and file[-1].isdigit():
            nodes.append(file)
    return nodes
    
def init_dict(nodes):
    dict = {}
    dict['throughput'] = init_node_data(nodes)
    dict['total_throughput'] = [] 
    dict['duration'] = init_node_data(nodes)
    dict['latency'] = init_node_data(nodes)
    return dict

def init_node_data(nodes):
    dict = {}
    for node in nodes:
        dict[node] = []
    return dict

def add_throughput(nodes, dict, total_dict, folder):
    all_committed = 0
    all_r_abort = 0
    all_s_abort = 0
    all_c_abort = 0
    all_abort = 0
    all_t_abort = 0
    for node in nodes:
        th_file = os.path.join(folder, node)
        th_lines = [line.rstrip('\n') for line in open(th_file)]
        th_lines = th_lines[1:-1]
        committed = 0
        aborted = 0
        for line in th_lines:
            words = line.split(",")
            #print words[3]+words[4]
            committed += int(words[3])
            aborted += int(words[4])
        stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
        [stat_line] = [x for x in stat_lines if x.startswith(node)]
	print stat_line
        stat_data = stat_line.split()
        read_abort = int(stat_data[1])
        specula_abort = int(stat_data[3])
        cascade_abort = int(stat_data[5])
	total_abort = aborted + int(read_abort) + int(specula_abort) + int(cascade_abort)
        #print str(committed)+' '+str(aborted) +' '+str(read_abort)+' '+str(specula_abort)+' '+str(cascade_abort) 
        real_committed = committed - read_abort - specula_abort - cascade_abort
        dict[node].append([real_committed, read_abort,
                          specula_abort, cascade_abort, aborted, total_abort])
	all_committed += real_committed
	all_r_abort += read_abort
	all_s_abort += specula_abort
	all_c_abort += cascade_abort
	all_abort += aborted
	all_t_abort += total_abort

    total_dict.append([all_committed, all_r_abort, all_s_abort, all_c_abort, all_abort, all_t_abort])
    

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


def add_duration(nodes, dict, folder):
    for node in nodes:
        dur_file = os.path.join(folder, node+'-prep')
        data = np.loadtxt(dur_file)
        if data.ndim == 2:
            #(lines, num_elem) = data.shape
            #dur_sum = data.sum(axis=0)
            #dur_avg = [x/lines for x in dur_sum]
	    dur_avg = list(np.average(data, axis=0))
        else:
            dur_avg = list(data)
        #print dur_avg
        stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
        [stat_line] = [x for x in stat_lines if x.startswith(node)]
        stat_data = stat_line.split()
        specula_abort_dur = stat_data[21] 
        specula_commit_dur = stat_data[23]
        dur_avg.append(specula_abort_dur), 
        dur_avg.append(specula_commit_dur)
        dict[node].append(dur_avg)
        
def add_latency(nodes, dict, folder):
    for node in nodes:
        lat_file = os.path.join(folder, node+'-new-order_latencies.csv')
        data = np.loadtxt(lat_file, delimiter=',', skiprows=1)
        #print data 
        (lines, num_elem) = data.shape
        lat_sum = data.sum(axis=0)
        lat_avg = [x/lines for x in lat_sum]
        dict[node].append(lat_avg[3:10])

def write_to_file(file_name, dict, keys, title):
    file = open(file_name, 'w')
    file.write(title+'\n')
    for key in keys:
        data_list = dict[key]
        data_array = np.array(data_list).astype(np.float)
        if data_array.ndim == 2:
            #(lines, num_elem) = data_array.shape
            #data_sum = data_array.sum(axis=0)
            #data_avg = [x/lines for x in data_sum]
	    data_avg = list(np.average(data_array, axis=0))
        else:
            data_avg = list(data_avg)
	if keys == ['total_throughput']:
	    data_avg = [x/60 for x in data_avg]
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
time = strftime("%Y-%m-%d-%H%M%S", gmtime())
output_fold = os.path.join(output, time)
os.mkdir(output_fold)
dict={}
sub_folders = glob.glob(root+'/20*') 
for f in sub_folders:
    input_folder = f #os.path.join(root, f)
    if os.path.isdir(input_folder):
        files = os.listdir(input_folder)
        nodes = get_nodes(files)
        #print 'nodes are %s' % (nodes)
        
        config_path = os.path.join(input_folder, "config")
        o = open(config_path)
        config = o.read()
        config = config[:-1]
        config = config.replace(' ', '_')
        if config not in dict:
            print config+', folder is:'+f
            o = os.path.join(output_fold, config)
            os.mkdir(o)
            dict[config] = init_dict(nodes)

        add_throughput(nodes, dict[config]['throughput'], dict[config]['total_throughput'], input_folder)
        add_duration(nodes, dict[config]['duration'], input_folder)
        add_latency(nodes, dict[config]['latency'], input_folder)
    	config_folder = os.path.join(output_fold, config)
	update_counter(config_folder, len(dict[config]['total_throughput']), f)

for config in dict:
    entry = dict[config]
    config_folder = os.path.join(output_fold, config)
    throughput = os.path.join(config_folder, 'throughput')
    total_throughput = os.path.join(config_folder, 'total_throughput')
    duration = os.path.join(config_folder, 'duration')
    latency = os.path.join(config_folder, 'latency')
    write_to_file(throughput, entry['throughput'], nodes, 'ip committed read_aborted specula_aborted cascade_abort normal_aborted total_aborted') 
    write_to_file(total_throughput, entry, ['total_throughput'], 'N/A committed read_aborted specula_aborted cascade_abort normal_aborted total_aborted') 
    write_std(total_throughput, entry['total_throughput'])
    write_to_file(duration, entry['duration'], nodes, 'ip read local_a remote_a local_c remote_c specula_c s_final_a s_final_c')
    write_to_file(latency, entry['latency'], nodes, 'ip min mean median 95th 99th 99_9th max')
    
