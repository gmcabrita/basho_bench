#!/usr/bin/env python
import sys
import os
import numpy as np
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
    dict['duration'] = init_node_data(nodes)
    dict['latency'] = init_node_data(nodes)
    return dict

def init_node_data(nodes):
    dict = {}
    for node in nodes:
        dict[node] = []
    return dict

def add_throughput(nodes, dict, folder):
    for node in nodes:
        th_file = os.path.join(folder, node)
        th_lines = [line.rstrip('\n') for line in open(th_file)]
        th_lines = th_lines[1:-1]
        print th_lines
        committed = 0
        aborted = 0
        for line in th_lines:
            words = line.split(",")
            print words[3]+words[4]
            committed += int(words[3])
            aborted += int(words[4])
        stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
        [stat_line] = [x for x in stat_lines if x.startswith(node)]
        stat_data = stat_line.split()
        read_abort = stat_data[1]
        specula_abort = stat_data[3]
        cascade_abort = stat_data[5]
        print str(committed)+' '+str(aborted) +' '+read_abort+' '+specula_abort+' '+cascade_abort 
        real_committed = committed - int(read_abort) - int(specula_abort) - int(cascade_abort)
        print real_committed 
        dict[node].append([real_committed, read_abort,
                          specula_abort, cascade_abort, aborted])


def add_duration(nodes, dict, folder):
    for node in nodes:
        dur_file = os.path.join(folder, node+'-prep')
        data = np.loadtxt(dur_file)
        if data.ndim == 2:
            (lines, num_elem) = data.shape
            dur_sum = data.sum(axis=0)
            dur_avg = [x/lines for x in dur_sum]
        else:
            dur_avg = list(data)
        print dur_avg
        stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
        [stat_line] = [x for x in stat_lines if x.startswith(node)]
        stat_data = stat_line.split()
        print stat_data
        specula_abort_dur = stat_data[21] 
        specula_commit_dur = stat_data[23]
        dur_avg.append(specula_abort_dur), 
        dur_avg.append(specula_commit_dur)
        dict[node].append(dur_avg)
        
def add_latency(nodes, dict, folder):
    for node in nodes:
        lat_file = os.path.join(folder, node+'-new-order_latencies.csv')
        data = np.loadtxt(lat_file, delimiter=',', skiprows=1)
        print data 
        (lines, num_elem) = data.shape
        lat_sum = data.sum(axis=0)
        lat_avg = [x/lines for x in lat_sum]
        dict[node].append(lat_avg)

def write_to_file(file_name, dict, nodes):
    file = open(file_name, 'w')
    for node in nodes:
        data_list = dict[node]
        data_array = np.array(data_list).astype(np.float)
        if data_array.ndim == 2:
            (lines, num_elem) = data_array.shape
            data_sum = data_array.sum(axis=0)
            data_avg = [x/lines for x in data_sum]
        else:
            data_avg = list(data_avg)
        file.write(node+':'+str(data_avg))
    file.close()
            
root = sys.argv[1]
output = sys.argv[2]
time = strftime("%Y-%m-%d-%H%M%S", gmtime())
output_fold = os.path.join(output, time)
os.mkdir(output_fold)
dict={}
sub_folders = os.listdir(root)
for f in sub_folders:
    input_folder = os.path.join(root, f)
    if os.path.isdir(input_folder):
        files = os.listdir(input_folder)
        nodes = get_nodes(files)
        print 'nodes are %s' % (nodes)
        
        config_path = os.path.join(input_folder, "config")
        o = open(config_path)
        config = o.read()
        config = config[:-1]
        config = config.replace(' ', '_')
        if config not in dict:
            print config
            o = os.path.join(output_fold, config)
            os.mkdir(o)
            dict[config] = init_dict(nodes)

        add_throughput(nodes, dict[config]['throughput'], input_folder)
        print dict
        add_duration(nodes, dict[config]['duration'], input_folder)
        print dict
        add_latency(nodes, dict[config]['latency'], input_folder)
        print dict

for config in dict:
    entry = dict[config]
    config_folder = os.path.join(output_fold, config)
    throughput = os.path.join(config_folder, 'throughput')
    duration = os.path.join(config_folder, 'duration')
    latency = os.path.join(config_folder, 'latency')
    write_to_file(throughput, entry['throughput'], nodes)
    write_to_file(duration, entry['duration'], nodes)
    write_to_file(latency, entry['latency'], nodes)

