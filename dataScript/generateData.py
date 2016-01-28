#!/usr/bin/env python3
import sys
import os
import glob
import numpy as np
from operator import add
from os import rename, listdir
from time import gmtime, strftime

def get_nodes(str_list):
    nodes = []
    for file in str_list:
        if file[0].isdigit() and file[-1].isdigit() and len(file) >= 8:
            nodes.append(file)
    return nodes
    
def init_dict(nodes):
    dict = {}
    dict['throughput'] = init_node_data(nodes)
    dict['total_throughput'] = [] 
    dict['total_duration'] = [] 
    dict['duration'] = init_node_data(nodes)
    dict['new-order'] = [] #init_node_data(nodes)
    dict['payment'] = [] #init_node_data(nodes)
    dict['order-status'] = [] #init_node_data(nodes)
    return dict

def init_node_data(nodes):
    dict = {}
    for node in nodes:
        dict[node] = []
    return dict

def add_throughput(nodes, dict, total_dict, folder):
    all_committed = 0
    all_r_abort = 0
    all_r_invalid = 0
    all_cert_abort = 0
    all_c_abort = 0
    all_abort = 0
    for node in nodes:
        th_file = os.path.join(folder, node)
        th_lines = [line.rstrip('\n') for line in open(th_file)]
        th_lines = th_lines[1:]
        committed = 0
        aborted = 0
        for line in th_lines:
            words = line.split(",")
            #print words[3]+words[4]
            committed += int(words[3])
            aborted += int(words[4])
        stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
        [stat_line] = [x for x in stat_lines if x.startswith(node)]
        stat_data = stat_line.split()
        if len(stat_data) == 28:
            read_abort = int(stat_data[1])
            read_invalid = int(stat_data[3])
            cert_abort = int(stat_data[5])
            cascade_abort = int(stat_data[7])
            if int(stat_data[9]) == 0:
                # This is specula!
                real_committed = committed
                cert_abort = aborted
            else:
                real_committed = int(stat_data[9])
        elif len(stat_data) == 26:
            read_abort = int(stat_data[1])
            read_invalid = 0 
            cert_abort = int(stat_data[3])
            cascade_abort = int(stat_data[5])
            if int(stat_data[7]) == 0:
                # This is specula!
                real_committed = committed
                cert_abort = aborted
            else:
                real_committed = int(stat_data[7])
        else:
            print("***WTF, data dimenstion is wrong!!!***")
        
        #print str(committed)+' '+str(aborted) +' '+str(read_abort)+' '+str(specula_abort)+' '+str(cascade_abort) 
        dict[node].append([real_committed, cert_abort, read_abort, read_invalid, cascade_abort])
        all_committed += real_committed
        all_cert_abort += cert_abort
        all_r_abort += read_abort
        all_r_invalid += read_invalid
        all_c_abort += cascade_abort

    total_dict.append([all_committed/60, all_cert_abort/60, all_r_abort/60, all_r_invalid/60, all_c_abort/60])
    

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


def add_duration(nodes, dict, total_dict, folder):
    total_dur = [0, 0, 0, 0, 0, 0, 0, 0, 0]
    for node in nodes:
        dur_file = os.path.join(folder, node+'-prep')
        if os.path.isfile(dur_file) == False:
            total_dict.append(total_dur)
            return 
        data = np.loadtxt(dur_file)
        if data.ndim == 2:
            #(lines, num_elem) = data.shape
            #dur_sum = data.sum(axis=0)
            #dur_avg = [x/lines for x in dur_sum]
	        dur_avg = list(np.average(data, axis=0))
        else:
            dur_avg = list(data)
        # convert to ms
        dur_avg = [x/1000 for x in dur_avg]
        #print dur_avg
        stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
        [stat_line] = [x for x in stat_lines if x.startswith(node)]
        stat_data = stat_line.split()
        local_cert_dur = float(stat_data[21])/1000
        specula_abort_dur = float(stat_data[23])/1000 
        specula_commit_dur = float(stat_data[25])/1000
        dur_avg.append(local_cert_dur), 
        dur_avg.append(specula_abort_dur), 
        dur_avg.append(specula_commit_dur)
        dict[node].append(dur_avg)
        total_dur = list(map(add, total_dur, dur_avg))
    total_avg_dur = [x/len(nodes) for x in total_dur]
    total_dict.append(total_avg_dur)
        
def add_latency(nodes, tag, dict, folder):
    total_latency=[0,0,0,0,0,0,0]
    for node in nodes:
        lat_file = os.path.join(folder, node+'-'+tag+'_latencies.csv')
        if os.path.isfile(lat_file) == False:
            dict.append(total_latency)
            return
        data = np.loadtxt(lat_file, delimiter=',', skiprows=1, usecols=range(3,10))
        #print data 
        #(lines, num_elem) = data.shape
        #lat_sum = data.sum(axis=0)
        #lat_avg = [x/lines for x in lat_sum]
        latency_avg = list(np.average(data, axis=0))
        total_latency = list(map(add, total_latency, latency_avg))

    total_latency = [x/len(nodes) for x in total_latency]
    dict.append(total_latency)

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
            print(config)
            o = os.path.join(output_fold, config)
            os.mkdir(o)
            dict[config] = init_dict(nodes)

        print("Folder: "+f)
        add_throughput(nodes, dict[config]['throughput'], dict[config]['total_throughput'], input_folder)
        add_duration(nodes, dict[config]['duration'], dict[config]['total_duration'], input_folder)
        add_latency(nodes, 'new-order', dict[config]['new-order'], input_folder)
        add_latency(nodes, 'payment', dict[config]['payment'], input_folder)
        add_latency(nodes, 'order-status', dict[config]['order-status'], input_folder)
        config_folder = os.path.join(output_fold, config)
        update_counter(config_folder, len(dict[config]['total_throughput']), f)

for config in dict:
    entry = dict[config]
    config_folder = os.path.join(output_fold, config)
    throughput = os.path.join(config_folder, 'throughput')
    total_throughput = os.path.join(config_folder, 'total_throughput')
    duration = os.path.join(config_folder, 'duration')
    total_duration = os.path.join(config_folder, 'total_duration')

    new_order_latency = os.path.join(config_folder, 'new-order-latency')
    payment_latency = os.path.join(config_folder, 'payment-latency')
    order_status_latency = os.path.join(config_folder, 'order-status-latency')
    write_to_file(new_order_latency, entry, ['new-order'], 'N/A min mean median 95th 99th 99_9th max')
    write_to_file(payment_latency, entry, ['payment'], 'N/A min mean median 95th 99th 99_9th max')
    write_to_file(order_status_latency, entry, ['order-status'], 'N/A min mean median 95th 99th 99_9th max')

    write_to_file(throughput, entry['throughput'], nodes, 'ip committed cert_aborted read_aborted read_invalid cascade_abort') 
    write_to_file(total_throughput, entry, ['total_throughput'], 'N/A committed cert_aborted read_aborted read_invalid cascade_abort') 
    write_std(total_throughput, entry['total_throughput'])
    write_to_file(duration, entry['duration'], nodes, 'ip read local_a remote_a local_c remote_c local_cert specula_c s_final_a s_final_c')
    write_to_file(total_duration, entry, ['total_duration'], 'N/A read local_a remote_a local_c remote_c local_cert specula_c s_final_a s_final_c')
    write_std(total_duration, entry['total_duration'])
    
