#!/usr/bin/env python

###!/usr/bin/env python3
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
    else:
        return int(line[:-1])/1000

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
    dict['-latency_percv'] = []
    dict['-latency_final'] = []
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
    all_notified_abort = 0
    all_r_abort = 0
    all_r_invalid = 0
    all_cert_abort = 0
    all_c_abort = 0
    all_abort = 0
    index = 1
    stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
    for node in nodes:
        th_file = os.path.join(folder, node)
        th_lines = [line.rstrip('\n') for line in open(th_file)]
        th_lines = th_lines[1:]
        committed = 0
        notified_abort = 0
        duration=0
        for line in th_lines:
            words = line.split(",")
            #print words[3]+words[4]
            committed += int(words[3])
            notified_abort += int(words[4])
            duration=max(duration,float(words[0]))
            #print("Duration is"+str(int(duration)))
        stat_line = stat_lines[index]
        stat_data = stat_line.split(',')
        if len(stat_data) == 20 or len(stat_data) == 7:
            read_abort = int(stat_data[0])
            read_invalid = int(stat_data[1])
            cert_abort = int(stat_data[2])
            cascade_abort = int(stat_data[3])
            if int(stat_data[4]) == 0:
                # This is not specula!
                real_committed = committed
                cert_abort = notified_abort
            else:
                real_committed = int(stat_data[4])
        #elif len(stat_data) == 26:
        #    read_abort = int(stat_data[1])
        #    read_invalid = 0 
        #    cert_abort = int(stat_data[3])
        #    cascade_abort = int(stat_data[5])
        #    if int(stat_data[7]) == 0:
        #        # This is specula!
        #        real_committed = committed
        #        cert_abort = aborted
        #    else:
        #        real_committed = int(stat_data[7])
        else:
            print("***WTF, data dimenstion is wrong!!!***")
            print("Stat data is "+str(stat_data))
        index += 1 
        
        #print str(committed)+' '+str(aborted) +' '+str(read_abort)+' '+str(specula_abort)+' '+str(cascade_abort) 
        if node in dict:
            dict[node].append([real_committed, cert_abort, read_abort, read_invalid, cascade_abort, notified_abort])
        all_committed += real_committed
        all_cert_abort += cert_abort
        all_r_abort += read_abort
        all_r_invalid += read_invalid
        all_c_abort += cascade_abort
        all_notified_abort += notified_abort
        all_abort += cert_abort + read_abort + read_invalid + cascade_abort
    print('All committed is'+str(real_committed))
    total_dict.append([all_committed/duration, all_cert_abort/duration, all_r_abort/duration, all_r_invalid/duration, all_c_abort/duration, all_abort/duration, all_notified_abort/duration, (all_abort-all_notified_abort)/duration])
    

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
    #total_dur = [0, 0, 0, 0, 0, 0, 0, 0]
    #print("**************** Add totoal duration***************")
    total_dur = []
    index = 1
    stat_lines = [line.rstrip('\n') for line in open(os.path.join(folder, 'stat'))]
        
    for node in nodes:
        dur_file = os.path.join(folder, node+'-prep')
        if os.path.isfile(dur_file) == False:
            print(dur_file+"IS NOT FILE")
            print("ADD NOTHING!!!")
            #total_dict.append(total_dur)
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
        #print("Dua avg is "+str(dur_avg))
        #print dur_avg
        stat_line = stat_lines[index] #[x for x in stat_lines if x.startswith(node)]
        stat_data = stat_line.split(',')
        #local_cert_dur = float(stat_data[21])/1000
        #dur_avg.append(float(stat_data[8])/1000) 
        #dur_avg.append(float(stat_data[9])/1000)
        #dur_avg.append(float(stat_data[10])/1000)
        #dur_avg.append(float(stat_data[11])/1000)
        #dur_avg.append(float(stat_data[12])/1000) 
        #dur_avg.append(float(stat_data[13])/1000)
        #dur_avg.append(float(stat_data[14])/1000)
        #dur_avg.append(float(stat_data[15])/1000)
        #dur_avg.append(float(stat_data[16])/1000) 
        #dur_avg.append(float(stat_data[17])/1000)
        #dur_avg.append(float(stat_data[18])/1000)
        #dur_avg.append(float(stat_data[19])/1000)
        if node in dict:
            dict[node].append(dur_avg)
        if total_dur == []:
            total_dur = dur_avg[:]
        else:
            total_dur = list(map(add, total_dur, dur_avg))
        index += 1
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
        latency_avg = list(np.average(data, axis=0))
        total_latency = list(map(add, total_latency, latency_avg))

    total_latency = [x/len(nodes) for x in total_latency]
    dict.append(total_latency)

def add_real_latency(tag, list, folder):
    lat_files = glob.glob(folder+'/*'+tag) 
    for file in lat_files:
        with open(file) as f:
            for line in f:
                lat= parse_line(line)
                if lat != '':
                    list.append(lat)

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
        add_real_latency('-latency_percv', dict[config]['-latency_percv'], input_folder)
        add_real_latency('-latency_final', dict[config]['-latency_final'], input_folder)
        config_folder = os.path.join(output_fold, config)
        update_counter(config_folder, len(dict[config]['total_throughput']), f)

for config in dict:
    print(config)
    entry = dict[config]
    config_folder = os.path.join(output_fold, config)
    throughput = os.path.join(config_folder, 'throughput')
    total_throughput = os.path.join(config_folder, 'total_throughput')
    duration = os.path.join(config_folder, 'duration')
    total_duration = os.path.join(config_folder, 'total_duration')
    real_latency = os.path.join(config_folder, 'real_latency')

    new_order_latency = os.path.join(config_folder, 'new-order-latency')
    payment_latency = os.path.join(config_folder, 'payment-latency')
    order_status_latency = os.path.join(config_folder, 'order-status-latency')
    write_to_file(new_order_latency, entry, ['new-order'], 'N/A min mean median 95th 99th 99_9th max')
    write_to_file(payment_latency, entry, ['payment'], 'N/A min mean median 95th 99th 99_9th max')
    write_to_file(order_status_latency, entry, ['order-status'], 'N/A min mean median 95th 99th 99_9th max')

    write_to_file(throughput, entry['throughput'], nodes, 'ip committed cert_aborted read_aborted read_invalid cascade_abort notified_abort') 

    if len(entry['-latency_final']) != 0:
        entry['-latency_percv'] = [sum(entry['-latency_percv'])/max(len(entry['-latency_percv']), 1)]
        entry['-latency_final'] = [sum(entry['-latency_final'])/len(entry['-latency_final'])]
        write_to_file(real_latency, entry, ['-latency_percv', '-latency_final'], 'percvlat finallat') 

    write_to_file(total_throughput, entry, ['total_throughput'], 'N/A committed cert_aborted read_aborted read_invalid cascade_abort all_abort notified_abort specula_abort') 
    write_std(total_throughput, entry['total_throughput'])
    write_to_file(duration, entry['duration'], nodes, 'ip no_read no_local_a no_remote_a no_local_c no_remote_c no_specula_c p_read p_local_a p_remote_a p_local_c p_remote_c p_specula_c nc_local nc_remote na_local na_remote pc_local pc_remote pa_local pa_remote gc_local gc_remote ga_local ga_remote')
    write_to_file(total_duration, entry, ['total_duration'], 'N/A ip no_read no_local_a no_remote_a no_local_c no_remote_c no_specula_c p_read p_local_a p_remote_a p_local_c p_remote_c p_specula_c nc_local nc_remote na_local na_remote pc_local pc_remote pa_local pa_remote gc_local gc_remote ga_local ga_remote') 
    write_std(total_duration, entry['total_duration'])
    
