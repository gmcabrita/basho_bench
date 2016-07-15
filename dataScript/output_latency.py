#!/usr/bin/env python

import matplotlib.pyplot as plt
import sys
import glob
import os
import matplotlib.gridspec as gridspec
from os.path import basename
import numpy as np
from time import gmtime, strftime
from get_data import get_data 
from helper import get_matching_series

input_folder='./latency/'
dcs=['IE', 'VA', 'TYO', 'FRA', 'CA', 'SG', 'OR', 'SY', 'SU'] 
latency_arr=[[] for i in range(9)]
for i in range(9):
    for j in range(8):
        NIndex = (i+9-j-1) % 9
        latency_arr[i].append((0, 0, dcs[NIndex]))

for i in range(9):
    base=i*3
    myregion = dcs[i]
    for j in range(3):
        [f] = glob.glob(input_folder+'/'+str(base+j)+'summary-*')
        with open(f) as fc:
            cnt=0
            for line in fc:
                (num, c, region) = latency_arr[(i+cnt+1)%9][cnt]
                if region != myregion:
                    print("Wrong!!!"+region+" "+myregion)
                else:
                    latency_arr[(i+cnt+1)%9][cnt] = (float(line)+num, c+1, region)
                    cnt+=1

replica_latency =[[] for i in range(9)]
for i in range(9):
    for j in range(8):
        (num, c, region) = latency_arr[i][j]
        latency_arr[i][j] = (num/c, region)
    replica_latency[i]=latency_arr[i]#[0:5]
        
for i in range(9):
    replica_latency[i].sort()
    print(dcs[i]+" "+str(replica_latency[i]))
