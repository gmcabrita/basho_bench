#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import glob
import os
import matplotlib.gridspec as gridspec
from os.path import basename
import numpy as np
from time import gmtime, strftime

file='./include/default_transitions.txt'
data = np.genfromtxt(file, delimiter='\t', skip_header=4, skip_footer=17, usecols=range(1,28))
prob_list=[[] for i in range(len(data[0]))]
total_prob=[0] * len(data[0])

full_dist=[0] * len(data)
full_dist[0]=1

for i,d in enumerate(data):
    for j,p in enumerate(d):
        total_prob[j] += p 
        prob_list[j].append(p)

for i, prob in enumerate(prob_list):
    last = prob[-1]
    prob[0] += last
    prob_list[i] = prob[:-1]

for i, prob in enumerate(prob_list):
    for j in range(len(prob)):
        prob_list[i][j] = prob_list[i][j] / total_prob[i]

header = np.genfromtxt(file, delimiter='\t', dtype=None, skip_header=4, skip_footer=17, usecols=range(0,1))
print(header)

old=[0] * len(data)
new=full_dist
for i in range(100):
    old=[0] * len(data)
    for j, prob in enumerate(prob_list):
        for k in range(len(prob)):
            old[k] += new[j] * prob[k]
    std = np.std(np.array(old)- np.array(new))
    #print("Std is "+str(std))
    new = old[:]
    old = [0] * len(data)
    s = sum(new)
    for i, v in enumerate(new):
        new[i] = v/s 


for i, title in enumerate(header):
    print(str(title)+" "+str(new[i]))


