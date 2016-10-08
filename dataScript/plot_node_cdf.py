#!/usr/bin/env python

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
from plot_cdf import *
from itertools import chain
import os
import numpy as np
import pandas as pd

def list_folders(path):
    files=glob.glob(path+"/*")
    specula = []
    for f in  files[:-1]:
        specula.append([f])
    print(specula)
    nospecula = [files[-1]]
    return specula, nospecula

def find_files(root, config):
    files=glob.glob(root+"/*")
    found = []
    for f in files:
        if config in open('example.txt').read(): 
            pass

root='/Users/liz/Documents/MyDocument/repositories/basho_bench2/specula_tests/sigmod/all/'


#allnodes = load_nodes('/Users/liz/Documents/MyDocument/repositories/basho_bench2/specula_tests/sigmod/all/2016-07-20-192954/nodes')
allnodes=[['52.212.40.41', '184.73.37.119']]

list1=[['specula_tests/icde/10-06/2016-10-07-004244'], ['specula_tests/icde/10-06/2016-10-07-003735/'], ['specula_tests/icde/10-06/2016-10-07-003226/'], ['specula_tests/icde/10-06/2016-10-06-221332'], ['specula_tests/icde/10-06/2016-10-06-221844/']]
plot_cdf(list1, [], 5, './figures/eurosys/cdf', '111', allnodes, allnodes, True, True, True)

allnodes1=[['52.212.70.221', '54.158.143.253']]
list2=[['specula_tests/icde/10-06/2016-10-07-140227/'], ['specula_tests/icde/10-06/2016-10-07-140739/'], ['specula_tests/icde/10-06/2016-10-07-141252']]
plot_cdf(list2, [], 5, './figures/eurosys/cdf', '222', allnodes1, allnodes, True, True, True)

list3=[['specula_tests/icde/10-06/2016-10-06-222358/'], ['specula_tests/icde/10-06/2016-10-06-222913']]
plot_cdf(list3, [], 5, './figures/eurosys/cdf', '333', allnodes, allnodes, True, True, True)

exit()

slen1_f1=['./specula_tests/cdf/2016-05-18-202602', './specula_tests/cdf/2016-05-18-203011']
slen1_f2=['./specula_tests/cdf/2016-05-18-203437', './specula_tests/cdf/2016-05-18-203840']

slen2_f1=['./specula_tests/cdf/2016-05-18-204337', './specula_tests/cdf/2016-05-18-204741']
slen2_f2=['./specula_tests/cdf/2016-05-18-205207', './specula_tests/cdf/2016-05-18-205711']

slen4_f1=['./specula_tests/cdf/2016-05-18-120832', './specula_tests/cdf/2016-05-18-112232']
slen4_f2=['./specula_tests/cdf/2016-05-18-112613', './specula_tests/cdf/2016-05-18-112932']

slen8_f1=['./specula_tests/cdf/2016-05-18-210209', './specula_tests/cdf/2016-05-18-210618']
slen8_f2=['./specula_tests/cdf/2016-05-18-211043', './specula_tests/cdf/2016-05-18-211447']

nospecula_folders1=['./specula_tests/cdf/2016-05-18-125345', './specula_tests/cdf/2016-05-18-125707/']
#nospecula_folders1=['./specula_tests/cdf/2016-05-19-185121']
nospecula_folders2=['./specula_tests/cdf/2016-05-18-130538', './specula_tests/cdf/2016-05-18-131626/']

plot_cdf([slen1_f1, slen2_f1, slen4_f1, slen8_f1], nospecula_folders1, 4, './figures/cdf/', 'cdf_lowlow')
plot_cdf([slen1_f2, slen2_f2, slen4_f2, slen8_f2], nospecula_folders2, 4, './figures/cdf/', 'cdf_highlow')

