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

nospeculanodes = load_nodes('/Users/liz/Documents/MyDocument/repositories/basho_bench2/specula_tests/cdf/after_paolo/newnodes')
allnodes = load_nodes('/Users/liz/Documents/MyDocument/repositories/basho_bench2/specula_tests/cdf/after_paolo/newnewnodes')
folder1=['./specula_tests/cdf/after_paolo/2016-05-21-163506']
folder2=['./specula_tests/cdf/after_paolo/2016-05-21-163935']

#pslen1=['./specula_tests/cdf/after_paolo/2016-05-20-224814']
#pslen2=['./specula_tests/cdf/after_paolo/2016-05-20-225619']
#pslen3=['./specula_tests/cdf/after_paolo/2016-05-20-230422']
#pslen4=['./specula_tests/cdf/after_paolo/2016-05-20-231227']

#pslen11=['./specula_tests/cdf/after_paolo/2016-05-20-225210']
#pslen21=['./specula_tests/cdf/after_paolo/2016-05-20-230014']
#pslen31=['./specula_tests/cdf/after_paolo/2016-05-20-230818']
#pslen41=['./specula_tests/cdf/after_paolo/2016-05-20-231623']
Fold1=glob.glob('./specula_tests/cdf/after_paolo/lowlow/*') 
Fold2=glob.glob('./specula_tests/cdf/after_paolo/highhigh/*')
spf1 = []
spf2 = []
for f in Fold1:
    spf1.append([f]) 
for f in Fold2:
    spf2.append([f]) 

print(spf1)
plot_cdf(spf1, folder1, 5, './figures/cdf/', 'lowlow', allnodes, nospeculanodes, True, True, True)
plot_cdf(spf2, folder2, 5, './figures/cdf/', 'highhigh', allnodes, nospeculanodes, False, False, False)
#plot_cdf(lowhighsp, lowhighnosp, 5, './figures/cdf/', 'lowhigh', allnodes, allnodes)
#plot_cdf(highlowsp, highlownosp, 5, './figures/cdf/', 'highlow', allnodes, allnodes)

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

