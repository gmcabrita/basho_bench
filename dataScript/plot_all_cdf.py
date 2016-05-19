#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
from plot_cdf import *
from itertools import chain
import os
import numpy as np
import pandas as pd

slen1_f1=['./specula_tests/cdf/2016-05-18-202602', './specula_tests/cdf/2016-05-18-203011']
slen1_f2=['./specula_tests/cdf/2016-05-18-203437', './specula_tests/cdf/2016-05-18-203840']

#nospecula_folders2=['./specula_tests/cdf/2016-05-18-130538', './specula_tests/cdf/2016-05-18-131626/']
#specula_folders3=['./specula_tests/cdf/2016-05-18-113632', './specula_tests/cdf/2016-05-18-113311']
#nospecula_folders3=['./specula_tests/cdf/2016-05-18-133116', './specula_tests/cdf/2016-05-18-133508/']
#specula_folders4=['./specula_tests/cdf/2016-05-18-114015', './specula_tests/cdf/2016-05-18-121212']
#nospecula_folders4=['./specula_tests/cdf/2016-05-18-133916', './specula_tests/cdf/2016-05-18-134304/']
#plot_cdf(specula_folders1, nospecula_folders1, '.', 'cdf_lowlow')
#plot_cdf(specula_folders2, nospecula_folders2, '.', 'cdf_highlow')
#plot_cdf(specula_folders3, nospecula_folders3, '.', 'cdf_lowhigh')
#plot_cdf(specula_folders4, nospecula_folders4, '.', 'cdf_highhigh')

slen2_f1=['./specula_tests/cdf/2016-05-18-204337', './specula_tests/cdf/2016-05-18-204741']
slen2_f2=['./specula_tests/cdf/2016-05-18-205207', './specula_tests/cdf/2016-05-18-205711']

slen4_f1=['./specula_tests/cdf/2016-05-18-120832', './specula_tests/cdf/2016-05-18-112232']
slen4_f2=['./specula_tests/cdf/2016-05-18-112613', './specula_tests/cdf/2016-05-18-112932']

slen8_f1=['./specula_tests/cdf/2016-05-18-210209', './specula_tests/cdf/2016-05-18-210618']
slen8_f2=['./specula_tests/cdf/2016-05-18-211043', './specula_tests/cdf/2016-05-18-211447']

nospecula_folders1=['./specula_tests/cdf/2016-05-18-125345', './specula_tests/cdf/2016-05-18-125707/']
nospecula_folders2=['./specula_tests/cdf/2016-05-18-130538', './specula_tests/cdf/2016-05-18-131626/']

plot_cdf([slen1_f1, slen2_f1, slen4_f1, slen8_f1], nospecula_folders1, 3.1, './specula_tests/cdf/all', 'cdf_lowlow')
plot_cdf([slen1_f2, slen2_f2, slen4_f2, slen8_f2], nospecula_folders1, 3.1, './specula_tests/cdf/all', 'cdf_highlow')

