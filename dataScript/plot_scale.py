#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
from plot_bar import *
from itertools import chain
import os
import numpy as np
import pandas as pd

nospecula_list=['8_80_0_false_false_8_2_1_9_2', '8_80_0_false_false_8_2_9_1_2', '8_80_0_false_false_8_2_80_10_2', '8_80_0_false_false_8_2_10_80_2', '8_80_20_false_nospecula_8']
specula_list=['8_80_0_true_true_8_2_1_9_2', '8_80_0_true_true_8_2_9_1_2', '8_80_0_true_true_8_2_80_10_2', '8_80_0_true_true_8_2_10_80_2', '8_80_20_true_specula_8']

types=['3 near DCs', '3 far DCs', '6 DCs', '9DCs']
legends=['TpccA', 'TpccB', 'TpccC', 'TpccD']
input_folder2='./stat/2016-05-19-144137/'
input_folder1='./stat/2016-05-19-143013/'
output_folder='./figures/scale/'

plot_multi_bars([input_folder1, input_folder2], output_folder, specula_list, nospecula_list, legends, types, 'scale_dc')
