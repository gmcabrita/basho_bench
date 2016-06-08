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

nospecula_list=['8_80_0_false_false_8_2_1_9_*', '8_80_0_false_false_8_2_9_1_*', '8_80_0_false_false_8_2_10_80_*', '8_80_0_false_false_8_2_80_10_*', '8_80_20_false_nospecula_8*']
specula_list=['8_80_0_true_true_8_2_1_9_*', '8_80_0_true_true_8_2_9_1_*', '8_80_0_true_true_8_2_10_80_*', '8_80_0_true_true_8_2_80_10_*', '8_80_20_true_specula_8*']

types=['3 near DCs', '3 far DCs', '6 DCs', '9 DCs']
legends=['TPC-C PR', 'TPC-C NR', 'TPC-C PW', 'TPC-C NW', 'RUBiS']
#colors=['#397fb8', '#397fb8', '#397fb8', '#397fb8', '#ed7e7e']
colors=['#253494', '#253494', '#253494', '#253494', '#41b6c4']
hatches=['xxx', '///', '...', '\\\\\\', '']
input_folder1='./stat/2016-05-19-143013/'
input_folder2='./stat/2016-05-19-144137/'
input_folder3='./stat/2016-05-19-164817/'
input_folder4='./stat/2016-05-19-164726/'
output_folder='./figures/scale/'

size=(8, 5)
#output_label(legends, colors, hatches)
plot_multi_bars([input_folder1, input_folder2, input_folder3, input_folder4], output_folder, specula_list, nospecula_list, legends, types, 'scale_dc', colors, hatches, size)

types=['Rep 3', 'Rep 6', 'Rep 9']
input_folder1='./stat/2016-05-19-170012/'
input_folder2='./stat/2016-05-19-170019/'
input_folder3='./stat/2016-05-19-164726/'
plot_multi_bars([input_folder1, input_folder2, input_folder3], output_folder, specula_list, nospecula_list, legends, types, 'scale_rep', colors, hatches, size)
