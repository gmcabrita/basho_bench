#!/usr/bin/env python

from pylab import *
import sys
import csv
import random
import os
import numpy as np
import csv
from helper import get_legend

# input data
def get_data(file):
    with open(file) as f:
        cnt = 0
        sum = 0
        for line in f:
            if 'swpd' in line or 'memory' in line: 
                pass
            else:
                words = line.split(' ')
                words = filter(None, words)
                cnt += 1
                sum += int(words[-3])
        print(100-sum/cnt)


get_data('specula_tests/icde/10-06/2016-10-07-163319/cpu_info')
get_data('specula_tests/icde/10-06/2016-10-07-163553/cpu_info')
get_data('specula_tests/icde/10-06/2016-10-07-163828/cpu_info')
get_data('specula_tests/icde/10-06/2016-10-07-164104/cpu_info')
