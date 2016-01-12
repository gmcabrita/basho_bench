#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys
import os
import numpy as np

# input data
input_folder = sys.argv[1]
output_folder = sys.argv[2]
length = (len(sys.argv) -3) // 2 
print(length)
firstlist = sys.argv[3:3+length]
secondlist = sys.argv[3+length:3+2*length]
tc = []
ta = []
sc = []
sa = []
for f in firstlist:
    path = os.path.join(input_folder, f+'/total_throughput')
    data = np.loadtxt(path, skiprows=1, usecols=range(1,7))
    print(data)
    tc.append(data[0, 0])
    ta.append(data[0, 5])
    sc.append(data[1, 0])
    sa.append(data[1, 5])
print(tc)
print(sc)
plt.figure()
plt.errorbar(range(1, length+1),tc, sc)
plt.errorbar(range(1, length+1),ta, sa)
plt.ylim([0,20])
plt.grid(True)
plt.savefig(output_folder+'/haha.png')
exit()
