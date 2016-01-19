#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
import os
import numpy as np

def get_same(list1, list2):
    index=0
    same=[]
    for e in list1:
        if e == list2[index]:
            same.append(index)  
        index+=1
    return same

def get_title(param_list):
    split_f=[]
    params=[' threads','% m','% s', ' do_repl', ' fast_reply', ' length', ' Ws']
    for f in param_list:
        f=f.replace('false','f').replace('true','t')
        split_f.append(f.split("_"))
    common=split_f[0]
    old_same=set(range(0, len(params)))
    for f in split_f:
        new_same=get_same(f, common)
        old_same=set(old_same) & set(new_same)
    old_same=list(old_same)
    new_split_f=[]
    for f in split_f:
        new_name=[i for j, i in enumerate(f) if j not in new_same]
        name_str=' '.join(map(str, new_name))
        new_split_f.append(name_str)

    title=""
    for f in old_same:
        title+=str(split_f[0][f])+str(params[f])+", "
    return title[:-2], new_split_f
