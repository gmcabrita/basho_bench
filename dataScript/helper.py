#!/usr/bin/env python3

import matplotlib.pyplot as plt
from pylab import *
import sys
import random
from itertools import chain
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
    params=[' threads','% m','% s', 'do_specula', 'fast_reply', ' length', ' Ws', '% NewOrder', '% Payment', 'rep']
    name_params=['T','%M','%S', 'Spec', 'Fast', 'SL', 'W', '%N', '%P', 'Rep']
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
        new_name=[(j, i) for j, i in enumerate(f) if j not in old_same]
        new_str_name=""
        for (i, p) in new_name:
            if i == 3:
                if p=='f':
                    new_str_name+='nosp'
                else:
                    new_str_name+='sp'
            elif i == 4:
                new_str_name=new_str_name
            else:
                new_str_name+=str(p)+str(name_params[i])
        new_split_f.append(new_str_name)

    title=""
    for f in old_same:
        if f == 3:
            if split_f[0][f]=='f':
                title+='no_spec, '
            else:
                title+='spec, '
        elif f == 4:
            title=title
        else:
            title+=str(split_f[0][f])+str(params[f])+", "
    return title[:-2], new_split_f
