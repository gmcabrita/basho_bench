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

def rreplace(s, old, new, occurrence):
    li = s.rsplit(old, occurrence)
    return new.join(li)

def get_title(param_list, type):
    if type == 'tpcc':
        return get_tpcc_title(param_list)
    elif type == 'micro':
        return get_micro_title(param_list)

def get_tpcc_title(param_list):
    split_f=[]
    params=[' threads','% m','% s', 'do_specula', 'fast_reply', ' length', ' Ws', '% NewOrder', '% Payment', 'rep']
    name_params=['T','%M','%S', 'Spec', 'Fast', 'SL', 'W', '%N', '%P', 'R']
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
        flag=False
        for (i, p) in new_name:
            if i == 3:
                if p=='f':
                    new_str_name+='nosp,'
                    flag=True
                else:
                    new_str_name+='sp,'
            elif i == 4:
                new_str_name=new_str_name
            elif i == 5:
                if flag == True: 
                    new_str_name=new_str_name
                else:
                    new_str_name+=str(p)+str(name_params[i])+','
            else:
                new_str_name+=str(p)+str(name_params[i])+','
        new_split_f.append(new_str_name[:-1])

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

def get_micro_title(param_list):
    split_f=[]
    params=[' threads',' mk',' sk',' ck',' mr', ' sr', ' cr', ' spec', ' spec length', '', 'rep']
    name_params=['T','MK','SK','CK','MR', 'SR', 'CR', 'SPEC', 'SL', 'ap', 'R']
    for f in param_list:
        f=f.replace('false','f').replace('true','t')
        f=rreplace(f, '000', 'k', 3)
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
        flag=False
        for (i, p) in new_name:
            if i == 7:
                if p=='f':
                    new_str_name+='nosp,'
                else:
                    new_str_name+='sp,'
            elif i == 8:
                if flag == True: 
                    new_str_name=new_str_name
                else:
                    new_str_name+=str(p)+str(name_params[i])+','
            else:
                new_str_name+=str(p)+str(name_params[i])+','
        new_split_f.append(new_str_name[:-1])

    title=""
    no_specula_flag=0
    for f in old_same:
        if f == 7:
            if split_f[0][f]=='f':
                title+='no_spec, '
                no_specula_flag=1
            else:
                title=title
        elif f == 8:
            if no_specula_flag==1:
                title=title
            else:
                title+=str(split_f[0][f])+str(params[f])+", "
        else:
            title+=str(split_f[0][f])+str(params[f])+", "
    return title[:-2], new_split_f
