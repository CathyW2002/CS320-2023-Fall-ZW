#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 27 16:50:42 2023

@author: cathy
"""

def string_length(s):
    return len(s)

def string_get_at(s, idx):
    return s[idx]

def int1_foreach(n, work_func):
    for i in range(n):
        work_func(i)

def string_make_fwork(foreach_func):
    res = []
    foreach_func(lambda c: res.append(c))
    return ''.join(res)

def string_merge(cs1, cs2):
    n1 = string_length(cs1)
    n2 = string_length(cs2)
    
    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                c1 = string_get_at(cs1, i1)
                c2 = string_get_at(cs2, i2)
                if c1 <= c2:
                    work(c1)
                    foreach(i1 + 1, i2, work)
                else:
                    work(c2)
                    foreach(i1, i2 + 1, work)
            else:
                int1_foreach(n1 - i1, lambda i: work(string_get_at(cs1, i1 + i)))
        else:
            int1_foreach(n2 - i2, lambda i: work(string_get_at(cs2, i2 + i)))
    
    return string_make_fwork(lambda work: foreach(0, 0, work))
