import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def string_length(cs):
    return len(cs)  

def string_get_at(cs):
    def get_at(i):
        if i < 0 or i >= len(cs):
            raise IndexError
        return cs[i]
    return get_at

def string_tabulate(n, func):
    return ''.join(func(i) for i in range(n))

def string_fset_at(cs, i0, c0):
    return string_tabulate(string_length(cs), lambda i: c0 if i == i0 else cs[i])

def string_foreach(cs, func):
    for c in cs:
        func(c)


def string_fset_at(cs, i0, c0):
    return ''.join([c0 if i == i0 else cs[i] for i in range(len(cs))])

def list_of_buddies(word):
    
    result_list = []  # This list will collect the items.

    alphabet = ''.join([chr(ord('a') + i) for i in range(26)])

    def work(item):
        result_list.append(item)

    for i0 in range(len(word)):
        c0 = word[i0]
        for c1 in alphabet:
            if c1 != c0:
                work(string_fset_at(word, i0, c1))

    return result_list
