import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def fnlist_reverse(xs):
    res = fnlist_nil()
    iterator = fnlist_iter(xs)
    for x in iterator:
        res = fnlist_cons(x, res)
    return res

def fnlist_make_fwork(fwork):
    res = fnlist_nil()

    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)

    fwork(work)
    return fnlist_reverse(res)
