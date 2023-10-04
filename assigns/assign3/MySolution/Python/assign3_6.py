import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

class MyList:
    pass

class Empty(MyList):
    def __repr__(self):
        return "[]"

class Cons(MyList):
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def __repr__(self):
        return f"{self.head} :: {repr(self.tail)}"

def mylist_nil():
    return Empty()

def mylist_cons(x, xs):
    return Cons(x, xs)

def mylist_snoc(xs, x):
    if isinstance(xs, Empty):
        return mylist_cons(x, xs)
    else:
        return mylist_cons(xs.head, mylist_snoc(xs.tail, x))

def mylist_append2(xs1, xs2):
    if isinstance(xs1, Empty):
        return xs2
    else:
        return mylist_cons(xs1.head, mylist_append2(xs1.tail, xs2))

def list_reverse(xs):
    return list_revapp(xs)

def list_revapp(xs, ys=Empty()):
    if isinstance(xs, Empty):
        return ys
    else:
        return list_revapp(xs.tail, mylist_cons(xs.head, ys))

def mylist_reverse(xs):
    return list_reverse(xs)

def mylist_foreach(xs, work):
    if isinstance(xs, Empty):
        return
    else:
        work(xs.head)
        mylist_foreach(xs.tail, work)

def mylist_rforeach(xs, work):
    reversed_list = mylist_reverse(xs)
    mylist_foreach(reversed_list, work)
