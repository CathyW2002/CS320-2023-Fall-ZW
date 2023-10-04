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


# Define the list_foreach function
def list_foreach(xs, work):
    if isinstance(xs, Empty):
        return
    elif isinstance(xs, Cons):
        work(xs.head)
        list_foreach(xs.tail, work)


# Define the list_reverse helper functions and list_reverse itself
def list_revapp(xs, ys=Empty()):
    if isinstance(xs, Empty):
        return ys
    elif isinstance(xs, Cons):
        return list_revapp(xs.tail, Cons(xs.head, ys))

def list_reverse(xs):
    return list_revapp(xs)


# Define the list_rforeach function
def list_rforeach(xs, work):
    list_foreach(list_reverse(xs), work)
