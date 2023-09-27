def fnlist_make_fwork(fwork):
    res = fnlist_nil()
    
    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)
    
    fwork(work)
    
    # create an empty fnlist to hold the reversed list
    reversed_res = fnlist_nil()
    temp = res  # point a temporary variable at res

    # traverse through the original list and prepend each item to the reversed list
    while temp.ctag != 0:
        reversed_res = fnlist_cons(temp.cons1, reversed_res)
        temp = temp.cons2  # move to the next item in the original list
    
    return reversed_res
