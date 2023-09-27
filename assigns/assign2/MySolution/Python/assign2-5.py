def fnlist_reverse(xs):
    res = fnlist_nil()
    for x in xs:
        res = fnlist_cons(x, res.get_cons2())
    return res

def fnlist_make_fwork(fwork):
    res = fnlist_nil()

    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)

    fwork(work)
    return fnlist_reverse(res)