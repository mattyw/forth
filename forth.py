# Trivial Python Forth.
# Based on http://www.openbookproject.net/py4fun
import sys


def main():
    if len(sys.argv) > 0:
        ev_file(sys.argv[1])
        return
    while True:
        line = raw_input("Forth:> ")
        ev(line)


def ev(line):
    pcode = compile(line)
    if pcode is None:
        return
    execute([], pcode)


def ev_file(file):
    ds = []
    with open(file) as f:
        for line in f:
            ds = execute(ds, compile(line.strip()))


def rPush(ds, cod, p):
    ds.append(cod[p])
    return p + 1, ds


def rAdd(ds, cod, p):
    b = ds.pop()
    a = ds.pop()
    ds.append(a + b)
    return None, ds


def rMul(ds, cod, p):
    b = ds.pop()
    a = ds.pop()
    ds.append(a * b)
    return None, ds


def rSub(ds, cod, p):
    b = ds.pop()
    a = ds.pop()
    ds.append(a - b)
    return None, ds


def rDiv(ds, cod, p):
    b = ds.pop()
    a = ds.pop()
    ds.append(a / b)
    return None, ds


def rDot(ds, cod, p):
    print ds.pop()
    return None, ds


def rDup(ds, cod, p):
    ds.append(ds[-1])
    return None, ds

rDict = {
    "+": rAdd,
    "-": rSub,
    "*": rMul,
    "/": rDiv,
    ".": rDot,
    "dup": rDup,
    }


def compile(line):
    pcode = []
    words = line.split(" ")
    for word in words:
        rAct = rDict.get(word)
        if rAct:
            pcode.append(rAct)
            continue
        pcode.append(rPush)
        try:
            pcode.append(int(word))
        except:
            pcode.append(float(word))
    return pcode


def execute(ds, pcode):
    # print pcode
    p = 0
    while p < len(pcode):
        func = pcode[p]
        p += 1
        # print p, func, ds
        newP, newDS = func(ds, pcode, p)
        if newP is not None:
            p = newP
        if newDS is not None:
            ds = newDS
    return ds


main()
# ev("5 6 + 7 8 + * .")
# TODO Support equvalent in idris
# TODO Add support for reading from microbit
