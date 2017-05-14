# Trivial Python Forth.
# Based on http://www.openbookproject.net/py4fun
# http://python.microbit.org/editor.html#
from microbit import *


def ev(stack, line):
    pcode = []
    try:
        pcode = compile(line)
    except Exception as e:
        return "error: %s %s" % (e, line)
    if pcode is None:
        return "not compiled"
    return execute(stack, pcode)


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
    print(ds.pop())
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


def stack_to_string(stack):
    return ",".join(map(str, stack))


def serial_mode():
    while True:
        uart.init()
        display.scroll(".")
        if uart.any():
            line = uart.readline().strip()
            resp = ev([], str(line, "utf-8"))
            uart.write("%s\n" % (resp))
        sleep(2000)


class Display:
    def __init__(self):
        self.ipt = "00000"
        self.out = "00000"

    def input(self, value):
        self.ipt = "{0:05b}".format(value)

    def output(self, value):
        self.out = "{0:05b}".format(value)

    def img(self):
        return Image("%s:%s:00000:00000:00000" % (self.ipt, self.out))


def button_mode():
    leds = Display()
    stack = [0]

    count = 0
    while True:
        if button_a.is_pressed():
            count = count + 1
            leds.input(count)
        if button_b.is_pressed():
            stack = ev(stack, "%s +" % (str(count)))
            count = 0
            leds.input(count)
            top = 0
            if len(stack) > 0:
                top = stack[0]
            leds.output(top)
        else:
            display.show(leds.img())
        sleep(200)

button_mode()
