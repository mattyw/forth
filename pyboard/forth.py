# Trivial Python Forth.
# Based on http://www.openbookproject.net/py4fun
import pyb
import time


def main():
        led3 = pyb.LED(3)
        led3.on()  # So we know the script is loaded
        led = pyb.LED(2)
        evalled = pyb.LED(4)
        usb = pyb.USB_VCP()
        while True:
            led.on()
            time.sleep_ms(500)
            line = usb.readline()
            if line is None:
                continue
            led.off()
            #evalled.on()
            usb.send(str(ev(line)) + "\n", timeout=5000)
            #evalled.off()
            time.sleep_ms(500)


def ev(line):
    return "hello" + line
    #pcode = compile(line)
    #if pcode is None:
    #    return "not compiled"
    #return execute([], pcode)


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


main()
