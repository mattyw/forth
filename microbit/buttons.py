from microbit import *

zero = Image("00000:00000:00000:00000:00000")
one = Image("50000:00000:00000:00000:00000")
two = Image("55000:00000:00000:00000:00000")
three = Image("55500:00000:00000:00000:00000")
four = Image("55550:00000:00000:00000:00000")
five = Image("55555:00000:00000:00000:00000")

options = [zero, one, two, three, four, five]

count = 0
while True:
    if button_a.is_pressed():
        count = count + 1
    if button_b.is_pressed():
        count = 0
    else:
        display.show(options[count])
    sleep(200)
