import serial

ser = serial.Serial("/dev/ttyACM0", 9600, timeout=1)
while True:
    line = raw_input("Forth:> ")
    if line == ":q":
        break
    ser.write(line.encode() + "\r")
    print ser.readall()
ser.close()
