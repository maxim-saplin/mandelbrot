# MacBook M1 Pro
# 3.12.0 - ~45 sec, 78513425
# 3.11.6 - ~41 sec, 78513425
# 3.9.6  - ~67 sec, 78513425
# For comparison, Dart VM - ~0.47 sec, 78513425
#                 gcc     - ~0.25 sec, 78513381
#
# VMWare, Ubuntu, Intel® Core™ i5-8257U CPU @ 1.40GHz × 2
# 3.12.0  - ~54 sec, 78513425
# 3.11.6  - ~54 sec, 78513425
# 3.10.12 - ~85 sec, 78513425
# 3.9.18  - ~85 sec, 78513425
# For comparison, Dart VM - ~0.62 sec, 78513425
#                 gcc     - ~0.33 sec, 79394478


import time
from math import sqrt

height = 1024
width = 1024
min_x = -2.0
max_x = 0.47
min_y = -1.12
max_y = 1.12
scalex = (max_x - min_x) / width
scaley = (max_y - min_y) / height
MAX_ITERS = 256


class Complex:
    def __init__(self, real:float, imag:float):
        self.real = real
        self.imag = imag

    def __add__(self, other:float):
        return Complex(self.real + other.real, self.imag + other.imag)

    def __mul__(self, other:float):
        real = self.real * other.real - self.imag * other.imag
        imag = self.imag * other.real + self.real * other.imag
        return Complex(real, imag)

    def modulus(self):
        return sqrt(self.real ** 2 + self.imag ** 2)

def mandelbrot_0(c: True) -> int:
    z = Complex(0.0, 0.0)
    nv = 0
    for i in range(1, MAX_ITERS):
        zNew = z * z + c
        if zNew.modulus() > 2.0:
            break
        z = zNew
        nv += 1
    return nv

def mandelbrot():
    output = [0]*(height*width)
    for h in range(height):
        cy = min_y + h * scaley
        for w in range(width):
            cx = min_x + w * scalex
            output[h*width+w] = mandelbrot_0(Complex(cx, cy))
    return output


for i in range(4):
    print(i+1, end=' ', flush=True)
    start_time = time.time()
    result = mandelbrot()
    end_time = time.time()
    execution_time = end_time - start_time
    print("Execution Time:", execution_time, end=' ')

    sum_total = 0

    for h in range(height):
        for w in range(width):
            sum_total += result[h*width+w]

    print("                 ", sum_total)
