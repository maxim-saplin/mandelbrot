# VMWare, Ubuntu, Intel® Core™ i5-8257U CPU @ 1.40GHz × 2
# Pure Python - 9,7 sec, sum 78513425
# njit - 0,33 sec, sum 78513425
# njit(fastmath=True) - 0,29 sec, sum 78513441
# njit(fastmath=True), parallel - 0,19 sec, sum 78513441
# Numba, skipping 1st iteration in eval (cold JIT start)

import time
from math import sqrt
import numba as numba

height = 1024
width = 1024
min_x = -2.0
max_x = 0.47
min_y = -1.12
max_y = 1.12
scalex = (max_x - min_x) / width
scaley = (max_y - min_y) / height
MAX_ITERS = 256

@numba.njit(fastmath=True)
def mandelbrot_0(cRe, cIm) -> int:
    zRe = 0.0
    zIm = 0.0
    nv = 0
    for i in range(1, MAX_ITERS):
        zReNew = zRe * zRe - zIm * zIm + cRe
        zImNew = 2.0 * zRe * zIm + cIm
        if sqrt(zReNew * zReNew + zImNew * zImNew) > 2.0:
            break
        zRe = zReNew
        zIm = zImNew
        nv += 1
    return nv

@numba.njit(fastmath=False, parallel=True)
def mandelbrot():
    output = [0]*(height*width)
    for h in numba.prange(height):
        cy = min_y + h * scaley
        for w in numba.prange(width):
            cx = min_x + w * scalex
            output[h*width+w] = mandelbrot_0(cx,cy)
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
    