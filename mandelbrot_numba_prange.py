# VMWare, Ubuntu, Intel® Core™ i5-8257U CPU @ 1.40GHz × 2
# njit - 0.68 sec, sum_result 78513425
# njit(fastmath=True) - 0.64 sec, sum_result 78513473
# njit(fastmath=False, parallel=True), prange() - 0,38 sec, sum_result  78513425
# njit(fastmath=True, parallel=True), prange() - 0,38 sec, sum_result 78513473
  # M4 Pro - 0.023-0.024  
  # 13600KF OC - 0.045-0.049

import time
import numpy as np
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
def mandelbrot_0(c: complex) -> int:
    z = c
    nv:int = 0
    for i in range(1, MAX_ITERS):
      if abs(z) > 2:
        break
      z = z*z + c
      nv += 1
    return nv

@numba.njit(fastmath=True, parallel=True)
def mandelbrot():
    output = np.empty((height, width), dtype=np.int32)
    for h in numba.prange(height):
        cy = min_y + h * scaley
        for w in numba.prange(width):
            cx = min_x + w * scalex
            output[h,w] = mandelbrot_0(complex(cx,cy))
    return output


for i in range(4):
    print(i+1, end=' ', flush=True)
    start_time = time.time()
    result = mandelbrot()
    end_time = time.time()
    execution_time = end_time - start_time
    print("Execution Time:", execution_time, end=' ')

    sum_result = np.sum(result)
    print("                 ", sum_result)