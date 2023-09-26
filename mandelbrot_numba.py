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

# njit - 0.68 sec
# njit(fastmath=True) - 0.64 sec

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

@numba.njit(fastmath=True)
def mandelbrot():
    output = np.empty((height, width), dtype=np.int32)
    for h in range(height):
        cy = min_y + h * scaley
        for w in range(width):
            cx = min_x + w * scalex
            output[h,w] = mandelbrot_0(complex(cx,cy))
    return output


for i in range(4):
    print(i+1, end=' ', flush=True)
    start_time = time.time()
    mandelbrot()
    end_time = time.time()
    execution_time = end_time - start_time
    print("Execution Time:", execution_time)