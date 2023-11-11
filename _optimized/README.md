Sqeezing every bit of performance. Algo and precision can be changed, the check SUM must not be out +/-1% range from 78513425, the produced result must be a valid mabdelbrot picture (use plot.py to verify), you can't pre-calculate the values (store ready results and output them as-is).

When measuring the implementation 11 runs of `mandelbrot` subroutine are conducted, the 1st run is a warm-up one and not taken into consideration. The code is started 3-5 times, should there be significant deviations from run-to-run 2nd best result is taken.

Test enviroment: Ubuntu 22.04.3 LTS, 64 bit, Intel Core i5-8257U @ 1.4GHz x 2, VMWare Workstation Player 17.0.1

| Language/variant                            | Current                         | Original         |
|---------------------------------------------|---------------------------------|------------------|
| C (SIMD + 32 bit + pipelining)              | Avg: 11.39ms, StdDev: 40.57%    | 0,29 sec         |
| Dart (Mirroring + Isolates + Known regions + Spare Indexes) | Avg: 14.2ms, StdDev: 21.2%    | 0,42 sec         |
| C# (SIMD + 32 bit float + Mirroring )       | Avg: 23.63ms, StdDev: 39.50%    | 0,33 sec         |


<center>
<img width="650" alt="image" src="https://github.com/maxim-saplin/mandelbrot/assets/7947027/a7cbc30e-3d13-4006-bb4b-610c68f69d2d">
</center>
