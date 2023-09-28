Benchmarking several langueges/tools with Mandelbrot set generation. No SIMD, no multithreading (except prange() trick with Numba), just 1-to-1 translation of code from one languages to another. No tricks (e.g. skipping sqrt, changing the algorythm) and hardcoer tailoring of code, just a minimum of language specific adjustment to make the code nicer while keeping all loops and operations in place.

By default using int32 and float64 (where possible, i.e. in Dart all integers are 64 bit). 

Sum check column shows the result of calculating the total sum of numbers in the array produced by `mandelbrot()` mthod. It demonstrates how there can be slight variations (+/-0,1%) in different languages and compiler settings dues to differences in floating point math utilized.

Ubuntu 22.04.3 LTS, 64 bit, Intel Core i5-8257U @ 1.4GHz x 2, VMWare Workstation Player 17.0.1

| Language/variant          | Time (seconds) | Version | Sum check | Comment       |
|---------------------------|----------------|---------|-----------|---------------|
| Python + NumPy            | 10,8           | 3.11.15 | 78513425  |               |
| Python + NP + Numba       | 0,68           | 3.11.15 | 78513425  |               |
| Python + Numba (fastmath) | 0,64           | 3.11.15 | 78513473  | Different sum |
| Python + Numba (prange)   | 0,38           | 3.11.15 | 78513425  | Parallel/MT   |
| Python + without NumPy    | 9,8            | 3.11.15 | 78513425  |               |
| Python + w/o NP + Numba   | 0,29           | 3.11.15 | 78513473  | Diff, fastmth |
| Python+w/o NP+Numba(prng) | 0,19           | 3.11.15 | 78513473  | Diff, fst, prl|
| JavaScript (Bun)          | 0,90           | 1.0.3   | 78513425  |               |
| JavaScript (Node)         | 0,82           | 12.22.9 | 78513425  |               |
| Go (go build)             | 0,69           | 1.21.1  | 78513478  | Different sum |
| Go (go run)               | 0,54           | 1.21.1  | 78513415  | Different sum |
| Dart (VM, JIT)            | 0,64           | 3.1.0   | 78513425  |               |
| Dart (AoT)                | 0,42           | 3.1.0   | 78513425  |               |
| Rust + num-complex        | 0,73           | 1.72.1  | 78513425  |               |
| Rust + custom Complex     | 0,32           | 1.72.1  | 78513425  |               |
| C# (JIT)                  | 0,37           | 7.0.111 | 78513425  |               |
| C# (AoT)                  | 0,33           | 7.0.111 | 78513425  |               |
| Mojo                      | 0,32           | 0.2.1   | 78513383  | Different sum |
| gcc                       | 0,69           | 11.4.0  | 78513478  | Different sum |
| gcc (-O3)                 | 0,32           | 11.4.0  | 78513478  | Different sum |
| gcc (-Ofast)              | 0,29           | 11.4.0  | 78513478  | Different sum |
