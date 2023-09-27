Ubuntu 22.04.3 LTS, 64 bit, Intel Core i5-8257U @ 1.4GHz x 2, VMWare Workstation Player 17.0.1

| Language/variant          | Time (seconds) | Version | Sum check | Comment       |
|---------------------------|----------------|---------|-----------|---------------|
| Python                    | 10,8           | 3.11.15 | 78513425  |               |
| Python + Numba            | 0,68           | 3.11.15 | 78513425  |               |
| Python + Numba (fastmath) | 0,64           | 3.11.15 | 78513473  | Different sum |
| Python + Numba (prange)   | 0,38           | 3.11.15 | 78513425  | Parallel/MT   |
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
