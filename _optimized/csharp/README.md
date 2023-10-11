
V4, unroll, fma, pipelining
AoT: Avg: 23.63ms, StdDev: 39.50%

V3, AVX512 and further optimisation
AoT: Avg: 26.50ms, StdDev: 15.37%, 78563750

Fine tuned and optimized versions of algos, not apple-to-apple comparisons


V2 (mirroring, improved SIMD), sum 78391100:
JIT, 2 CPU: Avg: 47.80ms, StdDev: 24.27%
JIT, 1 CPU: Avg: 40.80ms, StdDev: 30.60%
AOT, 2 CPU: Avg: 44.76ms, StdDev: 89.10%
AOT, 1 CPU: Avg: 33.14ms, StdDev: 4.05%

/M1 Pro:
JIT, 10CPU: Avg: 26.68ms, StdDev: 69.02%
JIT, 1CPU: Avg: 38.41ms, StdDev: 0.38%
AoT, 10CPU: Avg: 26.24ms, StdDev: 76.43%
AoT, 8CPU: Avg: 26.00ms, StdDev: 78.56%
AoT, 1CPU: Avg: 38.37ms, StdDev: 0.35%


V1, sum 78516973:

AoT, 1 CPU
Avg: 58.33ms, StdDev: 5.00%


dotnet run -c Release
NumCpu : 2
IsHardwareAccelerated : True
Vector<float>.Count : 8
0        Execution Time: 212.0    78516973
1        Execution Time: 51.0     78516973
2        Execution Time: 94.0     78516973
3        Execution Time: 106.0    78516973
4        Execution Time: 81.0     78516973
5        Execution Time: 94.0     78516973
6        Execution Time: 117.0    78516973
7        Execution Time: 105.0    78516973
8        Execution Time: 96.0     78516973
9        Execution Time: 90.0     78516973
10       Execution Time: 102.0    78516973
Avg: 98.33ms, StdDev: 10.65%


AoT, dotnet build -c Release
user@user-virtual-machine:~/Documents/src/mandelbrot/_optimized/csharp$ ./bin/Release/net7.0/ConsoleApp 
NumCpu : 2
IsHardwareAccelerated : True
Vector<float>.Count : 8
0        Execution Time: 352.0    78516973
1        Execution Time: 39.0     78516973
2        Execution Time: 50.0     78516973
3        Execution Time: 53.0     78516973
4        Execution Time: 40.0     78516973
5        Execution Time: 44.0     78516973
6        Execution Time: 47.0     78516973
7        Execution Time: 49.0     78516973
8        Execution Time: 43.0     78516973
9        Execution Time: 44.0     78516973
10       Execution Time: 66.0     78516973
Avg: 48.44ms, StdDev: 15.89%

user@user-virtual-machine:~/Documents/src/mandelbrot/_optimized/csharp$ ./bin/Release/net7.0/ConsoleApp 
NumCpu : 2
IsHardwareAccelerated : True
Vector<float>.Count : 8
0        Execution Time: 353.0    78516973
1        Execution Time: 47.0     78516973
2        Execution Time: 55.0     78516973
3        Execution Time: 64.0     78516973
4        Execution Time: 56.0     78516973
5        Execution Time: 54.0     78516973
6        Execution Time: 54.0     78516973
7        Execution Time: 54.0     78516973
8        Execution Time: 70.0     78516973
9        Execution Time: 62.0     78516973
10       Execution Time: 65.0     78516973
Avg: 59.33ms, StdDev: 10.15%