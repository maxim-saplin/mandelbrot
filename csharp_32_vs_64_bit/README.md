Testing how float precision/type size (64-bit double and 32-bit float) affect performance

## 64-bit `double`

### dotnet run -c Release

M1 Pro:  
Avg (double): 392.70ms, StdDev: 0.14%, sum 78513425
Avg (float): 395.49ms, StdDev: 0.38%, sum 78520223

VMWare, Ubuntu, Intel® Core™ i5-8257U CPU @ 1.40GHz × 2:
Avg (double): 355.99ms, StdDev: 13.13%, sum 78513425
Avg (float): 372.59ms, StdDev: 1.13%, sum 78520223

### dotnet publish -c Release

M1 Pro:
Avg (double): 392.42ms, StdDev: 0.11%, sum 78513425
Avg (float): 395.35ms, StdDev: 0.13%, sum 78520223

VMWare, Ubuntu, Intel® Core™ i5-8257U CPU @ 1.40GHz × 2:
Avg (double): 327.30ms, StdDev: 3.82%, sum 78513425
Avg (float): 370.90ms, StdDev: 1.48%, sum 78520223


