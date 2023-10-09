Testing how float precision/type size (64-bit double and 32-bit float) affect performance

## 64-bit `double`

### dotnet run -c Release

M1 Pro:  
Avg (double): 392.70ms, StdDev: 0.14%, sum 78513425
Avg (float): 395.49ms, StdDev: 0.38%, sum 78520223

### dotnet publish -c Release

M1 Pro:
Avg (double): 392.42ms, StdDev: 0.11%, sum 78513425
Avg (float): 395.35ms, StdDev: 0.13%, sum 78520223

