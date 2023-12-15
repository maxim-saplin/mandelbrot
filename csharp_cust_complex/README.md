## 7.0

dotnet run -c Release

sum 78513425
0.37 sec (0,33-0,48) AoT OFF

dotnet publish -c Release

sum 78513425
0,33 sec (0,32-0,4) AoT ON


## 8.0

dotnet run -c Release

sum 78513425
0.37 sec (0,33-0,45) AoT OFF

dotnet publish -c Release

sum 78513425
0,33 sec (0,32-0,45) AoT ON
