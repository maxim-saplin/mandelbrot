cmake_minimum_required(VERSION 3.15)
project(mandelbrot C)

set(CMAKE_C_STANDARD 11)

find_package(OpenMP REQUIRED)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mavx2 -fopenmp")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Ofast")

add_executable(mandelbrot main.c)
target_link_libraries(mandelbrot PRIVATE ${OpenMP_C_LIBRARIES} m)