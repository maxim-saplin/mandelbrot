using Dates
using LinearAlgebra

height = 1024
width = 1024
min_x = -2.0
max_x = 0.47
min_y = -1.12
max_y = 1.12
scalex = (max_x - min_x) / width
scaley = (max_y - min_y) / height
MAX_ITERS = 256

function mandelbrot_0(c::Complex{Float64})
    z = c
    nv = 0
    for i in 1:MAX_ITERS
        if abs(z) > 2
            break
        end
        z = z*z + c
        nv += 1
    end
    return nv
end

function mandelbrot()
    output = zeros(Int32, height, width)
    for h in 1:height
        cy = min_y + h * scaley
        for w in 1:width
            cx = min_x + w * scalex
            output[h,w] = mandelbrot_0(complex(cx,cy))
        end
    end
    return output
end

for i in 1:3
    println(i, " ", flush=true)
    start_time = Dates.now()
    result = mandelbrot()
    end_time = Dates.now()
    execution_time = end_time - start_time
    println("Execution Time: ", execution_time, " ")
    sum_result = sum(result)
    println("                 ", sum_result)
end