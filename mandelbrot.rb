# 28.5, sum 79273394

require 'time'

height = 1024
width = 1024
min_x = -2.0
max_x = 0.47
min_y = -1.12
max_y = 1.12
scalex = (max_x - min_x) / width
scaley = (max_y - min_y) / height
MAX_ITERS = 256


# class CustomComplex
#   attr_accessor :real, :imaginary

#   def initialize(real, imaginary)
#     @real = real
#     @imaginary = imaginary
#   end

#   def +(other)
#     CustomComplex.new(@real + other.real, @imaginary + other.imaginary)
#   end

#   def *(other)
#     real_part = @real * other.real - @imaginary * other.imaginary
#     imaginary_part = @real * other.imaginary + @imaginary * other.real
#     CustomComplex.new(real_part, imaginary_part)
#   end

#   def abs
#     Math.sqrt(@real**2 + @imaginary**2)
#   end
# end

def mandelbrot_0(c)
  z = Complex(0, 0)
  nv = 0
  for i in 1...MAX_ITERS
    break if z.abs > 2.0
    z = z*z + c
    nv += 1
  end
  nv
end

def mandelbrot(height, width, min_x, max_x, min_y, max_y, scalex, scaley)
  output = Array.new(height) { Array.new(width) }
  for h in 0...height
    cy = min_y + h * scaley
    for w in 0...width
      cx = min_x + w * scalex
      output[h][w] = mandelbrot_0(Complex(cx, cy))
    end
  end
  output
end

3.times do |i|
  print "#{i+1} "
  start_time = Time.now
  result = mandelbrot(height, width, min_x, max_x, min_y, max_y, scalex, scaley)
  end_time = Time.now
  execution_time = end_time - start_time
  print "Execution Time: #{execution_time} "
  sum_result = result.flatten.sum
  print "                 #{sum_result}\n"
end