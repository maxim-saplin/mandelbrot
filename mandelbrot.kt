// kotlinc-jvm 1.9.0 (JRE 11.0.20.1+1-post-Ubuntu-0ubuntu122.04)
// kotlinc mandelbrot.kt -include-runtime -d mandelbrot.jar
// java -jar mandelbrot.jar 
// ~0.6 (0.54-0.87)
//1 Execution Time: 0.542                  78513425
//2 Execution Time: 0.539                  78513425
//3 Execution Time: 0.766                  78513425
//4 Execution Time: 0.608                  78513425
//5 Execution Time: 0.871                  78513425
//6 Execution Time: 0.721                  78513425
//7 Execution Time: 0.519                  78513425
//8 Execution Time: 0.734                  78513425
//9 Execution Time: 0.599                  78513425
//10 Execution Time: 0.629                  78513425

import java.util.Arrays

class Mandelbrot {
    companion object {
        // Define constants
        private const val height = 1024
        private const val width = 1024
        private const val min_x = -2.0
        private const val max_x = 0.47
        private const val min_y = -1.12
        private const val max_y = 1.12
        private val scalex = (max_x - min_x) / width
        private val scaley = (max_y - min_y) / height
        private const val MAX_ITERS = 256

        // Define the mandelbrot_0 method
        fun mandelbrot_0(c: Complex): Int {
            var z = c
            var nv = 0
            for (i in 1 until MAX_ITERS) {
                if (z.abs() > 2) {
                    break
                }
                z = z.multiply(z).add(c)
                nv++
            }
            return nv
        }

        // Define the mandelbrot method
        fun mandelbrot(): Array<IntArray> {
            val output = Array(height) { IntArray(width) }
            for (h in 0 until height) {
                val cy = min_y + h * scaley
                for (w in 0 until width) {
                    val cx = min_x + w * scalex
                    output[h][w] = mandelbrot_0(Complex(cx, cy))
                }
            }
            return output
        }

        @JvmStatic
        fun main() {
            // Print numbers 1 to 10
            for (i in 1..10) {
                print("$i ")
                // Start timer
                val start_time = System.currentTimeMillis()
                // Calculate Mandelbrot set
                val result = mandelbrot()
                // End timer
                val end_time = System.currentTimeMillis()
                // Calculate execution time
                val execution_time = (end_time - start_time) / 1000.0
                print("Execution Time: $execution_time ")
                // Calculate sum of result
                val sum_result = result.flatMap { it.asIterable() }.sum()
                println("                 $sum_result")
            }
        }
    }
}

// Define Complex class for complex number operations
class Complex(private val real: Double, private val imaginary: Double) {
    fun abs(): Double {
        return Math.sqrt(real * real + imaginary * imaginary)
    }

    fun multiply(other: Complex): Complex {
        val realPart = real * other.real - imaginary * other.imaginary
        val imaginaryPart = real * other.imaginary + imaginary * other.real
        return Complex(realPart, imaginaryPart)
    }

    fun add(other: Complex): Complex {
        val realPart = real + other.real
        val imaginaryPart = imaginary + other.imaginary
        return Complex(realPart, imaginaryPart)
    }
}

fun main() {
    Mandelbrot.main()
}