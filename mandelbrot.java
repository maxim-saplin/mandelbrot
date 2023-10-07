// sum 78513425
// openjdk 11.0.20.1 
// java mandelbrot.java - 0.5 sec (ignoring the 1st and 2nd slower runs)


import java.util.Arrays;

public class Mandelbrot {
    // Define constants
    private static final int height = 1024;
    private static final int width = 1024;
    private static final double min_x = -2.0;
    private static final double max_x = 0.47;
    private static final double min_y = -1.12;
    private static final double max_y = 1.12;
    private static final double scalex = (max_x - min_x) / width;
    private static final double scaley = (max_y - min_y) / height;
    private static final int MAX_ITERS = 256;

    // Define the mandelbrot_0 method
    public static int mandelbrot_0(Complex c) {
        Complex z = c;
        int nv = 0;
        for (int i = 1; i < MAX_ITERS; i++) {
            if (z.abs() > 2) {
                break;
            }
            z = z.multiply(z).add(c);
            nv++;
        }
        return nv;
    }

    // Define the mandelbrot method
    public static int[][] mandelbrot() {
        int[][] output = new int[height][width];
        for (int h = 0; h < height; h++) {
            double cy = min_y + h * scaley;
            for (int w = 0; w < width; w++) {
                double cx = min_x + w * scalex;
                output[h][w] = mandelbrot_0(new Complex(cx, cy));
            }
        }
        return output;
    }

    public static void main(String[] args) {
        // Print numbers 1 to 3
        for (int i = 1; i <= 3; i++) {
            System.out.print(i + " ");
            // Start timer
            long start_time = System.currentTimeMillis();
            // Calculate Mandelbrot set
            int[][] result = mandelbrot();
            // End timer
            long end_time = System.currentTimeMillis();
            // Calculate execution time
            double execution_time = (end_time - start_time) / 1000.0;
            System.out.print("Execution Time: " + execution_time + " ");
            // Calculate sum of result
            int sum_result = Arrays.stream(result).flatMapToInt(Arrays::stream).sum();
            System.out.println("                 " + sum_result);
        }
    }
}

// Define Complex class for complex number operations
class Complex {
    private double real;
    private double imaginary;

    public Complex(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public double abs() {
        return Math.sqrt(real * real + imaginary * imaginary);
    }

    public Complex multiply(Complex other) {
        double realPart = real * other.real - imaginary * other.imaginary;
        double imaginaryPart = real * other.imaginary + imaginary * other.real;
        return new Complex(realPart, imaginaryPart);
    }

    public Complex add(Complex other) {
        double realPart = real + other.real;
        double imaginaryPart = imaginary + other.imaginary;
        return new Complex(realPart, imaginaryPart);
    }
}