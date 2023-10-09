using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace ConsoleApp
{
    public static class Program
    {
        static readonly int Height = 1024;
        static readonly int Width = 1024;
        static readonly double MinX = -2.0;
        static readonly double MaxX = 0.47;
        static readonly double MinY = -1.12;
        static readonly double MaxY = 1.12;
        static readonly double ScaleX = (MaxX - MinX) / Width;
        static readonly double ScaleY = (MaxY - MinY) / Height;
        static readonly int MaxIters = 256;

        // Introducing float versions of the above double variables
        static readonly float MinXf = (float)MinX;
        static readonly float MinYf = (float)MinY;
        static readonly float ScaleXf = (float)ScaleX;
        static readonly float ScaleYf = (float)ScaleY;

        // Introducing a structure to hold complex numbers
        public struct Complex
        {
            public double Real { get; set; }
            public double Imaginary { get; set; }

            public Complex(double real, double imaginary)
            {
                Real = real;
                Imaginary = imaginary;
            }

            public double Magnitude => Math.Sqrt(Real * Real + Imaginary * Imaginary);

            public Complex Add(Complex c) => new Complex(Real + c.Real, Imaginary + c.Imaginary);

            public Complex Subtract(Complex c) => new Complex(Real - c.Real, Imaginary - c.Imaginary);

            public Complex Multiply(Complex c) => new Complex(Real * c.Real - Imaginary * c.Imaginary, Real * c.Imaginary + Imaginary * c.Real);
        }

        public struct ComplexF
        {
            public float Real { get; set; }
            public float Imaginary { get; set; }

            public ComplexF(float real, float imaginary)
            {
                Real = real;
                Imaginary = imaginary;
            }

            public float Magnitude => (float)Math.Sqrt(Real * Real + Imaginary * Imaginary);

            public ComplexF Add(ComplexF c) => new ComplexF(Real + c.Real, Imaginary + c.Imaginary);

            public ComplexF Subtract(ComplexF c) => new ComplexF(Real - c.Real, Imaginary - c.Imaginary);

            public ComplexF Multiply(ComplexF c) => new ComplexF(Real * c.Real - Imaginary * c.Imaginary, Real * c.Imaginary + Imaginary * c.Real);
        }


        // Updating Mandelbrot_0 to use the Complex structure
        private static int Mandelbrot_0(Complex c)
        {
            Complex z = new Complex(0.0, 0.0);
            int nv = 0;
            for (int i = 1; i < MaxIters; i++)
            {
                Complex zNew = z.Multiply(z).Add(c);
                if (zNew.Magnitude > 2.0)
                {
                    break;
                }

                z = zNew;
                nv += 1;
            }

            return nv;
        }

        // Updating Mandelbrot to use the Complex structure
        private static int[] Mandelbrot()
        {
            var output = new int[Height * Width];
            for (int h = 0, idx = 0; h < Height; h++, idx += Width)
            {
                double cy = MinY + h * ScaleY;
                for (int w = 0; w < Width; w++)
                {
                    double cx = MinX + w * ScaleX;
                    output[idx + w] = Mandelbrot_0(new Complex(cx, cy));
                }
            }
            return output;
        }

        // Updating Mandelbrot_0 to use the ComplexF structure
        private static int Mandelbrot_0F(ComplexF c)
        {
            ComplexF z = new ComplexF(0.0f, 0.0f);
            int nv = 0;
            for (int i = 1; i < MaxIters; i++)
            {
                ComplexF zNew = z.Multiply(z).Add(c);
                if (zNew.Magnitude > 2.0f)
                {
                    break;
                }

                z = zNew;
                nv += 1;
            }

            return nv;
        }

        // Updating Mandelbrot to use the ComplexF structure
        private static int[] MandelbrotF()
        {
            var output = new int[Height * Width];
            for (int h = 0, idx = 0; h < Height; h++, idx += Width)
            {
                float cy = MinYf + h * ScaleYf;
                for (int w = 0; w < Width; w++)
                {
                    float cx = MinXf + w * ScaleXf;
                    output[idx + w] = Mandelbrot_0F(new ComplexF(cx, cy));
                }
            }
            return output;
        }

        public static void Main()
        {
            int[] Result = new int[0];

            // Running the benchmark with double
            var measurements = new List<double>();
            for (int i = -1; i < 10; i++)
            {
                Console.Write(i + 1 + "\t ");
                Console.Out.Flush();
                var stopWatch = new Stopwatch();
                stopWatch.Start();
                Result = Mandelbrot();
                stopWatch.Stop();
                var executionTime = stopWatch.Elapsed;
                if (i >= 0)
                {
                    measurements.Add(executionTime.TotalMilliseconds);
                }

                var sum = Result.Aggregate(0L, (s, u) => s + u);

                Console.WriteLine("Execution Time (double):      {0:F2}ms\t  {1}", executionTime.TotalMilliseconds, (int)sum);
            }

            var average = measurements.Average();
            var sumOfSquares = measurements.Select(x => Math.Pow(x - average, 2)).Sum();
            var standardDeviation = Math.Sqrt(sumOfSquares / (measurements.Count - 1)) / average * 100;
            Console.WriteLine("Avg (double): {0:F2}ms, StdDev: {1:F2}%", average, standardDeviation);

            //GC.Collect();

            // Running the benchmark with float
            measurements = new List<double>();
            for (int i = -1; i < 10; i++)
            {
                Console.Write(i + 1 + "\t ");
                Console.Out.Flush();
                var stopWatch = new Stopwatch();
                stopWatch.Start();
                Result = MandelbrotF();
                stopWatch.Stop();
                var executionTime = stopWatch.Elapsed;
                if (i >= 0)
                {
                    measurements.Add(executionTime.TotalMilliseconds);
                }

                var sum = Result.Aggregate(0L, (s, u) => s + u);

                Console.WriteLine("Execution Time (float):      {0:F2}ms\t  {1}", executionTime.TotalMilliseconds, (int)sum);
            }

            average = measurements.Average();
            sumOfSquares = measurements.Select(x => Math.Pow(x - average, 2)).Sum();
            standardDeviation = Math.Sqrt(sumOfSquares / (measurements.Count - 1)) / average * 100;
            Console.WriteLine("Avg (float): {0:F2}ms, StdDev: {1:F2}%", average, standardDeviation);
        }
    }
}
