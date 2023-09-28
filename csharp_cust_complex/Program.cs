using System;

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


        public static void Main()
        {
            for (int i = 0; i < 3; i++)
            {
                Console.Write(i + 1 + " ");
                Console.Out.Flush();
                var stopWatch = new System.Diagnostics.Stopwatch();
                stopWatch.Start();
                var result = Mandelbrot();
                stopWatch.Stop();
                var executionTime = stopWatch.Elapsed;
                Console.Write("Execution Time: {0}", (double)(executionTime.Milliseconds)/1000);

                int sum = 0;
                for (int j = 0; j < result.Length; j++)
                {
                    sum += result[j];
                }

                Console.WriteLine("                    " + sum);
            }
        }
    }
}
