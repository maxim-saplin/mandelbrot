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

        private static int Mandelbrot_0(double cRe, double cIm)
        {
            double zRe = 0.0;
            double zIm = 0.0;
            int nv = 0;
            for (int i = 1; i < MaxIters; i++)
            {
                double zReNew = zRe * zRe - zIm * zIm + cRe;
                double zImNew = 2.0 * zRe * zIm + cIm;
                if (Math.Sqrt(zReNew * zReNew + zImNew * zImNew) > 2.0)
                {
                    break;
                }

                zRe = zReNew;
                zIm = zImNew;
                nv += 1;
            }

            return nv;
        }

        private static int[] Mandelbrot()
        {
            var output = new int[Height * Width];
            for (int h = 0, idx = 0; h < Height; h++, idx += Width)
            {
                double cy = MinY + h * ScaleY;
                for (int w = 0; w < Width; w++)
                {
                    double cx = MinX + w * ScaleX;
                    output[idx + w] = Mandelbrot_0(cx, cy);
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
