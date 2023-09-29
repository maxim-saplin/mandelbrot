using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Threading.Tasks;

namespace ConsoleApp
{
    public static class Program
    {
        static readonly int Height = 1024;
        static readonly int Width = 1024;
        static readonly float MinX = -2.0f;
        static readonly float MaxX = 0.47f;
        static readonly float MinY = -1.12f;
        static readonly float MaxY = 1.12f;
        static readonly float ScaleX = (MaxX - MinX) / Width;
        static readonly float ScaleY = (MaxY - MinY) / Height;
        static readonly int MaxIters = 256;
        static readonly int NumCpu = 1;//Environment.ProcessorCount;
        private static readonly int[] Result = new int[Height * Width];

        private static void MandelbrotSimd()
        {
            var options = new ParallelOptions
            {
                MaxDegreeOfParallelism = NumCpu
            };
            Parallel.For(0, Height, options, h =>
            {
                foreach (var (w, cxVec, cyVec) in ComplexPlaneSimd(h))
                {
                    var nvVec = Mandelbrot_0_simd(cxVec, cyVec);
                    nvVec.CopyTo(Result, h * Width + w);
                }
            });
        }

        private static IEnumerable<(int w, Vector<float> cx, Vector<float> cy)> ComplexPlaneSimd(int h)
        {
            var minYVec = new Vector<float>(MinY);
            var scaleXVec = new Vector<float>(ScaleX);
            var scaleYVec = new Vector<float>(ScaleY);
            var displacementArray = new float[] { 0, 1, 2, 3, 4, 5, 6, 7 };
            for (int w = 0; w < Width; w += Vector<float>.Count)
            {
                var cyVec = Vector.Add(minYVec, Vector.Multiply(new Vector<float>(h), scaleYVec));
                var cxVec = Vector.Add(new Vector<float>(MinX + w * ScaleX),
                    Vector.Multiply(new Vector<float>(displacementArray), scaleXVec));
                yield return (w, cxVec, cyVec);
            }
        }

        private static Vector<int> Mandelbrot_0_simd(Vector<float> cReVec, Vector<float> cImVec)
        {
            var zReVec = new Vector<float>(0.0f);
            var zImVec = new Vector<float>(0.0f);
            var nvVec = new Vector<int>(0);
            var breakVec = new Vector<int>(0);
            var fourVec = new Vector<float>(4.0f);
            for (int i = 1; i < MaxIters; i++)
            {
                var zReNewVec =
                    Vector.Add(Vector.Subtract(Vector.Multiply(zReVec, zReVec), Vector.Multiply(zImVec, zImVec)),
                        cReVec);
                var zImNewVec = Vector.Add(Vector.Multiply(Vector.Multiply(zReVec, zImVec), new Vector<float>(2)),
                    cImVec);
                var mag2Vec = Vector.Add(Vector.Multiply(zReNewVec, zReNewVec), Vector.Multiply(zImNewVec, zImNewVec));
                var maskVec = Vector.GreaterThan(mag2Vec, fourVec);
                breakVec = Vector.BitwiseOr(breakVec, Vector.Negate(maskVec));
                nvVec = Vector.ConditionalSelect(Vector.Equals(breakVec, Vector<int>.Zero),
                    Vector.Add(nvVec, Vector<int>.One), nvVec);
                if (Vector.Dot(breakVec, Vector<int>.One) == Vector<int>.Count)
                {
                    break;
                }

                zReVec = zReNewVec;
                zImVec = zImNewVec;
            }

            return nvVec;
        }

        public static void Main()
        {
            Console.WriteLine("NumCpu : {0}", NumCpu);
            Console.WriteLine("IsHardwareAccelerated : {0}", Vector.IsHardwareAccelerated);
            Console.WriteLine("Vector<float>.Count : {0}", Vector<float>.Count);
            var totalElapsed = TimeSpan.Zero;
            List<double> measurements = new List<double>();
            for (int i = -1; i < 10; i++)
            {
                Console.Write(i + 1 + "\t ");
                Console.Out.Flush();
                var stopWatch = new System.Diagnostics.Stopwatch();
                stopWatch.Start();
                MandelbrotSimd();
                stopWatch.Stop();
                var executionTime = stopWatch.Elapsed;
                if (i > 0)
                {
                    measurements.Add(executionTime.Milliseconds);
                }
                var sum = Result.Sum();
                Console.WriteLine("Execution Time: {0:F1}\t  {1}", (double)(executionTime.Milliseconds), sum);
            }

            double average = measurements.Average();
            double sumOfSquares = measurements.Select(x => Math.Pow(x - average, 2)).Sum();
            double standardDeviation = Math.Sqrt(sumOfSquares / (measurements.Count - 1)) / average * 100;

            // Use format string instead of inline values
            Console.WriteLine("Avg: {0:F2}ms, StdDev: {1:F2}%", average, standardDeviation);
        }
    }
}