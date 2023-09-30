using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.Intrinsics;
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
        static readonly int NumCpu = Environment.ProcessorCount;
        static readonly Vector<float> MinYVec = new(MinY);
        static readonly Vector<float> ScaleXVec = new(ScaleX);
        static readonly Vector<float> ScaleYVec = new(ScaleY);
        static readonly Vector<float> FourVec = new(4.0f);
        private static readonly Vector<float> TwoVec = new(2f);
        private static readonly float[] DisplacementArray = { 0, 1, 2, 3, 4, 5, 6, 7 };
        private static readonly Vector<float> DisplacementVector = new(DisplacementArray);
        private static readonly int[] Result = new int[Height * Width];
        private static readonly int Hw = Height * Width;

        private static readonly Vector256<int> InverseMask256 =
            Vector256.Create(new[] { 7, 6, 5, 4, 3, 2, 1, 0 });

        private static readonly Vector128<int> InverseMask128 =
            Vector128.Create(new[] { 3, 2, 1, 0 });

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void MandelbrotSimd()
        {
            var options = new ParallelOptions
            {
                MaxDegreeOfParallelism = NumCpu
            };
            Parallel.For(0, Height / 2 + 1, options, ComplexPlaneSimd);
        }
        
        private static void ComplexPlaneSimd(int h)
        {
            unchecked
            {
                int offset = h * Width;
                for (int w = 0; w < Width; w += Vector<float>.Count)
                {
                    var cyVec = Vector.Add(MinYVec, Vector.Multiply(new Vector<float>(h), ScaleYVec));
                    var cxVec = Vector.Add(new Vector<float>(MinX + w * ScaleX),
                        Vector.Multiply(DisplacementVector, ScaleXVec));
                    Mandelbrot_0_simd(offset + w, cxVec, cyVec);
                }
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static void Mandelbrot_0_simd(int offset, Vector<float> cReVec, Vector<float> cImVec)
        {
            unchecked
            {
                var zReVec = Vector<float>.Zero;
                var zImVec = Vector<float>.Zero;
                var nvVec = Vector<int>.Zero;
                var breakVec = Vector<int>.Zero;
                for (int i = 1; i < MaxIters; i++)
                {
                    var zReNewVec =
                        Vector.Add(Vector.Subtract(Vector.Multiply(zReVec, zReVec), Vector.Multiply(zImVec, zImVec)),
                            cReVec);
                    var zImNewVec = Vector.Add(Vector.Multiply(Vector.Multiply(zReVec, zImVec), TwoVec),
                        cImVec);
                    var mag2Vec = Vector.Add(Vector.Multiply(zReNewVec, zReNewVec),
                        Vector.Multiply(zImNewVec, zImNewVec));
                    var maskVec = Vector.GreaterThan(mag2Vec, FourVec);
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

                nvVec.CopyTo(Result, offset);
                switch (Vector<int>.Count)
                {
                    case 4:
                        Vector128.Shuffle(nvVec.AsVector128(), InverseMask128)
                            .CopyTo(Result, Hw - offset - 4);
                        break;
                    case 8:
                        Vector256.Shuffle(nvVec.AsVector256(), InverseMask256)
                            .CopyTo(Result, Hw - offset - 8);
                        break;
                }
            }
        }

        public static void Main()
        {
            Console.WriteLine("NumCpu : {0}", NumCpu);
            Console.WriteLine("IsHardwareAccelerated : {0}", Vector.IsHardwareAccelerated);
            Console.WriteLine("Vector<float>.Count : {0}", Vector<float>.Count);
            var measurements = new List<double>();
            for (int i = -1; i < 10; i++)
            {
                Console.Write(i + 1 + "\t ");
                Console.Out.Flush();
                Array.Clear(Result);
                var stopWatch = new Stopwatch();
                stopWatch.Start();
                MandelbrotSimd();
                stopWatch.Stop();
                var executionTime = stopWatch.Elapsed;
                if (i >= 0) {
                    measurements.Add(executionTime.TotalMilliseconds);
                }
                var sum = Result.Sum();
                Console.WriteLine("Execution Time:      {0:F2}ms\t  {1}", executionTime.TotalMilliseconds, sum);
            }
            
            var average = measurements.Average();
            var sumOfSquares = measurements.Select(x => Math.Pow(x - average, 2)).Sum();
            var standardDeviation = Math.Sqrt(sumOfSquares / (measurements.Count - 1)) / average * 100;
            Console.WriteLine("Avg: {0:F2}ms, StdDev: {1:F2}%", average, standardDeviation);
        }
    }
}