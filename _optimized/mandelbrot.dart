// V1 Original
// M1 Pro, sum 78513425
// dart mandelbrot.dart - Avg: 478.0ms, StdDev: 0.4626%
// dart compile exe mandelbrot.dart - Avg: 256.2ms, StdDev: 0.1646%
// Intel, sum 78513425
// dart mandelbrot.dart - Avg: 614.0ms, StdDev: 2.7101%
// dart compile exe mandelbrot.dart - Avg: 451.3ms, StdDev: 1.7418%

import 'dart:io';
import 'dart:math' as math;
import 'dart:typed_data';

const int height = 1024;
const int width = 1024;
const double min_x = -2.0;
const double max_x = 0.47;
const double min_y = -1.12;
const double max_y = 1.12;
const double scalex = (max_x - min_x) / width;
const double scaley = (max_y - min_y) / height;
const int MAX_ITERS = 256;

int mandelbrot_0(Complex c) {
  Complex z = c;
  int nv = 0;
  for (int i = 1; i < MAX_ITERS; i++) {
    if (z.abs() > 2) {
      break;
    }
    z = z * z + c;
    nv += 1;
  }
  return nv;
}

Uint32List mandelbrot() {
  var output = Uint32List(width * height);
  for (int h = 0; h < height; h++) {
    double cy = min_y + h * scaley;
    for (int w = 0; w < width; w++) {
      double cx = min_x + w * scalex;
      output[h * width + w] = mandelbrot_0(Complex(cx, cy));
    }
  }
  return output;
}

void main() {
  const iterations = 10;
  var measurements = <double>[];
  for (int i = -1; i < iterations; i++) {
    stdout.write('${i + 1}\t ');
    DateTime start_time = DateTime.now();
    var result = mandelbrot();
    DateTime end_time = DateTime.now();
    Duration execution_time = end_time.difference(start_time);
    stdout.write(' Execution Time: ${execution_time.inMilliseconds}');

    int sum = result.reduce((value, element) => value + element);
    stdout.writeln('                       $sum');
    if (i >= 0) {
      measurements.add(execution_time.inMilliseconds.toDouble());
    }
  }

  // Calculate average and standard deviation
  double average = measurements.reduce((a, b) => a + b) / measurements.length;
  var sumOfSquares =
      measurements.map((x) => math.pow(x - average, 2)).reduce((a, b) => a + b);
  double standardDeviation =
      math.sqrt(sumOfSquares / (measurements.length - 1)) / average * 100;

  print('Avg: ${average}ms, StdDev: ${standardDeviation.toStringAsFixed(4)}%');
}

class Complex {
  final double re;
  final double im;

  const Complex(this.re, this.im);

  Complex operator +(Complex other) {
    return Complex(re + other.re, im + other.im);
  }

  Complex operator *(Complex other) {
    return Complex(
        re * other.re - im * other.im, re * other.im + im * other.re);
  }

  double abs() {
    return math.sqrt(re * re + im * im);
  }
}
