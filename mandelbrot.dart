// dart mandelbrot.dart               - 0,62 - 0,70 sec
// dart compile exe mandelbrot.dart   - 0,42 sec
// dart mandelbrot.dart               - Unit32List,  0,60 - 0,64 sec
// dart mandelbrot.dart               - Unit32List,  0,44 - 0,46 sec

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
  for (int i = 0; i < 3; i++) {
    print('${i + 1} ');
    DateTime start_time = DateTime.now();
    var result = mandelbrot();
    DateTime end_time = DateTime.now();
    Duration execution_time = end_time.difference(start_time);
    print(
        'Execution Time: ${execution_time.inMilliseconds.toDouble() / 1000.0}');
  }
}

class Complex {
  final double re;
  final double im;

  Complex(this.re, this.im);

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
