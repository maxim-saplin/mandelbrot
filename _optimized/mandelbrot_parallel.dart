// V1, sum 78514525
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 145.7ms, StdDev: 1.2128%
// Intel
// dart mandelbrot_parallel.dart - Avg: 266.7ms, StdDev: 4.5031%
// dart compile exe mandelbrot_parallel.dart - Avg: 288.0ms, StdDev: 12.1533%

import 'dart:io';
import 'dart:math' as math;
import 'dart:typed_data';
import 'dart:isolate';
import 'dart:async';

const int height = 1024;
const int width = 1024;
const double min_x = -2.0;
const double max_x = 0.47;
const double min_y = -1.12;
const double max_y = 1.12;
const double scalex = (max_x - min_x) / width;
const double scaley = (max_y - min_y) / height;
const int MAX_ITERS = 256;

// Define a class to hold the data we'll send to each isolate
class IsolateData {
  final SendPort sendPort;
  final int start;
  final int end;

  IsolateData(this.sendPort, this.start, this.end);
}

// Modify the mandelbrot function to accept IsolateData and calculate a portion of the set
void mandelbrot(IsolateData isolateData) {
  final output = Uint8List(width * (isolateData.end - isolateData.start));
  final cxx = Float32List(width);

  for (int w = 0; w < width; w++) {
    cxx[w] = min_x + w * scalex;
  }

  int index = 0;
  for (int h = isolateData.start; h < isolateData.end; h++) {
    final double cy = min_y + h * scaley;

    for (int w = 0; w < width; w++) {
      final double cx = cxx[w];
      double zx = cx, zy = cy;
      int nv = 0;

      while (nv < MAX_ITERS - 1) {
        final double zzx = zx * zx;
        final double zzy = zy * zy;

        if ((zzx + zzy) > 4.0) {
          break;
        }

        double new_zx = (zzx - zzy) + cx;
        zy = 2 * zx * zy + cy;
        zx = new_zx;
        nv++;

        if (nv >= MAX_ITERS - 1) {
          break;
        }

        final double zzx2 = zx * zx;
        final double zzy2 = zy * zy;

        if ((zzx2 + zzy2) > 4.0) {
          break;
        }

        double new_zx2 = (zzx2 - zzy2) + cx;
        zy = 2 * zx * zy + cy;
        zx = new_zx2;
        nv++;
      }

      output[index++] = nv;
    }
  }
  isolateData.sendPort.send(output);
}

// Function to spawn the isolates and merge the results
Future<Uint8List> spawnIsolates() async {
  final response = Completer<Uint8List>();
  final receivePort = ReceivePort();
  final isolateData1 = IsolateData(receivePort.sendPort, 0, height ~/ 2);
  final isolateData2 = IsolateData(receivePort.sendPort, height ~/ 2, height);

  await Isolate.spawn(mandelbrot, isolateData1);
  await Isolate.spawn(mandelbrot, isolateData2);

  final results = await receivePort.take(2).toList();

  response.complete(Uint8List.fromList([...results[0], ...results[1]]));
  return response.future;
}

void main() async {
  const iterations = 10;
  Uint8List result = Uint8List(0);
  var measurements = <double>[];
  for (int i = -1; i < iterations; i++) {
    stdout.write('${i + 1}\t ');
    DateTime start_time = DateTime.now();
    //result = mandelbrot();
    result = await spawnIsolates();
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

  //calculateFrequencies(result);
}

void calculateFrequencies(Uint8List list) {
  var frequencyMap = <int, int>{};

  // Calculate frequencies
  for (var value in list) {
    if (frequencyMap.containsKey(value)) {
      frequencyMap[value] = frequencyMap[value]! + 1;
    } else {
      frequencyMap[value] = 1;
    }
  }

  // Calculate percentages
  var percentageMap = <int, double>{};
  for (var entry in frequencyMap.entries) {
    percentageMap[entry.key] = (entry.value / list.length) * 100;
  }

  // Sort by frequency
  var sortedEntries = percentageMap.entries.toList()
    ..sort((a, b) => b.value.compareTo(a.value));

  // Print stats
  for (var entry in sortedEntries) {
    print('${entry.key},\t Frequency: ${entry.value.toStringAsFixed(2)}%');
  }
}

// 255,     Frequency: 27.53%
// 2,       Frequency: 21.28%
// 3,       Frequency: 12.40%
// 4,       Frequency: 7.50%
// 5,       Frequency: 4.77%
// 0,       Frequency: 4.51%
// 1,       Frequency: 4.02%
// 6,       Frequency: 3.46%
// 7,       Frequency: 2.34%
// 8,       Frequency: 1.80%
// 9,       Frequency: 1.32%
// 10,      Frequency: 1.08%
// 11,      Frequency: 0.83%
// 12,      Frequency: 0.71%
// 13,      Frequency: 0.58%
// 14,      Frequency: 0.49%
// 15,      Frequency: 0.41%
// 16,      Frequency: 0.38%
// 17,      Frequency: 0.31%
// 18,      Frequency: 0.29%
// 19,      Frequency: 0.24%
// 20,      Frequency: 0.21%
// 21,      Frequency: 0.19%
// 22,      Frequency: 0.18%
// 23,      Frequency: 0.16%
// 24,      Frequency: 0.15%
// 25,      Frequency: 0.14%
// 26,      Frequency: 0.13%
// 28,      Frequency: 0.11%
// 27,      Frequency: 0.10%
// 29,      Frequency: 0.09%
// 30,      Frequency: 0.09%
// 31,      Frequency: 0.09%
// 32,      Frequency: 0.08%
// 33,      Frequency: 0.07%
// 34,      Frequency: 0.07%
// 36,      Frequency: 0.07%
// 38,      Frequency: 0.06%
// 40,      Frequency: 0.06%
// 37,      Frequency: 0.05%
// 35,      Frequency: 0.05%
// 39,      Frequency: 0.05%
// 41,      Frequency: 0.05%
// 42,      Frequency: 0.04%
// 48,      Frequency: 0.04%
// 43,      Frequency: 0.04%
// 44,      Frequency: 0.04%
// 47,      Frequency: 0.04%
// 45,      Frequency: 0.04%
// 46,      Frequency: 0.03%
// 50,      Frequency: 0.03%
// 55,      Frequency: 0.03%
// 52,      Frequency: 0.03%
// 51,      Frequency: 0.03%
// 53,      Frequency: 0.03%
// 49,      Frequency: 0.03%
// 56,      Frequency: 0.02%
// 54,      Frequency: 0.02%
// 59,      Frequency: 0.02%
// 58,      Frequency: 0.02%
// 57,      Frequency: 0.02%
// 62,      Frequency: 0.02%
// 64,      Frequency: 0.02%
// 60,      Frequency: 0.02%
// 63,      Frequency: 0.02%
// 61,      Frequency: 0.02%
// 68,      Frequency: 0.02%
// 67,      Frequency: 0.02%
// 71,      Frequency: 0.02%
// 70,      Frequency: 0.02%
// 65,      Frequency: 0.02%
// 66,      Frequency: 0.02%
// 73,      Frequency: 0.02%
// 72,      Frequency: 0.01%
// 74,      Frequency: 0.01%
// 76,      Frequency: 0.01%
// 78,      Frequency: 0.01%
// 83,      Frequency: 0.01%
// 69,      Frequency: 0.01%
// 80,      Frequency: 0.01%
// 77,      Frequency: 0.01%
// 75,      Frequency: 0.01%
// 82,      Frequency: 0.01%
// 88,      Frequency: 0.01%
// 79,      Frequency: 0.01%
// 84,      Frequency: 0.01%
// 81,      Frequency: 0.01%
// 87,      Frequency: 0.01%
// 86,      Frequency: 0.01%
// 91,      Frequency: 0.01%
// 95,      Frequency: 0.01%
// 92,      Frequency: 0.01%
// 90,      Frequency: 0.01%
// 103,     Frequency: 0.01%
// 98,      Frequency: 0.01%
// 100,     Frequency: 0.01%
// 93,      Frequency: 0.01%
// 97,      Frequency: 0.01%
// 89,      Frequency: 0.01%
// 94,      Frequency: 0.01%
// 104,     Frequency: 0.01%
// 85,      Frequency: 0.01%
// 96,      Frequency: 0.01%
// 124,     Frequency: 0.01%
// 102,     Frequency: 0.01%
// 106,     Frequency: 0.01%
// 110,     Frequency: 0.01%
// 101,     Frequency: 0.01%
// 136,     Frequency: 0.01%
// 121,     Frequency: 0.01%
// 108,     Frequency: 0.01%
// 114,     Frequency: 0.01%
// 117,     Frequency: 0.01%
// 129,     Frequency: 0.01%
// 140,     Frequency: 0.01%
// 99,      Frequency: 0.01%
// 118,     Frequency: 0.01%
// 128,     Frequency: 0.01%
// 122,     Frequency: 0.01%
// 115,     Frequency: 0.01%
// 105,     Frequency: 0.01%
// 111,     Frequency: 0.01%
// 162,     Frequency: 0.00%
// 130,     Frequency: 0.00%
// 107,     Frequency: 0.00%
// 112,     Frequency: 0.00%
// 116,     Frequency: 0.00%
// 142,     Frequency: 0.00%
// 125,     Frequency: 0.00%
// 145,     Frequency: 0.00%
// 135,     Frequency: 0.00%
// 161,     Frequency: 0.00%
// 137,     Frequency: 0.00%
// 109,     Frequency: 0.00%
// 157,     Frequency: 0.00%
// 132,     Frequency: 0.00%
// 155,     Frequency: 0.00%
// 133,     Frequency: 0.00%
// 134,     Frequency: 0.00%
// 165,     Frequency: 0.00%
// 139,     Frequency: 0.00%
// 127,     Frequency: 0.00%
// 119,     Frequency: 0.00%
// 151,     Frequency: 0.00%
// 148,     Frequency: 0.00%
// 143,     Frequency: 0.00%
// 141,     Frequency: 0.00%
// 113,     Frequency: 0.00%
// 167,     Frequency: 0.00%
// 150,     Frequency: 0.00%
// 158,     Frequency: 0.00%
// 131,     Frequency: 0.00%
// 126,     Frequency: 0.00%
// 120,     Frequency: 0.00%
// 147,     Frequency: 0.00%
// 188,     Frequency: 0.00%
// 146,     Frequency: 0.00%
// 190,     Frequency: 0.00%
// 182,     Frequency: 0.00%
// 152,     Frequency: 0.00%
// 123,     Frequency: 0.00%
// 149,     Frequency: 0.00%
// 187,     Frequency: 0.00%
// 183,     Frequency: 0.00%
// 163,     Frequency: 0.00%
// 191,     Frequency: 0.00%
// 173,     Frequency: 0.00%
// 176,     Frequency: 0.00%
// 177,     Frequency: 0.00%
// 222,     Frequency: 0.00%
// 156,     Frequency: 0.00%
// 175,     Frequency: 0.00%
// 154,     Frequency: 0.00%
// 192,     Frequency: 0.00%
// 228,     Frequency: 0.00%
// 153,     Frequency: 0.00%
// 170,     Frequency: 0.00%
// 159,     Frequency: 0.00%
// 198,     Frequency: 0.00%
// 174,     Frequency: 0.00%
// 172,     Frequency: 0.00%
// 179,     Frequency: 0.00%
// 138,     Frequency: 0.00%
// 243,     Frequency: 0.00%
// 171,     Frequency: 0.00%
// 208,     Frequency: 0.00%
// 196,     Frequency: 0.00%
// 184,     Frequency: 0.00%
// 166,     Frequency: 0.00%
// 160,     Frequency: 0.00%
// 180,     Frequency: 0.00%
// 144,     Frequency: 0.00%
// 185,     Frequency: 0.00%
// 193,     Frequency: 0.00%
// 239,     Frequency: 0.00%
// 204,     Frequency: 0.00%
// 221,     Frequency: 0.00%
// 195,     Frequency: 0.00%
// 181,     Frequency: 0.00%
// 232,     Frequency: 0.00%
// 205,     Frequency: 0.00%
// 209,     Frequency: 0.00%
// 169,     Frequency: 0.00%
// 247,     Frequency: 0.00%
// 207,     Frequency: 0.00%
// 202,     Frequency: 0.00%
// 178,     Frequency: 0.00%
// 168,     Frequency: 0.00%
// 219,     Frequency: 0.00%
// 224,     Frequency: 0.00%
// 186,     Frequency: 0.00%
// 246,     Frequency: 0.00%
// 210,     Frequency: 0.00%
// 235,     Frequency: 0.00%
// 216,     Frequency: 0.00%
// 240,     Frequency: 0.00%
// 223,     Frequency: 0.00%
// 217,     Frequency: 0.00%
// 233,     Frequency: 0.00%
// 206,     Frequency: 0.00%
// 189,     Frequency: 0.00%
// 197,     Frequency: 0.00%
// 201,     Frequency: 0.00%
// 236,     Frequency: 0.00%
// 194,     Frequency: 0.00%
// 248,     Frequency: 0.00%
// 218,     Frequency: 0.00%
// 215,     Frequency: 0.00%
// 237,     Frequency: 0.00%
// 242,     Frequency: 0.00%
// 213,     Frequency: 0.00%
// 252,     Frequency: 0.00%
// 164,     Frequency: 0.00%
// 227,     Frequency: 0.00%
// 214,     Frequency: 0.00%
// 203,     Frequency: 0.00%
// 199,     Frequency: 0.00%
// 234,     Frequency: 0.00%
// 249,     Frequency: 0.00%
// 200,     Frequency: 0.00%
// 245,     Frequency: 0.00%
// 226,     Frequency: 0.00%
// 212,     Frequency: 0.00%
// 241,     Frequency: 0.00%
// 238,     Frequency: 0.00%
// 250,     Frequency: 0.00%
// 253,     Frequency: 0.00%
// 251,     Frequency: 0.00%
// 220,     Frequency: 0.00%
// 229,     Frequency: 0.00%
// 254,     Frequency: 0.00%
// 211,     Frequency: 0.00%
// 231,     Frequency: 0.00%
// 225,     Frequency: 0.00%
// 230,     Frequency: 0.00%
// 244,     Frequency: 0.00%
