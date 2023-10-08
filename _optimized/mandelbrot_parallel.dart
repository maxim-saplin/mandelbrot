// V8, aproximating known regions with cicrles, sum 78312740
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 24.0ms, StdDev: 18.7371%
// dart compile exe mandelbrot_parallel.dart - Avg: 23.5ms, StdDev: 15.6993%
// Intel
// dart mandelbrot_parallel.dart - Avg: 25.3ms, StdDev: 18.0698%
// dart compile exe mandelbrot_parallel.dart - Avg: 19.2ms, StdDev: 14.2742%

// V7, one spawned isolate, one main, Isolate.exit and sleeps, sum 78279528
// This version started giving different gains for ARM and Intel, optimizing for Intel affected ARM negatively
// Delay/slleps in the loop doesn't look nice yet seem legit. Isolate.exit() is supposed to increase cross-isolate transfer at that seems to be the case
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 51.3ms, StdDev: 12.0182%
// dart compile exe mandelbrot_parallel.dart - Avg: 47.5ms, StdDev: 10.5146%
// Intel
// dart mandelbrot_parallel.dart - Avg: 57.2ms, StdDev: 10.1539%
// dart compile exe mandelbrot_parallel.dart - Avg: 56.6ms, StdDev: 15.1573%

// V6, all previoius optimization, but no isolates, 1 thread
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 48.5ms, StdDev: 3.2601%
// dart compile exe mandelbrot_parallel.dart - Avg: 48.0ms, StdDev: 0.0000%
// Intel
// dart mandelbrot_parallel.dart - Avg: 69.3ms, StdDev: 16.0116%
// dart compile exe mandelbrot_parallel.dart - Avg: 62.5ms, StdDev: 2.2940%

// V5, more known regions, sum 78279528
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 36.8ms, StdDev: 16.4446%
// dart compile exe mandelbrot_parallel.dart - Avg: 35.0ms, StdDev: 1.3469%
// Intel
// dart mandelbrot_parallel.dart - Avg: 70.0ms, StdDev: 12.7775%
// dart compile exe mandelbrot_parallel.dart - Avg: 67.9ms, StdDev: 5.2162%

// V4, removing iterations for known regions, sum 78279528
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 55.5ms, StdDev: 15.5287%
// dart compile exe mandelbrot_parallel.dart - Avg: 52.2ms, StdDev: 0.8077%
// Intel
// dart mandelbrot_parallel.dart - Avg: 91.2ms, StdDev: 20.4404%
// dart compile exe mandelbrot_parallel.dart - Avg: 84.8ms, StdDev: 12.3280%

// V3, mirroring, sum 78277544
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 116.6ms, StdDev: 14.5949%
// dart compile exe mandelbrot_parallel.dart - Avg: 111.5ms, StdDev: 0.4727%
// Intel
// dart mandelbrot_parallel.dart - Avg: 169.5ms, StdDev: 18.0908%
// dart compile exe mandelbrot_parallel.dart - Avg: 149.4ms, StdDev: 6.6949%

// V2, prepping isolates, sum 78514525
// M1 Pro
// dart mandelbrot_parallel.dart - Avg: 135.5ms, StdDev: 14.6542%
// dart compile exe mandelbrot_parallel.dart - Avg: 129.4ms, StdDev: 0.3991%
// Intel
// dart mandelbrot_parallel.dart - Avg: 212.6ms, StdDev: 16.7892%
// dart compile exe mandelbrot_parallel.dart - Avg: 211.2ms, StdDev: 4.1805%

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
class MandelbrotRequest {
  final int start;
  final int end;

  MandelbrotRequest(this.start, this.end);
}

void isolateBody(SendPort replyToMainPort) {
  var isolatePort = ReceivePort();
  replyToMainPort.send(isolatePort.sendPort);
  isolatePort.listen((message) async {
    var rq = (message as MandelbrotRequest);
    final output = mandelbrot(rq.start, rq.end);
    //replyToMainPort.send(output);
    Isolate.exit(replyToMainPort, output);
  });
}

Uint8List mandelbrot(int start, int end, [bool returnBigList = false]) {
  //final output = Uint8List(width * (end - start));
  final output = returnBigList
      ? Uint8List(width * height)
      : Uint8List(width * (end - start));
  final cxx = Float32List(width);

  for (int w = 0; w < width; w++) {
    cxx[w] = min_x + w * scalex;
  }

// Expanded `variables` to hold all local variables and constants
  Float32List variables = Float32List(15);

  // Mapping of old variable names and constants to `variables` indexes
  // variables[0] -> cx
  // variables[1] -> cy
  // variables[2] -> zx
  // variables[3] -> zy
  // variables[4] -> zx2
  // variables[5] -> zy2
  // variables[6] -> r1
  // variables[7] -> dx1
  // variables[8] -> dy1
  // variables[9] -> r2
  // variables[10] -> dx2
  // variables[11] -> r3
  // variables[12] -> dx3
  // variables[13] -> dy3
  // variables[14] -> 4.0 (constant used in the condition)

  variables[6] = 0.607 * 0.607;
  variables[7] = 0.133;
  variables[8] = 0.0386;
  variables[9] = 0.248 * 0.248;
  variables[10] = 1.0;
  variables[11] = 0.343 * 0.343;
  variables[12] = -0.0089;
  variables[13] = 0.2634;
  variables[14] = 4.0;

  int index = 0;
  for (int h = start; h < end; h++) {
    variables[1] = min_y + h * scaley; // cy

    for (int w = 0; w < width; w++) {
      variables[0] = cxx[w]; // cx
      int nv = 0;

      if (variables[0] > -0.75 &&
              variables[0] < 0.23 &&
              ((variables[0] + variables[7]) * (variables[0] + variables[7]) +
                      (variables[1] + variables[8]) *
                          (variables[1] + variables[8]) <
                  variables[6]) ||
          ((variables[0] + variables[10]) * (variables[0] + variables[10]) +
                  variables[1] * variables[1] <
              variables[9]) ||
          (variables[0] > 0.23 &&
              (variables[0] + variables[12]) * (variables[0] + variables[12]) +
                      (variables[1] + variables[13]) *
                          (variables[1] + variables[13]) <
                  variables[11])) {
        nv = MAX_ITERS - 1;
      } else {
        while (nv < MAX_ITERS) {
          variables[2] = variables[0] * variables[0]; // zx * zx
          variables[3] = variables[1] * variables[1]; // zy * zy

          if ((variables[2] + variables[3]) > variables[14]) {
            break;
          }

          variables[4] = (variables[2] - variables[3]) + variables[0]; // new_zx
          variables[1] = 2 * variables[0] * variables[1] + variables[1]; // zy
          variables[0] = variables[4]; // zx
          nv++;

          if (nv >= MAX_ITERS - 1) {
            break;
          }

          variables[2] = variables[0] * variables[0]; // zzx
          variables[3] = variables[1] * variables[1]; // zzy

          if ((variables[2] + variables[3]) > variables[14]) {
            break;
          }

          variables[4] = (variables[2] - variables[3]) + variables[0]; // new_zx
          variables[1] = 2 * variables[0] * variables[1] + variables[1]; // zy
          variables[0] = variables[4]; // zx

          nv++;
        }
      }
      output[index++] = nv;
    }
  }

  //print(count255 / index);

  // Uint8List mirror = Uint8List(output.length);
  // int hght = (output.length / width).floor();
  // for (int h = 0; h < hght; h++) {
  //   for (int w = 0; w < width; w++) {
  //     mirror[(hght - h - 1) * width + w] = output[h * width + w];
  //   }
  // }

  return output;
}

Isolate? i1, i2;
ReceivePort? mainIsolatPort1, mainIsolatPort2;

Future<(SendPort?, SendPort?, Stream?, Stream?)> spawnIsolates(
    [bool one = false]) async {
  mainIsolatPort1 = ReceivePort();

  i1 = await Isolate.spawn<SendPort>(isolateBody, mainIsolatPort1!.sendPort);

  if (!one) {
    mainIsolatPort2 = ReceivePort();
    i2 = await Isolate.spawn<SendPort>(isolateBody, mainIsolatPort2!.sendPort);
  }

  var r1 = mainIsolatPort1!.asBroadcastStream();
  var r2 = !one ? mainIsolatPort2!.asBroadcastStream() : null;

  var p1 = await r1.first as SendPort;
  var p2 = !one ? await r2!.first as SendPort : null;

  return (p1, p2, r1, r2);
}

void killIsolates() {
  mainIsolatPort1?.close();
  mainIsolatPort2?.close();
  i1?.kill();
  i2?.kill();
}

void main() async {
  const iterations = 10;
  Uint8List result = Uint8List(0);
  var measurements = <double>[];
  //var (send1, _, receive1, _) = await spawnIsolates(true);
  final isolateData = MandelbrotRequest(height ~/ 4, height ~/ 2);
  //final isolateData1 = MandelbrotRequest(0, height ~/ 4);
  // final isolateData2 = MandelbrotRequest(height ~/ 4, height ~/ 2);
  for (int i = -1; i < iterations; i++) {
    stdout.write('${i + 1}\t ');
    DateTime start_time = DateTime.now();
    var (send1, _, receive1, _) = await spawnIsolates(true);

    //send1!.send(isolateData1);
    // send2.send(isolateData2);

    // var futures = [receive1.first, receive2.first];
    // var results = await Future.wait(futures);
    // var b = BytesBuilder(copy: false);
    // b.add(results[0][0]);
    // b.add(results[1][0]);
    // b.add(results[1][1]);
    // b.add(results[0][1]);
    // result = b.toBytes();

    send1!.send(isolateData);

    result = mandelbrot(0, height ~/ 4, true);
    int hght = height ~/ 4;
    int wdth = width ~/ 8;
    var r = result.buffer.asUint64List();
    for (int h = 0; h < hght; h++) {
      for (int w = 0; w < wdth; w++) {
        r[(height - h - 1) * wdth + w] = r[h * wdth + w];
      }
    }

    var res2 = await receive1!.first;

    var r2 = res2.buffer.asUint64List();
    for (int h = 0; h < hght; h++) {
      for (int w = 0; w < wdth; w++) {
        r[(3 * hght - h - 1) * wdth + w] = r2[h * wdth + w];
        r[(hght + h) * wdth + w] = r2[h * wdth + w];
      }
    }

    DateTime end_time = DateTime.now();
    Duration execution_time = end_time.difference(start_time);
    stdout.write(' Execution Time: ${execution_time.inMilliseconds}');

    int sum = result.reduce((value, element) => value + element);
    stdout.writeln('                       $sum');
    if (i >= 0) {
      measurements.add(execution_time.inMilliseconds.toDouble());
    }

    killIsolates();
    await Future.delayed(Duration(milliseconds: 150));
  }

  // Calculate average and standard deviation
  double average = measurements.reduce((a, b) => a + b) / measurements.length;
  var sumOfSquares =
      measurements.map((x) => math.pow(x - average, 2)).reduce((a, b) => a + b);
  double standardDeviation =
      math.sqrt(sumOfSquares / (measurements.length - 1)) / average * 100;

  print('Avg: ${average}ms, StdDev: ${standardDeviation.toStringAsFixed(4)}%');

  //calculateFrequencies(result);
  writeToFile(result);
  stdout.flush();
}

void writeToFile(Uint8List data) {
  // Convert Uint8List to comma separated string
  String dataString = data.join(',');

  // Write to file
  File file = File("output.txt");
  file.writeAsStringSync(dataString, mode: FileMode.write);
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
