// bun mandelbrot.js - 0,9 sec, sum 78513425
// node mandelbrot.js - 0,82 sec, sum 78513425
// FireFox 118.0.1 - 2,8 sec


const height = 1024;
const width = 1024;
const minX = -2.0;
const maxX = 0.47;
const minY = -1.12;
const maxY = 1.12;
const scaleX = (maxX - minX) / width;
const scaleY = (maxY - minY) / height;
const maxIters = 256;

function complex(re, im) {
  return { re, im };
}

function complexAbs(c) {
  return Math.sqrt(c.re * c.re + c.im * c.im);
}

function complexAdd(a, b) {
  return complex(a.re + b.re, a.im + b.im);
}

function complexMult(a, b) {
  return complex(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re);
}

function mandelbrot0(c) {
  let z = c;
  let nv = 0;
  for (let i = 1; i < maxIters; i++) {
    if (complexAbs(z) > 2) {
      break;
    }
    z = complexAdd(complexMult(z, z), c);
    nv++;
  }
  return nv;
}

function mandelbrot() {
  const output = new Array(height).fill(null).map(() => Array(width).fill(0));
  for (let h = 0; h < height; h++) {
    const cy = minY + h * scaleY;
    for (let w = 0; w < width; w++) {
      const cx = minX + w * scaleX;
      output[h][w] = mandelbrot0(complex(cx, cy));
    }
  }
  return output;
}

function run() {
  let iterations = 3;
  for (let i = 0; i < iterations; i++) {
    console.log(`${i + 1} `);
    const startTime = Date.now();
    const result = mandelbrot();
    const endTime = Date.now();
    const executionTime = (endTime - startTime) / 1000; // Convert milliseconds to seconds
    console.log(`Execution Time: ${executionTime.toFixed(2)}s`); // Display two decimal places

    let sumResult = 0;
    for (let row of result) {
      for (let value of row) {
        sumResult += value;
      }
    }
    console.log(`                  ${sumResult}`);
  }
}

run();