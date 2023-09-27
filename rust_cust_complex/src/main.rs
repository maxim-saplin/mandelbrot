// 0,7 sec
// sum 78513425

use std::time::Instant;

const HEIGHT: usize = 1024;
const WIDTH: usize = 1024;
const MIN_X: f64 = -2.0;
const MAX_X: f64 = 0.47;
const MIN_Y: f64 = -1.12;
const MAX_Y: f64 = 1.12;
const SCALEX: f64 = (MAX_X - MIN_X) / WIDTH as f64;
const SCALEY: f64 = (MAX_Y - MIN_Y) / HEIGHT as f64;
const MAX_ITERS: usize = 256;

fn mandelbrot_0(c: num_complex::Complex<f64>) -> usize {
    let mut z = c;
    let mut nv = 0;
    for _ in 1..MAX_ITERS {
        if z.norm() > 2.0 {
            break;
        }
        z = z * z + c;
        nv += 1;
    }
    nv
}

fn mandelbrot() -> Vec<Vec<usize>> {
    let mut output = vec![vec![0; WIDTH]; HEIGHT];
    for h in 0..HEIGHT {
        let cy = MIN_Y + (h as f64) * SCALEY;
        for w in 0..WIDTH {
            let cx = MIN_X + (w as f64) * SCALEX;
            output[h][w] = mandelbrot_0(num_complex::Complex::new(cx, cy));
        }
    }
    output
}

fn main() {
    for i in 0..3 {
        print!("{}", i + 1);
        let start_time = Instant::now();
        let _result = mandelbrot();
        let end_time = Instant::now();
        let execution_time = (end_time - start_time).as_secs_f64();
        print!(" Execution Time: {}", execution_time);
        // Calculate and print the sum of all elements in the `result` vector

    let sum: usize = _result.iter().flatten().sum();
    println!("                 {}", sum);
    }
}
