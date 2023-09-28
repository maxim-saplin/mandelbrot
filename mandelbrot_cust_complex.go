// go run mandelbrot_cust_complex.go
// Complex64 0,35 sec, sum 78513415
// go build -ldflags="-s -w" -o mandelbrot-optimized mandelbrot_cust_complex.go
// Complex64 0,35 sec, sum 78513415

package main

import (
	"fmt"
	"math"
	"time"
)

const (
	height   = 1024
	width    = 1024
	minX     = -2.0
	maxX     = 0.47
	minY     = -1.12
	maxY     = 1.12
	scaleX   = (maxX - minX) / width
	scaleY   = (maxY - minY) / height
	maxIters = 256
)

// Introducing a custom structure for complex number that uses 64 bit float
type Complex64 struct {
	Real float64
	Imag float64
}

// Function to create a new Complex64 number
func NewComplex64(real, imag float64) Complex64 {
	return Complex64{Real: real, Imag: imag}
}

// Function to add two Complex64 numbers
func (c Complex64) Add(other Complex64) Complex64 {
	return NewComplex64(c.Real+other.Real, c.Imag+other.Imag)
}

// Function to multiply two Complex64 numbers
func (c Complex64) Mul(other Complex64) Complex64 {
	return NewComplex64(c.Real*other.Real-c.Imag*other.Imag, c.Real*other.Imag+c.Imag*other.Real)
}

// Function to get the absolute value of a Complex64 number
func (c Complex64) Abs() float64 {
	return math.Sqrt(c.Real*c.Real + c.Imag*c.Imag)
}

// Updating the mandelbrot0 function to use Complex64 instead of complex128
func mandelbrot0(c Complex64) int {
	z := c
	nv := 0
	for i := 1; i < maxIters; i++ {
		if z.Abs() > 2 {
			break
		}
		z = z.Mul(z).Add(c)
		nv++
	}
	return nv
}

// Updating the mandelbrot function to use Complex64 instead of complex128
func mandelbrot() [][]int {
	output := make([][]int, height)
	for h := 0; h < height; h++ {
		output[h] = make([]int, width)
		cy := minY + float64(h)*scaleY
		for w := 0; w < width; w++ {
			cx := minX + float64(w)*scaleX
			output[h][w] = mandelbrot0(NewComplex64(cx, cy))
		}
	}
	return output
}

func main() {
	for i := 0; i < 3; i++ {
		fmt.Printf("%d ", i+1)
		startTime := time.Now()
		result := mandelbrot()
		endTime := time.Now()
		executionTime := endTime.Sub(startTime)
		fmt.Printf("Execution Time: %.3f", executionTime.Seconds())

		sumResult := 0
		for _, row := range result {
			for _, value := range row {
				sumResult += value
			}
		}
		fmt.Printf("                  %d\n", sumResult)
	}
}