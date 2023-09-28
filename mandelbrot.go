// go run mandelbrot.go
// complex128 0,54 sec, sum 78513415
// go build -ldflags="-s -w" -o mandelbrot-optimized mandelbrot.go
// complex128 0,69 sec, sum 79394478

package main

import (
	"fmt"
	"math/cmplx"
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

func mandelbrot0(c complex128) int {
	z := c
	nv := 0
	for i := 1; i < maxIters; i++ {
		if cmplx.Abs(z) > 2 {
			break
		}
		z = z*z + c
		nv++
	}
	return nv
}

func mandelbrot() [][]int {
	output := make([][]int, height)
	for h := 0; h < height; h++ {
		output[h] = make([]int, width)
		cy := minY + float64(h)*scaleY
		for w := 0; w < width; w++ {
			cx := minX + float64(w)*scaleX
			output[h][w] = mandelbrot0(complex(cx, cy))
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