// gcc -o mandelbrot mandelbrot.c -lm ./mandelbrot

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <complex.h>

int height = 1024;
int width = 1024;
double min_x = -2.0;
double max_x = 0.47;
double min_y = -1.12;
double max_y = 1.12;
double scalex;
double scaley;
int MAX_ITERS = 256;

int mandelbrot_0(double complex c) {
    double complex z = c;
    int nv = 0;
    for (int i = 1; i < MAX_ITERS; i++) {
        if (abs(z) > 2) {
            break;
        }
        z = z * z + c;
        nv += 1;
    }
    return nv;
}

int* mandelbrot() {
    int* output = malloc(height * width * sizeof(int));
    for (int h = 0; h < height; h++) {
        double cy = min_y + h * scaley;
        for (int w = 0; w < width; w++) {
            double cx = min_x + w * scalex;
            output[h * width + w] = mandelbrot_0(cx + cy * I);
        }
    }
    return output;
}

int main() {
    scalex = (max_x - min_x) / width;
    scaley = (max_y - min_y) / height;

    for (int i = 0; i < 3; i++) {
        printf("%d ", i + 1);
        fflush(stdout);
        clock_t start_time = clock();
        int* result = mandelbrot();
        clock_t end_time = clock();
        double execution_time = (double)(end_time - start_time) / CLOCKS_PER_SEC;
        printf("Execution Time: %lf\n", execution_time);

        // Free allocated memory
        free(result);
    }

    return 0;
}
