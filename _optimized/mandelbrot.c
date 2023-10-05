// gcc main.c -mavx2 -fopenmp -Ofast
// Avg: 22.51ms, StdDev: 25.9203%, sum 78563750

#include <stdio.h>
#include <stdlib.h>
#include <immintrin.h>
#include <omp.h>
#include <memory.h>
#include <time.h>
#include <math.h>

#define uint unsigned int
#define HEIGHT 1024
#define WIDTH 1024
#define MIN_X (-2.0f)
#define MAX_X 0.47f
#define MIN_Y (-1.12f)
#define MAX_Y 1.12f
#define SCALE_X ((MAX_X - MIN_X) / WIDTH)
#define SCALE_Y ((MAX_Y - MIN_Y) / HEIGHT)
#define MAX_ITERS 256
#define NUM_CPU omp_get_num_procs()
#define OFFSET(h, w) ((h) * WIDTH + (w))

void inline __attribute__((always_inline)) Mandelbrot_0_simd(int h, uint* pResult) {
    const __m256 MinYVec = _mm256_set1_ps(MIN_Y);
    const __m256 ScaleXVec = _mm256_set1_ps(SCALE_X);
    const __m256 ScaleYVec = _mm256_set1_ps(SCALE_Y);
    const __m256i IdentVector = _mm256_set1_epi32(1);
    const __m256 FourVec = _mm256_set1_ps(4.0f);
    const __m256 TwoVec = _mm256_set1_ps(2.0f);
    const __m256 DisplacementVector = _mm256_set_ps(7.0f, 6.0f, 5.0f, 4.0f, 3.0f, 2.0f, 1.0f, 0.0f);

    const int offset = h * WIDTH;
    const int mirrorOffset = (HEIGHT - h - 1) * WIDTH;
    for (int w = 0; w < WIDTH; w += 8) {
        const __m256 cyVec = _mm256_add_ps(MinYVec, _mm256_mul_ps(_mm256_set1_ps((float)h), ScaleYVec));
        const __m256 cxVec = _mm256_add_ps(_mm256_set1_ps(MIN_X + (float)w * SCALE_X), _mm256_mul_ps(DisplacementVector, ScaleXVec));
        __m256i nvVec = _mm256_setzero_si256();
        __m256 zReVec = _mm256_setzero_ps();
        __m256 zImVec = _mm256_setzero_ps();
        __m256i breakVec = _mm256_setzero_si256();
        int i = 0;
        do {
            const __m256 zReNewVec = _mm256_add_ps(_mm256_sub_ps(_mm256_mul_ps(zReVec, zReVec), _mm256_mul_ps(zImVec, zImVec)), cxVec);
            const __m256 zImNewVec = _mm256_add_ps(_mm256_mul_ps(_mm256_mul_ps(zReVec, zImVec), TwoVec), cyVec);
            const __m256 mag2Vec = _mm256_add_ps(_mm256_mul_ps(zReNewVec, zReNewVec), _mm256_mul_ps(zImNewVec, zImNewVec));
            const __m256i maskVec = _mm256_add_epi32(_mm256_castps_si256(_mm256_cmp_ps(mag2Vec, FourVec, _CMP_LT_OQ)), IdentVector);
            breakVec = _mm256_or_si256(maskVec, breakVec);
            nvVec = _mm256_add_epi32(nvVec, _mm256_andnot_si256(maskVec, IdentVector));
            zReVec = zReNewVec;
            zImVec = zImNewVec;
            i++;
        } while (!_mm256_testz_si256(_mm256_andnot_si256(breakVec, IdentVector), IdentVector) && i < MAX_ITERS);
        _mm256_storeu_si256((__m256i*)(pResult + offset + w), nvVec);
        _mm256_storeu_si256((__m256i*)(pResult + mirrorOffset + w), nvVec);
    }
}

void MandelbrotSimd(uint* pResult) {
#pragma omp parallel num_threads(NUM_CPU * 4) shared(pResult) default(none)
    {
#pragma omp for schedule(static)
        for (int h = 0; h < HEIGHT / 2; h++) {
            Mandelbrot_0_simd(h, pResult);
        }
    }
}

int main() {
    printf("NumCpu : %d\n", NUM_CPU);
    printf("AVX supported: %s\n", _mm256_testz_si256(_mm256_setzero_si256(), _mm256_setzero_si256()) ? "yes" : "no");
    uint* pResult = (uint*)calloc(HEIGHT * WIDTH, sizeof(uint));



    double* measurements = (double*)malloc(11 * sizeof(double));
    for (int i = -1; i < 10; i++) {
        memset(pResult, 0, HEIGHT * WIDTH * sizeof(uint));
        struct timespec start, end;
        clock_gettime(CLOCK_REALTIME , &start);
        MandelbrotSimd(pResult);
        clock_gettime(CLOCK_REALTIME , &end);
        double exec_time = (double )(end.tv_sec - start.tv_sec) * 1000.0 + (end.tv_nsec - start.tv_nsec) / 1000000.0;
        if (i >= 0) {
            measurements[i] = exec_time;
            uint sum = 0;
            for (int h = 0; h < HEIGHT; h++) {
                for (int w = 0; w < WIDTH; w++) {
                    sum += pResult[OFFSET(h, w)];
                }
            }
            printf("Execution Time:      %fms\t  %u\n", exec_time, sum);
        }
    }

    double average = 0.0;
    for (int i = 0; i < 10; i++) {
        average += measurements[i];
    }
    average /= 10.0;

    double sum_of_squares = 0.0;
    for (int i = 0; i < 10; i++) {
        sum_of_squares += (measurements[i] - average) * (measurements[i] - average);
    }
    double standard_deviation = sqrt(sum_of_squares / 9.0) / average * 100.0;
    printf("Avg: %.2fms, StdDev: %.4f%%\n", average , standard_deviation);
    FILE* fp = fopen("output.txt", "w");
    if (fp == NULL) {
        printf("Error opening file!\n");
        return 1;
    }
    for (int i = 0; i < HEIGHT * WIDTH - 1; i++) {
        fprintf(fp, "%d,", pResult[i]);
    }
    fprintf(fp, "%d", pResult[HEIGHT * WIDTH - 1]);
    fclose(fp);

    free(pResult);
    free(measurements);

    return 0;
}