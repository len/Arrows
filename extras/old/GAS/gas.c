/*
 * This library supports the following operations for float32 and float64
 * and modular integers of 8, 16 and 32 bits:
 * 	scalv: x[i] *= a;
 * 	axpyv: y[i] += x[j]*a
 * 	peval: f(a), x[0] + x[1]*a + ... + x[n]*(a^n)
 * 	gemm: C += AB*a
 *
 * TODO:
 * 	pmul, pquorem, prem
 * 	scalm, axpbym, copym, swapm, setm
 * 	gemmt
 * 	trmm
 * 	trsm
 *
 */

#include <stdlib.h>
#include <stdint.h>

#include <cblas.h>

#define min(a,b) ((a) < (b) ? (a) : (b))

/* The ideal block size for doing tiled matrix multiplication (depends on cache size) */
#define BLOCK_SIZE 40

/* Test if an integer is a power of 2 */
#define IS_POWER_OF_TWO(m) (((m)-1)&(m)==0)

/* Test if can delay the reduction of the dot product of n-tuples modulo m, where m has 32 bits and the dot product is computed in 64 bits */
#define CAN_DELAY_REDUCTION(n,m) ((n) < ((0x100000000 / ((m)-1)) * (0x100000000 / ((m)-1))))

uint32_t version() {
	return 1;
}

void fp32_scalv(float *x, uint32_t x_start, uint32_t x_stop, float alpha) {
	if (x_stop == 0 || x_start > x_stop || alpha == 1) return;
	if (alpha == 0) { /* maybe dont need this */
		for (uint32_t i=x_start-1; i<=x_stop-1; i++) x[i] = 0;
	} else {
		cblas_sscal(x_stop-x_start+1, alpha, &x[x_start-1], 1);
	}
}

void fp32_axpyv(float * __restrict y, uint32_t y_start, uint32_t y_stop, float * __restrict x, uint32_t x_start, float alpha) {
	if (y_stop == 0 || y_start > y_stop || alpha == 0) return;
	cblas_saxpy(y_stop-y_start+1, alpha, &x[x_start-1], 1, &y[y_start-1], 1);
}

float fp32_peval(float *x, uint32_t x_start, uint32_t x_stop, float alpha) {
	if (x_stop == 0 || x_start > x_stop) return 0;
	if (alpha == 0) {
		return x[x_start-1];
	} else {
		double tmp = 0;
		for (uint32_t i=x_stop-1; i>=x_start-1; i--) tmp = tmp*alpha + x[i];
		return tmp;
	}
}

void fp32_gemm(float *C, uint32_t C_start, uint32_t C_stride, float *A, uint32_t A_start, uint32_t A_stride, uint32_t A_width, uint32_t A_height, float *B, uint32_t B_start, uint32_t B_stride, uint32_t B_width, float alpha) {
	if (alpha == 0) return;
	cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, A_height, B_width, A_width, alpha, &A[A_start-1], A_stride, &B[B_start-1], B_stride, 1, &C[C_start-1], C_stride);
}

void fp32_trmm(float *B, uint32_t B_start, uint32_t B_stride, uint32_t B_width, float *A, uint32_t A_start, uint32_t A_stride, uint32_t A_width, uint32_t A_height, int is_upper, int is_unitdiagonal, float alpha) {
	if (alpha == 0) return;
	cblas_strmm(CblasRowMajor, CblasLeft, is_upper ? CblasUpper : CblasLower, CblasNoTrans, is_unitdiagonal ? CblasUnit : CblasNonUnit, A_height, A_width, alpha, &A[A_start-1], A_stride, &B[B_start-1], B_stride);
}

void fp64_scalv(double *x, uint32_t x_start, uint32_t x_stop, double alpha) {
	if (x_stop == 0 || x_start > x_stop || alpha == 1) return;
	if (alpha == 0) { /* maybe dont need this */
		for (uint32_t i=x_start-1; i<=x_stop-1; i++) x[i] = 0;
	} else {
		cblas_dscal(x_stop-x_start+1, alpha, &x[x_start-1], 1);
	}
}

void fp64_axpyv(double * __restrict y, uint32_t y_start, uint32_t y_stop, double * __restrict x, uint32_t x_start, double alpha) {
	if (y_stop == 0 || y_start > y_stop || alpha == 0) return;
	cblas_daxpy(y_stop-y_start+1, alpha, &x[x_start-1], 1, &y[y_start-1], 1);
}

double fp64_peval(double *x, uint32_t x_start, uint32_t x_stop, double alpha) {
	if (x_stop == 0 || x_start > x_stop) return 0;
	if (alpha == 0) {
		return x[x_start-1];
	} else {
		double tmp = 0;
		for (uint32_t i=x_stop-1; i>=x_start-1; i--) tmp = tmp*alpha + x[i];
		return tmp;
	}
}

void fp64_gemm(double *C, uint32_t C_start, uint32_t C_stride, double *A, uint32_t A_start, uint32_t A_stride, uint32_t A_width, uint32_t A_height, double *B, uint32_t B_start, uint32_t B_stride, uint32_t B_width, double alpha) {
	if (alpha == 0) return;
	cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, A_height, B_width, A_width, alpha, &A[A_start-1], A_stride, &B[B_start-1], B_stride, 1, &C[C_start-1], C_stride);
}

void fp64_trmm(double *B, uint32_t B_start, uint32_t B_stride, uint32_t B_width, double *A, uint32_t A_start, uint32_t A_stride, uint32_t A_width, uint32_t A_height, int is_upper, int is_unitdiagonal, double alpha) {
	if (alpha == 0) return;
	cblas_dtrmm(CblasRowMajor, CblasLeft, is_upper ? CblasUpper : CblasLower, CblasNoTrans, is_unitdiagonal ? CblasUnit : CblasNonUnit, A_height, A_width, alpha, &A[A_start-1], A_stride, &B[B_start-1], B_stride);
}

void mod32_scalv(uint32_t *x, uint32_t x_start, uint32_t x_stop, uint32_t alpha, uint32_t modulus) {
	if (x_stop == 0 || x_start > x_stop || alpha == 1) return;
	if (alpha == 0) {
		#pragma GCC unroll 8
		for (uint32_t i=x_start-1; i<=x_stop-1; i++) x[i] = 0;
	} else if (alpha == modulus-1) {
		#pragma GCC unroll 8
		for (uint32_t i=x_start-1; i<=x_stop-1; i++) if (x[i]) x[i] = modulus - x[i];
	} else if (modulus == 0) {
		#pragma GCC unroll 8
		for (uint32_t i=x_start-1; i<=x_stop-1; i++) x[i] *= alpha;
	} else if (IS_POWER_OF_TWO(modulus)) {
		#pragma GCC unroll 8
		for (uint32_t i=x_start-1; i<=x_stop-1; i++) x[i] = x[i]*alpha & (modulus-1);
	} else {
		#pragma GCC unroll 8
		for (uint32_t i=x_start-1; i<=x_stop-1; i++) x[i] = (uint64_t)x[i]*alpha % modulus;
	}
}

void mod32_axpyv(uint32_t * __restrict y, uint32_t y_start, uint32_t y_stop, uint32_t * __restrict x, uint32_t x_start, uint32_t alpha, uint32_t modulus) {
	if (y_stop == 0 || y_start > y_stop || alpha == 0) return;
	/* TODO: optimize for alpha = 1 or -1 */
	if (modulus == 0) {
		#pragma GCC unroll 8
		for (uint32_t i=0; i<y_stop-y_start+1; i++) y[i+y_start-1] += x[i+x_start-1]*alpha;
	} else if (IS_POWER_OF_TWO(modulus)) {
		#pragma GCC unroll 8
		for (uint32_t i=0; i<y_stop-y_start+1; i++) y[i+y_start-1] = (y[i+y_start-1] + x[i+x_start-1]*alpha) & (modulus-1);
	} else {
		#pragma GCC unroll 8
		for (uint32_t i=0; i<y_stop-y_start+1; i++) y[i+y_start-1] = (y[i+y_start-1] + (uint64_t)x[i+x_start-1]*alpha) % modulus;
	}
}

uint32_t mod32_peval(uint32_t *x, uint32_t x_start, uint32_t x_stop, uint32_t alpha, uint32_t modulus) {
	if (x_stop == 0 || x_start > x_stop) return 0;
	if (alpha == 0) {
		return x[x_start-1];
	} else if (IS_POWER_OF_TWO(modulus)) {
		uint32_t tmp = 0;
		#pragma GCC unroll 8
		for (uint32_t i=x_stop-1; i>=x_start-1; i--) tmp = tmp*alpha + x[i];
		return tmp & (modulus-1);
	} else {
		uint64_t tmp = 0;
		#pragma GCC unroll 8
		for (uint32_t i=x_stop-1; i>=x_start-1; i--) tmp = (tmp*alpha + x[i]) % modulus;
		return tmp;
	}
}

void inline _mm_naive(uint32_t *C, uint32_t C_stride, uint32_t *A, uint32_t A_stride, uint32_t A_width, uint32_t A_height, uint32_t *B, uint32_t B_stride, uint32_t B_width, uint32_t a, uint32_t modulus) {
	if (a == 1) {
		for (int i=0; i < A_height; i++)
			#pragma GCC unroll 8
			for (int k=0; k < A_width; k++)
				#pragma GCC unroll 8
				for (int j=0; j < B_width; j++)
					C[i*C_stride+j] = (C[i*C_stride+j] + (uint64_t)A[i*A_stride+k] * B[k*B_stride+j]) % modulus;
	} else {
		for (int i=0; i < A_height; i++)
			#pragma GCC unroll 8
			for (int k=0; k < A_width; k++)
				#pragma GCC unroll 8
				for (int j=0; j < B_width; j++)
					C[i*C_stride+j] = (C[i*C_stride+j] + (((uint64_t)A[i*A_stride+k] * B[k*B_stride+j]) % modulus) * a) % modulus;
	}
}

void inline _mm_blocked_delayed(uint32_t block_size, uint32_t * __restrict C, uint32_t C_stride, uint32_t * __restrict A, uint32_t A_stride, uint32_t A_width, uint32_t A_height, uint32_t * __restrict B, uint32_t B_stride, uint32_t B_width, uint32_t a, uint32_t modulus) {
	uint32_t block_bottom, block_right;
	for (int kk=0; kk < A_height; kk += block_size) {
		block_bottom = min(kk + block_size, A_height);
		for (int jj=0; jj < B_width; jj += block_size) {
			block_right = min(jj + block_size, B_width);
			for (int i=0; i < A_height; i++) {
				#pragma GCC unroll 8
				for (int j=jj; j < block_right; j++) {
					uint64_t acc = 0;
					#pragma GCC unroll 8
					for (int k=kk; k < block_bottom; k++)
						acc += (uint64_t)A[i*A_stride+k] * B[k*B_stride+j];
					acc %= modulus;
					C[i*C_stride+j] = (C[i*C_stride+j] + acc * a) % modulus;
				}
			}
		}
	}
}

void mod32_gemm(uint32_t *C, uint32_t C_start, uint32_t C_stride, uint32_t *A, uint32_t A_start, uint32_t A_stride, uint32_t A_width, uint32_t A_height, uint32_t *B, uint32_t B_start, uint32_t B_stride, uint32_t B_width, uint32_t alpha, uint32_t modulus) {
	if (alpha == 0) return;
	if (CAN_DELAY_REDUCTION(BLOCK_SIZE, modulus)) {
		_mm_blocked_delayed(BLOCK_SIZE, &C[C_start-1], C_stride, &A[A_start-1], A_stride, A_width, A_height, &B[B_start-1], B_stride, B_width, alpha, modulus);
		return;
	}
	if (CAN_DELAY_REDUCTION(BLOCK_SIZE/2, modulus)) {
		_mm_blocked_delayed(BLOCK_SIZE/2, &C[C_start-1], C_stride, &A[A_start-1], A_stride, A_width, A_height, &B[B_start-1], B_stride, B_width, alpha, modulus);
		return;
	}
	_mm_naive(&C[C_start-1], C_stride, &A[A_start-1], A_stride, A_width, A_height, &B[B_start-1], B_stride, B_width, alpha, modulus);
}

void fp64_from_mod32(double *F, uint32_t F_start, uint32_t F_stride, uint32_t *M, uint32_t M_start, uint32_t M_stride, uint32_t width, uint32_t height, uint32_t modulus) {
	for (uint32_t i=0; i<height; i++) {
		for (uint32_t j=0; j<width; j++) {
			uint32_t tmp = M[M_start-1+i*M_stride+j];
			if (tmp <= modulus>>1)
				F[F_start-1+i*F_stride+j] = tmp;
			else
				F[F_start-1+i*F_stride+j] = (int64_t)tmp - modulus;
		}
	}
}

void mod32_from_fp64(uint32_t *M, uint32_t M_start, uint32_t M_stride, double *F, uint32_t F_start, uint32_t F_stride, uint32_t width, uint32_t height, uint32_t modulus) {
	for (uint32_t i=0; i<height; i++) {
		for (uint32_t j=0; j<width; j++) {
			double tmp = F[F_start-1+i*F_stride+j];
			if (tmp >= 0)
				M[M_start-1+i*M_stride+j] = (uint64_t)tmp % modulus;
			else
				M[M_start-1+i*M_stride+j] = modulus - ((uint64_t)abs(tmp) % modulus);
		}
	}
}

