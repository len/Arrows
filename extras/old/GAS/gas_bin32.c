#include <stdint.h>

uint64_t inline _mm8x8(uint64_t A, uint64_t B) {
	const uint64_t ROW = 0x00000000000000FF;
	const uint64_t COL = 0x0101010101010101;
	uint64_t C = 0;
	for (int i=0; i<8; i++) {
		uint64_t p = COL & (A >> i);
		uint64_t r = ROW & (B >> i*8);
		C ^= (p*r);
	}
	return C;
}

void inline _mm16x16(uint64_t C[2][2], const uint64_t A[2][2], const uint64_t B[2][2]) {
	for (int i=0; i<2; i++) {
		for (int j=0; j<2; i++) {
			C[i][j] = _mm8x8(A[i][0], B[0][j]) ^ _mm8x8(A[i][1], B[1][j]);
		}
	}
}
