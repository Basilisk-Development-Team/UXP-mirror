// Stub x86 init hooks for Darwin when x86 asm objects are disabled.
#include <stdint.h>

#include "libavcodec/avcodec.h"
#include "libavcodec/dct.h"
#include "libavcodec/fdctdsp.h"
#include "libavcodec/fft.h"
#include "libavcodec/idctdsp.h"

void ff_dct_init_x86(DCTContext *s) {
	(void)s;
}

void ff_fdctdsp_init_x86(FDCTDSPContext *c, AVCodecContext *avctx,
						 unsigned high_bit_depth) {
	(void)c;
	(void)avctx;
	(void)high_bit_depth;
}

void ff_fft_init_x86(FFTContext *s) {
	(void)s;
}

int ff_init_scantable_permutation_x86(uint8_t *idct_permutation,
									  enum idct_permutation_type perm_type) {
	(void)idct_permutation;
	(void)perm_type;
	return 0;
}

void ff_idctdsp_init_x86(IDCTDSPContext *c, AVCodecContext *avctx,
						 unsigned high_bit_depth) {
	(void)c;
	(void)avctx;
	(void)high_bit_depth;
}