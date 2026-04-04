#include <assert.h>

#include "deflate.h"

void ZLIB_INTERNAL crc_fold_init(deflate_state *const s) {
    (void)s;
    assert(0);
}

void ZLIB_INTERNAL crc_fold_copy(deflate_state *const s,
                                 unsigned char *dst,
                                 const unsigned char *src,
                                 long len) {
    (void)s;
    (void)dst;
    (void)src;
    (void)len;
    assert(0);
}

unsigned ZLIB_INTERNAL crc_fold_512to32(deflate_state *const s) {
    (void)s;
    assert(0);
    return 0;
}

void ZLIB_INTERNAL fill_window_sse(deflate_state *s) {
    (void)s;
    assert(0);
}
