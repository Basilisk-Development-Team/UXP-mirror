// Scalar CRC fallback for builds that disable CRC SIMD folding.

#include "deflate.h"
#include <string.h>

void ZLIB_INTERNAL crc_fold_init(deflate_state *const s)
{
    s->strm->adler = crc32(0L, Z_NULL, 0);
}

void ZLIB_INTERNAL crc_fold_copy(deflate_state *const s,
                                 unsigned char *dst,
                                 const unsigned char *src,
                                 long len)
{
    memcpy(dst, src, (size_t)len);
    s->strm->adler = crc32(s->strm->adler, dst, (uInt)len);
}

unsigned ZLIB_INTERNAL crc_fold_512to32(deflate_state *const s)
{
    return (unsigned)s->strm->adler;
}
