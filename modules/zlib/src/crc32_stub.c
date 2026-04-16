// Minimal CRC folding stubs for builds without CRC SIMD support.

#include "zutil.h"
#include "crc32.h"

// The folded CRC state type is declared in crc32.h as crc32_fold (or similar).
// We don't use any SIMD here; we just fall back to scalar crc32.

void crc_fold_init(crc32_fold *crc)
{
    // Initialize to standard CRC-32 initial value.
    crc->crc = 0xffffffffu;
}

void crc_fold_copy(crc32_fold *crc,
                   unsigned char *dst,
                   const unsigned char *src,
                   long len)
{
    // Simple scalar copy + CRC update.
    memcpy(dst, src, (size_t)len);
    crc->crc = crc32(crc->crc, src, (uInt)len);
}

uint32_t crc_fold_512to32(crc32_fold *crc)
{
    // Finalize CRC (invert bits as usual).
    return crc->crc ^ 0xffffffffu;
}
