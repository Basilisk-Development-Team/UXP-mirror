// Minimal SIMD stub for platforms without SIMD support.
// This file must NOT define x86_cpu_enable_simd or x86_check_features.

#include "zutil.h"
#include "deflate.h"

// These signatures must match the prototypes in zlib's inflate.c / deflate.c.

void fill_window_sse(deflate_state *s)
{
    // No SIMD available — generic code will be used instead.
}

void inflate_fast_sse(deflate_state *s, unsigned start)
{
    // No SIMD available — generic inflate_fast will be used.
}

void chunkcopy_sse(unsigned char *out, const unsigned char *from, unsigned len)
{
    // No SIMD — fall back to normal memcpy.
    memcpy(out, from, len);
}

void chunkcopy_safe_sse(unsigned char *out, const unsigned char *from,
                        unsigned len, unsigned space)
{
    // Safe copy fallback.
    if (len > space)
        len = space;
    memcpy(out, from, len);
}
