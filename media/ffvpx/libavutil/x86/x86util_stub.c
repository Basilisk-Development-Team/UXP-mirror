// Empty stub to satisfy build system on macOS
#include <stdint.h>

// CPU flags: return 0 (no SIMD)
int ff_get_cpu_flags_x86(void) {
    return 0;
}

// Fixed DSP: do nothing
void ff_fixed_dsp_init_x86(void *dsp) {
    (void)dsp;
}

// Float DSP: do nothing
void ff_float_dsp_init_x86(void *fdsp) {
    (void)fdsp;
}

// LLS init: do nothing
void ff_init_lls_x86(void *lls) {
    (void)lls;
}