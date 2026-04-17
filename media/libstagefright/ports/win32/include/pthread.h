/*
 * Stagefright's win32 include path shadows the toolchain <pthread.h>.
 * On MinGW we need the real pthread declarations for libstdc++ gthr.
 */

#ifndef stagefright_win32_pthread_h
#define stagefright_win32_pthread_h

#if defined(__MINGW32__)
#  include_next <pthread.h>
#endif

#endif
