/*
 * copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * high precision timer, useful to profile code
 */

#ifndef AVUTIL_TIMER_H
#define AVUTIL_TIMER_H

#include "config.h"

#if HAVE_INLINE_ASM

#define AV_READ_TIME read_time

static inline uint64_t read_time(void)
{

#if ARCH_LOONGARCH64
    uint64_t a, id;
    __asm__ volatile ( "rdtime.d  %0, %1" : "=r"(a), "=r"(id) :: );
    return a;
#else
#define START_TIMER
#define STOP_TIMER(id) { }
#endif

#endif /* AVUTIL_TIMER_H */
