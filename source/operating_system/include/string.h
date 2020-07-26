/*****************************************************************************/
/* Program         -- HAVK Libc                                              */
/* Filename        -- string.h                                               */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020                     */
/*****************************************************************************/

#ifndef STRING_H
#define STRING_H

#include <stddef.h>

/* Standard libc `memset()` (C89). */
void memset(void *area, int value, size_t bytes);

/* Standard libc `memcpy()` (C89). */
void *memcpy(void *destination, const void *source, size_t bytes);

/* Standard libc `strlen()` (C89). */
size_t strlen(const char *string);

#endif
