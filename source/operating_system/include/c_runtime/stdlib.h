/*****************************************************************************/
/* Program         -- HAVK Libc                                              */
/* Filename        -- stdlib.h                                               */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020-2021                */
/*****************************************************************************/

#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define RAND_MAX 0x7FFFFFFF

/* Standard libc `exit()` (C89). */
void exit(int status);

/* Standard libc `abort()` (C89). */
void abort(void);

/* Standard libc `abs()` (C89). */
int abs(int value);

/* Standard libc `rand()` (C89).
/  Generates an integer using xorwow. */
int rand(void);

/* Standard libc `rand()` (C89).
/  Seeds the generator (xorwow). */
void srand(unsigned int seed);

/* Standard libc `malloc()` (C89). */
void *malloc(size_t bytes);

/* Standard libc `calloc()` (C89). */
void *calloc(size_t block_count, size_t bytes);

/* Standard libc `free()` (C89). */
void free(void *allocation);

#endif
