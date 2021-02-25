/*****************************************************************************/
/* Program         -- HAVK Libc                                              */
/* Filename        -- string.h                                               */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020-2021                */
/*****************************************************************************/

#ifndef STRING_H
#define STRING_H

#include <stddef.h>

/* Standard libc `memset()` (C89). */
void *memset(void *area, int value, size_t bytes);

/* Standard libc `memcpy()` (C89). */
void *memcpy(void *destination, const void *source, size_t bytes);

/* Standard libc `strlen()` (C89). */
size_t strlen(const char *string);

/* Standard libc `strnlen()` (C89). */
size_t strnlen(const char *string, size_t length);

/* Standard libc `strcmp()` (C89). */
int strcmp(const char* string_1, const char* string_2);

/* Standard libc `strncmp()` (C89). */
int strncmp(const char* string_1, const char* string_2, size_t bytes);

/* Standard libc `strcpy()` (C89). */
char *strcpy(char *destination, const char *source);

/* Standard libc `strncpy()` (C89). */
char *strncpy(char *destination, const char *source, size_t length);

#endif
