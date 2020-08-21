///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Libc                                              //
// Filename        -- string.c                                               //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include "string.h"

void *memcpy(void *destination, const void *source, size_t bytes)
{
	register char *i = destination;
	register const char *a = source;

	for (; bytes--; *i++ = *a++);

	return destination;
}

void memset(void *area, int value, size_t bytes)
{
	register char *i = area;

	for (; bytes--; *i++ = value);
}

size_t strlen(const char *string)
{
	register size_t i;

	for (i = 0; string[i]; ++i);

	return i;
}

size_t strnlen(const char *string, size_t length)
{
	register size_t i;

	for (i = 0; i < length && string[i]; ++i);

	return i;
}

char *strcpy(char *destination, const char *source) // TODO: Expand this later.
{
	return memcpy(destination, source, strlen(source));
}

char *strncpy(char *destination, const char *source, size_t length)
{
	memset(destination, 0, length);  // TODO: Expand this later.
	return memcpy(destination, source, strnlen(source, length));
}
