#ifndef STRING_H
#define STRING_H

#include <havk.h>

/* Generic string length checker. Returns length. */
size_t strlen(const char_ht *string);

/* A version of `strncpy`. Taking inspiration from OpenBSD's `strlcpy`,
/  the function always adds a null-terminator to the end of the destination.
/  In the hopeful future, I would want the function to be able to check the 
/  allocated memory size of the destination, which is impossible during 
/  run-time in GNU/Linux as `malloc` does not keep that information. */
size_t sstrncpy(char_ht *destination, const char_ht *source, size_t bytes);

#endif