#ifndef STRING_H
#define STRING_H

#include <havk.h>

/* Generic string length checker. Returns length. */
size_t strlen(const char_ht *string);

/* Taking inspiration from OpenBSD's `strlcpy`, this function
/  always adds a null-terminator to the end of the destination, and it only
/  copies up until the first null-terminator. Returns size of copy. */
/* TODO: In the hopeful future, I would want the function to be able to check
/  the (allocated) memory size of the destination, which is impossible during
/  run-time in GNU/Linux as `malloc` does not return that information. */
size_t h_strncpy(char_ht *destination, const char_ht *source, size_t bytes);

char_ht *strcat(char_ht *destination, const char_ht *source);

#endif