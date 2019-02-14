#ifndef STANDARD_H
#define STANDARD_H
/* standard.h:
/  This header is where more standard and/or basic functions
/  are located. If a function deviates from an official function heavily,
/  then it's prefixed with "h_" to indicate that it's a special HAVK type.
/  This does not mean that all functions without it are identical, they
/  may have a small addition to them, like e.g. the `sleep()` function. */

#include <havk.h>
#include <havk/terminal.h>
#include <havk/PIT.h>
#include <havk/PS2.h>

/* These do not include the addition of a null-terminator. */
#define INT32_MAX_DIGITS 10
#define UINT32_MAX_DIGITS 10
#define INT64_MAX_DIGITS 19
#define UINT64_MAX_DIGITS 20

#define INT32_MAX_HEX_DIGITS 8
#define UINT32_MAX_HEX_DIGITS 8
#define INT64_MAX_HEX_DIGITS 16
#define UINT64_MAX_HEX_DIGITS 16

/* Self-explanatory. String to integer. */
int32_t atoi(const char_ht *string);

/* Method is taken from the legendary Kernighan & Ritchie. Does not handle
/  the largest negative number possible. All changes to it include a bit more
/  safety and an option for base. If the "sign" variable is true, then an
/  additional character appears at the start of the string indicating so.
/  The length passed must account for it if "sign" is true. Returns true
/  if the integer was converted, otherwise it returns false. Returning a
/  pointer to the string seems dangerous if the string or length is bad. */
bool h_itoa(int32_t number, int16_t base, char_ht *string,
	size_t length, bool sign);

/* Same as above, but handles unsigned integers instead. */
bool h_utoa(uint32_t number, int16_t base, char_ht *string, size_t length);

/* TODO: Universal implementation for now. Will make it more experimental. */
void *memcpy(void *destination, const void *source, size_t length);

/* Relies on IRQ 0, which means the PIT's frequency divider must be setup.
/  The second argument indicates how often the condition should be checked. */
void sleep(uint32_t seconds, bool fast);

/* Returns the PS/2 key when the PS/2 key is a certain character. Only
/  really useful for user interaction, as the "ps2_key" variable is accessible
/  by everything anyway. */
char_ht h_getchar(char_ht character);

#endif
