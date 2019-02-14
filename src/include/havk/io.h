#ifndef IO_H
#define IO_H

#include <havk.h>
#include <havk/standard.h>
#include <havk/terminal.h>
#include <havk/string.h>

/* Outputs a byte to an I/O port. See source for more. */
void out_byte(uint8_t value, uint16_t port);

/* Returns the value of an I/O port. */
uint8_t in_byte(uint16_t port);

/* An simple `printf()`. Not to official C standards (at all). */
void printf(const char_ht *restrict string_format, ...);

/* TODO: This will eventually be used for printing kernel issues.
/  Right now, it's just a `printf()` that handles a line shift, along
/  with printing how many seconds it has roughly been since boot.
/  The conversion from integer to ASCII characters is done by me so
/  if need be, I can retain when the last kernel message occured. */
#define hprintf(x, ...) do\
{\
	h_utoa(tick / tick_frequency, 10, tick_string, UINT64_MAX_DIGITS);\
	printf("\r[%s]\x20", tick_string);\
	printf(x, __VA_ARGS__);\
} while(0)


#endif
