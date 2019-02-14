#ifndef HAVK_H
#define HAVK_H

#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>
#include <limits.h>

/* Kept the idea of compiler warnings from the tutorial. */
#if defined(__linux__)
	#error "You must use a cross-compiler to build this operating system."
#endif

#if !defined(__i386__)
	#error "You must use an i386/i686 32-bit targeting cross-compiler."
#endif

/* Pretty sure that character types are guaranteed
/  to be at least 8 bits anyway? Named these with an "h(avk)" before the
/  "t" to signify that these aren't real/standard types. */
typedef char char_ht; /* `char` may be signed or unsigned. */
#if CHAR_BIT == 8
	typedef int8_t schar_ht;
	typedef uint8_t uchar_ht;
#else
	typedef signed char schar_ht;
	typedef unsigned char uchar_ht;
#endif

/* Disable interrupts and halt indefinitely. */
#define BREAKPOINT _Pragma ("message \"\n\033[4;95mLOCATED AT ^^^\n\\033[0\"")\
	do { __asm__ volatile ("CLI; HLT;"); } while (1)

/* Waits for an interrupt. Currently used just to check interrupts. */
#define HALT _Pragma ("message \"\n\033[4;95mLOCATED AT ^^^\n\\033[0\"")\
	__asm__ volatile ("HLT")


#endif
