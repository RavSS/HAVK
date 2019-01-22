#ifndef HAVK_H
#define HAVK_H

#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <limits.h>

/* Kept the idea of compiler warnings from the tutorial. */
#if defined(__linux__)
	#error "You must use a cross-compiler to build this operating system."
#endif

#if !defined(__i386__)
	#error "You must use a i386/i686 32-bit targetting cross-compiler."
#endif

/* Pretty sure that character types are guaranteed
/  to be at least 8 bits anyway? Named these with an "h(avk)" before the
/  "t" to signify that these aren't real/standard types. */
#if CHAR_BIT == 8
	typedef char char_ht; /* `char` may be signed or unsigned. */
	typedef int8_t schar_ht;
	typedef uint8_t uchar_ht;
#else
	typedef char_ht char;
	typedef schar_ht signed char;
	typedef uchar_ht unsigned char;
#endif

#endif