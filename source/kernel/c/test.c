///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK                                                   //
// Filename        -- test.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2019-2020                //
///////////////////////////////////////////////////////////////////////////////

// This is a test file used for debugging things in C as opposed to Ada, where
// it's more convenient to manipulate pointers and access raw memory. Import
// and call the below functions manually from wherever, but don't keep them in
// either the Ada code or here.
#ifdef DEBUG
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <limits.h>

// These must be used everywhere if you're going to make a task out of a user
// mode function so you can access variables without page violations etc.
#define RING_3_TEXT __attribute__((section(".isolated_text")))
#define RING_3_DATA __attribute__((section(".isolated_data")))
#define RING_3_BSS __attribute__((section(".isolated_bss")))

// The prefixes I use to indicate what the source code for a function is.
#define ADA(x) ada__ ## x
#define ASSEMBLY(x) assembly__ ## x
#define C(x) c__ ## x

// The function to import for calling the testing logic.
#define MAIN C(test) // `c__test()`

#define PACKED __attribute__((__packed__))

extern void ADA(log(const char *log, const char *tag, const bool warn,
	const bool critical));

// Just in case we can't access the kernel's version of this.
static void __stack_chk_fail(void) { while (1); }

// A 64-bit unsigned integer imaging function for logging numeric values. It's
// as compact as I could make it without a string reversal function.
const char *uitoa(uint64_t value)
{
	static char buffer[64 + 1];
	uint_fast8_t digits = 0;
	for (uint64_t i = value; i; i /= 10, ++digits);
	for (; value; buffer[--digits] = (value % 10) + '0', value /= 10);
	return buffer;
}

//////////////////////// Test functions go below here. ////////////////////////

void MAIN(void)
{
	return;
}

#endif
