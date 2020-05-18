///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK System Call Tester                                //
// Filename        -- syscall_test.c                                         //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2019-2020                //
///////////////////////////////////////////////////////////////////////////////

#include <havk.h>

// This compiles down to a binary, so don't read or write anything that belongs
// likely belongs in the data section and don't write anything anywhere that
// isn't stack space.
int main(void)
{
	for (uint64_t i = 1; i <= 10; ++i)
	{
		__asm__ volatile
		(
			"MOV RDI, 0;"
			"MOV RSI, 0x575CA11;"
			"MOV RDX, %0;"
			"SYSCALL;"
			:
			: "g" (i)
			: "rdi", "rsi", "rdx"
		);
	}

	while (1);
}
