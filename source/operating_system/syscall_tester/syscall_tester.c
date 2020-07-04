///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK System Call Tester                                //
// Filename        -- syscall_test.c                                         //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include <havk.h>

#define THREADS 5
#define STACK_SIZE 512
#define PAGE_FAULT_ADDRESS 0xDEADC0DE

// The below attribute aligns the thread's stack pointer (RSP).
void __attribute__((force_align_arg_pointer)) thread_function(void)
{
	sysargs_ht arguments;

	arguments.argument_1 = 0x575CA11;

	for (uint64_t i = 1; i < UINT64_MAX; ++i)
	{
		if (i == 0x10000000) // Cause a page fault on purpose.
		{
			uint64_t *fault = (uint64_t *)PAGE_FAULT_ADDRESS;
			*fault = 1;
			arguments.argument_3 = *fault;
		}
		else if (i % 100000000) // Poor man's delay.
		{
			continue;
		}

		arguments.argument_2 = i;
		syscall(NULL_OPERATION, &arguments);
	}

	while (true);
}

// This file is just for testing the system calls and ELF loading mechanism.
int main(void)
{
	static uint64_t alignas(16) stacks[THREADS][STACK_SIZE];
	sysargs_ht arguments;

	arguments.argument_1 = (uint64_t) thread_function;

	for (uint64_t i = 1, s = 0; i <= UINT32_MAX; ++i)
	{
		arguments.argument_2 = (uint64_t) &stacks[s][STACK_SIZE - 1];

		if (s < THREADS
			&& syscall(CREATE_THREAD_OPERATION, &arguments)
				== NO_ERROR)
		{
			++s;
		}
	}

	while (true);
}
