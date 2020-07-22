///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System Thread Tester                    //
// Filename        -- thread_tester.c                                        //
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

	arguments.operation = NULL_OPERATION;
	arguments.argument_1 = 0x575CA11;
	arguments.argument_5 = 0x1337133713371337;

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
		syscall(&arguments);
	}

	while (true);
}

// This file is just for testing the system call for thread creation.
int main(void)
{
	static uint64_t alignas(16) stacks[THREADS][STACK_SIZE];
	uint64_t current_thread = 0;
	sysargs_ht arguments;

	arguments.operation = CREATE_THREAD_OPERATION;
	arguments.argument_1 = (uint64_t) thread_function;

	while (current_thread < THREADS)
	{
		arguments.argument_2
			= (uint64_t) &stacks[current_thread][STACK_SIZE - 1];
		current_thread += syscall(&arguments) == NO_ERROR;
	}

	return 0;
}
