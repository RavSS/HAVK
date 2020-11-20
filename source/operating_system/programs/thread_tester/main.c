///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System Thread Tester                    //
// Filename        -- main.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <havk/havk.h>

#define THREADS 5
#define STACK_SIZE 512
#define PAGE_FAULT_ADDRESS 0xDEADC0DE

// The below attribute aligns the thread's stack pointer (RSP).
void ALIGN_STACK thread_function(void)
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
		system_call(&arguments);
	}

	while (true);
}

// This file is just for testing the system call for thread creation.
// TODO: I've removed threading in the kernel and I want to bring it into user
// space instead, so this will have to wait for now.
uint64_t main(void)
{
	static char log_string[256] = "TODO: User-space thread capabilities.";
	sysargs_ht arguments;

	arguments.operation = LOG_OPERATION;
	system_call_xmm(&arguments, log_string);

	return 1;
}
