///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Libc                                              //
// Filename        -- stdlib.c                                               //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include "stdlib.h"
#include "havk/havk.h"

void exit(int status)
{
	sysargs_ht arguments;
	arguments.operation = EXIT_TASK_OPERATION;
	arguments.argument_1 = (uint64_t) status;
	syscall(&arguments);
	while (true); // Wait for time slice to expire.
}

void abort(void)
{
	exit(EXIT_FAILURE);
}

int abs(int value)
{
	return (value >> (CHAR_BIT * sizeof(int) - 1) | 1) * value;
}

// The random functions (and state) below were taken from Wikipedia.
// READ: https://en.wikipedia.org/wiki/Xorshift#xorwow

static struct rand_state_seed
{
	uint32_t a, b, c, d, counter;
} rand_state;

int rand(void)
{
	uint32_t ret, t = rand_state.d;

	const uint32_t s = rand_state.a;
	rand_state.d = rand_state.c;
	rand_state.c = rand_state.b;
	rand_state.b = s;

	t ^= t >> 2;
	t ^= t << 1;
	t ^= s ^ (s << 4);
	rand_state.a = t;

	rand_state.counter += 362437;
	ret = t + rand_state.counter;
	return ret > RAND_MAX ? RAND_MAX : ret;
}

void srand(unsigned int seed)
{
	// As long as not all remain zero, it should be fine.
	rand_state.a = (seed >> 6) | 1;
	rand_state.b = (seed >> 12) | 1;
	rand_state.c = (seed >> 18) | 1;
	rand_state.d = (seed >> 24) | 1;
}

extern void *__gnat_malloc(size_t bytes);
void *malloc(size_t bytes)
{
	return __gnat_malloc(bytes);
}

extern void __gnat_free(void *allocation);
void free(void *allocation)
{
	__gnat_free(allocation);
}
