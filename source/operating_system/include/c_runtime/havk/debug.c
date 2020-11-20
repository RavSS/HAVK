///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System                                  //
// Filename        -- debug.c                                                //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include <havk/debug.h>

// Lifted this from my kernel's "test.c".
char *uitoa(uintmax_t value, char *buffer)
{
	uint_fast8_t digits = 0;
	for (uintmax_t i = value; i; i /= 10, ++digits);
	for (; value; buffer[--digits] = (value % 10) + '0', value /= 10);
	return buffer;
}

void log_string(const char *text)
{
	sysargs_ht arguments;
	uint8_t buffer[256];

	arguments.operation = LOG_OPERATION;

	for (uint_fast16_t i = 0; i < ARRAY_LENGTH(buffer); buffer[i++] = 0);

	for (uint_fast16_t i = 0; text[i] && i < ARRAY_LENGTH(buffer); ++i)
	{
		buffer[i] = text[i];
	}

	system_call_xmm(&arguments, buffer);
}
