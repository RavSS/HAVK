///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System IPC Tester                       //
// Filename        -- main.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020-2021                //
///////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <havk/havk.h>
#include <havk/debug.h>

static uint64_t task_id;
static uint8_t identity[256];
static const uint64_t SENDING_PORT = 1337;

void send_test(void)
{
	uint8_t message[256];
	sysargs_ht arguments;

	arguments.operation = SEND_MESSAGE_OPERATION;
	arguments.argument_1 = task_id;
	arguments.argument_2 = ARRAY_LENGTH(message) * 8;
	arguments.argument_3 = SENDING_PORT;

	memset(message, 0, ARRAY_LENGTH(message));

	const char *prefix = "Hello \"";
	const char *name = (char *)&identity[9];
	const char *suffix = "\" receiver.";

	memcpy(message, prefix, strlen(prefix));
	memcpy(&message[strlen(prefix)], name, 64);
	memcpy(&message[strlen(prefix) + 64], suffix, strlen(suffix));

	system_call_xmm(&arguments, message);
}

void receive_test(void)
{
	uint8_t message[2][256];
	sysargs_ht arguments;
	arguments.operation = RECEIVE_MESSAGE_OPERATION;

	while (system_call_xmm(&arguments, message[0]) != NO_ERROR);
	arguments.operation = LOG_OPERATION;

	const char *prefix = "Sender says [";
	const char *suffix = "].";

	memcpy(message[1], prefix, strlen(prefix));
	memcpy(&message[1][strlen(prefix)], message[0], 128);
	memcpy(&message[1][strlen(prefix) + 128], suffix, strlen(suffix));

	system_call_xmm(&arguments, message[1]);
}

uint64_t main(void)
{
	sysargs_ht arguments;

	arguments.operation = IDENTIFY_TASK_OPERATION;
	arguments.argument_1 = 0;

	if (system_call_xmm(&arguments, identity) != NO_ERROR)
	{
		return 1;
	}

	task_id = (uint64_t) identity[0];

	send_test();
	receive_test();

	return 0;
}
