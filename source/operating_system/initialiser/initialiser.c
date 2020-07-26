///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System Initialiser                      //
// Filename        -- ipc_tester.c                                           //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <havk/havk.h>

#define SYSTEM_FOLDER ">HAVK>system>"

#define IPC_TESTER_PATH SYSTEM_FOLDER "IPC_TE~1.ELF"
#define IPC_TESTER_NAME "IPC Tester"

#define THREAD_TESTER_PATH SYSTEM_FOLDER "THREAD~1.ELF"
#define THREAD_TESTER_NAME "Thread Tester"

#define FRAMEBUFFER_TESTER_PATH SYSTEM_FOLDER "FRAMEB~1.ELF"
#define FRAMEBUFFER_TESTER_NAME "Framebuffer Tester"

// Null terminated only. Only for convenience, zero error checking or safety.
static syserr_ht log_string(const char *text)
{
	sysargs_ht arguments;
	uint8_t buffer[256];

	memset(buffer, 0, ARRAY_LENGTH(buffer));
	arguments.operation = LOG_OPERATION;
	memcpy(buffer, text, strlen(text));
	return syscall_data(&arguments, buffer);
}

// The name can be up to 64 bytes and the path can be up to 192 bytes.
static syserr_ht load_file(const char *name, const char *path)
{
	sysargs_ht arguments;
	uint8_t buffer[256];

	memset(buffer, 0, ARRAY_LENGTH(buffer));
	arguments.operation = LOAD_ELF_OPERATION;
	memcpy(buffer, path, strlen(path));
	memcpy(&buffer[192], name, strlen(name));
	return syscall_data(&arguments, buffer);
}

#define LOADER(x, y) \
MACRO_BEGIN\
	if (load_file(x, y) != NO_ERROR)\
	{\
		log_string(\
			"Failed to load the following program: \"" y "\".");\
		return 1;\
	}\
MACRO_END

uint64_t main(void)
{
	const char *start_log = "Starting initialisation from user space.";
	log_string(start_log);

	LOADER(IPC_TESTER_NAME, IPC_TESTER_PATH);
	LOADER(THREAD_TESTER_NAME, THREAD_TESTER_PATH);
	LOADER(FRAMEBUFFER_TESTER_NAME, FRAMEBUFFER_TESTER_PATH);

	return 0;
}