///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System Initialiser                      //
// Filename        -- main.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <havk/havk.h>
#include <havk/debug.h>

#define SYSTEM_FOLDER ">HAVK>system>"

#define IPC_TESTER_PATH SYSTEM_FOLDER "IPC_TE~1.ELF"
#define IPC_TESTER_NAME "IPC Tester"

#define THREAD_TESTER_PATH SYSTEM_FOLDER "THREAD~1.ELF"
#define THREAD_TESTER_NAME "Thread Tester"

#define FRAMEBUFFER_TESTER_PATH SYSTEM_FOLDER "FRAMEB~1.ELF"
#define FRAMEBUFFER_TESTER_NAME "Framebuffer Tester"

#define TERMINAL_PATH SYSTEM_FOLDER "terminal.elf"
#define TERMINAL_NAME "Terminal"

#define PS2_PATH SYSTEM_FOLDER "ps2.elf"
#define PS2_NAME "PS/2 Driver"

// The name can be up to 64 bytes and the path can be up to 192 bytes.
static syserr_ht load_file(const char *name, const char *path)
{
	FILE *elf_file;
	uint8_t *elf_file_buffer;
	size_t elf_file_size;
	sysargs_ht arguments;

	elf_file = fopen(path, "rb");

	if (!elf_file)
	{
		log_string("Failed to open the ELF file.");
		return PATH_ERROR;
	}

	// I'm going to store the whole file in the task's memory just for the
	// excuse of trying out the memory allocator, even though I could avoid
	// it by only allocating a 256 temporary buffer between the file and
	// the kernel buffer itself.
	fseek(elf_file, 0, SEEK_END);
	elf_file_size = ftell(elf_file) + 1; // Sizes are always one-based.
	elf_file_buffer = calloc(1, elf_file_size > 256 ? elf_file_size : 256);

	if (!elf_file_buffer)
	{
		fclose(elf_file);
		log_string("Failed to allocate memory to hold file data.");
		return MEMORY_ERROR;
	}

	rewind(elf_file);

	if (!fread(elf_file_buffer, elf_file_size, 1, elf_file))
	{
		free(elf_file_buffer);
		fclose(elf_file);
		log_string("Failed to read entire file.");
		return SIZE_ERROR;
	}

	arguments.operation = BUFFER_OPERATION;
	arguments.argument_1 = 1; // Create the kernel buffer.
	arguments.argument_2 = elf_file_size;
	if (system_call(&arguments) != NO_ERROR)
	{
		free(elf_file_buffer);
		fclose(elf_file);
		log_string("Failed to allocate a kernel buffer.");
		return MEMORY_ERROR;
	}

	arguments.argument_1 = 3; // Write to the kernel buffer.
	for (arguments.argument_2 = 1; arguments.argument_2 < elf_file_size;
		arguments.argument_2 += 256)
	{
		if (system_call_xmm(&arguments,
			&elf_file_buffer[arguments.argument_2 - 1])
				!= NO_ERROR)
		{
			free(elf_file_buffer);
			fclose(elf_file);
			log_string("Failed to write to the kernel buffer.");
			return MEMORY_ERROR;
		}
	}

	arguments.operation = LOAD_ELF_OPERATION;
	strncpy(elf_file_buffer, name, 256); // Reuse the buffer.
	if (system_call(&arguments) != NO_ERROR)
	{
		free(elf_file_buffer);
		fclose(elf_file);
		log_string("Failed to load the ELF file.");
		return ATTEMPT_ERROR;
	}

	arguments.operation = BUFFER_OPERATION;
	arguments.argument_1 = 4; // Destroy the kernel buffer.
	if (system_call(&arguments) != NO_ERROR)
	{
		free(elf_file_buffer);
		fclose(elf_file);
		log_string("Failed to remove old kernel buffer.");
		return MEMORY_ERROR;
	}

	free(elf_file_buffer);
	fclose(elf_file);
	return NO_ERROR;
}

#define LOADER(x, y) \
MACRO_BEGIN\
	if (load_file(x, y) != NO_ERROR)\
	{\
		log_string(\
			"Failed to properly load the following program: "\
			"\"" y "\".");\
		return EXIT_FAILURE;\
	}\
MACRO_END

int main(void)
{
	log_string("Starting initialisation from user space.");

	// TODO: The `fread()` function is very slow right now, so I'll only
	// load one example task.
	log_string("TODO: This will take a while...");
	LOADER(FRAMEBUFFER_TESTER_NAME, FRAMEBUFFER_TESTER_PATH);

	/* Disabled tasks.
	LOADER(IPC_TESTER_NAME, IPC_TESTER_PATH);
	LOADER(THREAD_TESTER_NAME, THREAD_TESTER_PATH);
	LOADER(TERMINAL_NAME, TERMINAL_PATH);
	LOADER(PS2_NAME, PS2_PATH);
	*/

	return EXIT_SUCCESS;
}
