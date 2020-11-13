///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System                                  //
// Filename        -- havk.h                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

// This header includes core functionality that is supported even without libc
// and other useful little things. I'm only assuming GCC compatibility.
#ifndef HAVK_H
#define HAVK_H

// All of the freestanding headers (except for iso646.h, which is too useless).
#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdalign.h>
#include <stdnoreturn.h>
#include <limits.h>
#include <float.h>

// Syntactic sugar for declaring macros and other attributes.
#define MACRO_BEGIN do{
#define MACRO_END }while(0)
#define PACKED __attribute__((packed))
#define ALWAYS_INLINE __inline__ __attribute__((always_inline))
#define ASSEMBLY_BODY __attribute__((naked))
#define ALIGN_STACK __attribute__((force_align_arg_pointer))

// Helper macros.
#define ARRAY_LENGTH(x) (sizeof((x)) / sizeof((x)[0]))

typedef enum
{
	NULL_OPERATION,
	EXIT_TASK_OPERATION,
	RECEIVE_MESSAGE_OPERATION,
	SEND_MESSAGE_OPERATION,
	IDENTIFY_TASK_OPERATION,
	LOAD_ELF_OPERATION,
	HEAP_INCREASE_OPERATION,
	YIELD_OPERATION,
	LOG_OPERATION,
	IRQ_STATISTICS_OPERATION,
	IO_PORT_OPERATION,
	BUFFER_OPERATION,
	FRAMEBUFFER_ACCESS_OPERATION
} syscall_ht;

typedef enum
{
	NO_ERROR,
	ATTEMPT_ERROR,
	MEMORY_ERROR,
	PERMISSION_ERROR,
	SIZE_ERROR,
	INDEX_ERROR,
	FORMAT_ERROR,
	HARDWARE_ERROR,
	PATH_ERROR
} syserr_ht;

typedef struct
{
	uint64_t operation; // RDI.
	uint64_t argument_1; // RSI.
	uint64_t argument_2; // RDX.
	uint64_t argument_3; // R8.
	uint64_t argument_4; // R9.
	uint64_t argument_5; // R10.
} PACKED sysargs_ht;

// The base for the IRQs, starting with the ISA IRQs first for the PIT.
#define IRQ_BASE 32

// A general system call wrapper to use as a point of reference. Cannot handle
// all calls. For now, avoid making assembly functions for each separate
// operation and instead try to only use this.
inline syserr_ht syscall(sysargs_ht *arguments)
{
	syserr_ht status;

	__asm__ volatile
	(
		"PUSH %[asm_arguments_pointer];"
		"MOV RSI, [%[asm_arguments_pointer] + 8 * 1];"
		"MOV RDX, [%[asm_arguments_pointer] + 8 * 2];"
		"MOV R8, [%[asm_arguments_pointer] + 8 * 3];"
		"MOV R9, [%[asm_arguments_pointer] + 8 * 4];"
		"MOV R10, [%[asm_arguments_pointer] + 8 * 5];"
		"MOV RDI, [%[asm_arguments_pointer] + 8 * 0];"
		"SYSCALL;"
		"POP %[asm_arguments_pointer];"
		"MOV [%[asm_arguments_pointer] + 8 * 1], RSI;"
		"MOV [%[asm_arguments_pointer] + 8 * 2], RDX;"
		"MOV [%[asm_arguments_pointer] + 8 * 3], R8;"
		"MOV [%[asm_arguments_pointer] + 8 * 4], R9;"
		"MOV [%[asm_arguments_pointer] + 8 * 5], R10;"
		"MOV %[asm_status], EAX;"
		: [asm_status] "=r" (status)
		: [asm_arguments_pointer] "r" (arguments)
		: "rdi", "rsi", "rdx", "r8", "r9", "r10", "rax", "rcx", "r11",
			"memory"
	);

	return status;
}

// A general system call wrapper that also sends or receives data up to 256
// bytes along with the system call. The data buffer area must be valid up to
// 256 bytes, even if you intend to send less.
inline syserr_ht syscall_data(sysargs_ht *arguments, void *data)
{
	__asm__ volatile
	(
		"MOVDQU XMM0, [%[asm_data_pointer] + 16 * 0];"
		"MOVDQU XMM1, [%[asm_data_pointer] + 16 * 1];"
		"MOVDQU XMM2, [%[asm_data_pointer] + 16 * 2];"
		"MOVDQU XMM3, [%[asm_data_pointer] + 16 * 3];"
		"MOVDQU XMM4, [%[asm_data_pointer] + 16 * 4];"
		"MOVDQU XMM5, [%[asm_data_pointer] + 16 * 5];"
		"MOVDQU XMM6, [%[asm_data_pointer] + 16 * 6];"
		"MOVDQU XMM7, [%[asm_data_pointer] + 16 * 7];"
		"MOVDQU XMM8, [%[asm_data_pointer] + 16 * 8];"
		"MOVDQU XMM9, [%[asm_data_pointer] + 16 * 9];"
		"MOVDQU XMM10, [%[asm_data_pointer] + 16 * 10];"
		"MOVDQU XMM11, [%[asm_data_pointer] + 16 * 11];"
		"MOVDQU XMM12, [%[asm_data_pointer] + 16 * 12];"
		"MOVDQU XMM13, [%[asm_data_pointer] + 16 * 13];"
		"MOVDQU XMM14, [%[asm_data_pointer] + 16 * 14];"
		"MOVDQU XMM15, [%[asm_data_pointer] + 16 * 15];"
		:
		: [asm_data_pointer] "r" (data)
		:
	);

	register syserr_ht status = syscall(arguments);

	__asm__ volatile
	(
		"MOVDQU [%[asm_data_pointer] + 16 * 0], XMM0;"
		"MOVDQU [%[asm_data_pointer] + 16 * 1], XMM1;"
		"MOVDQU [%[asm_data_pointer] + 16 * 2], XMM2;"
		"MOVDQU [%[asm_data_pointer] + 16 * 3], XMM3;"
		"MOVDQU [%[asm_data_pointer] + 16 * 4], XMM4;"
		"MOVDQU [%[asm_data_pointer] + 16 * 5], XMM5;"
		"MOVDQU [%[asm_data_pointer] + 16 * 6], XMM6;"
		"MOVDQU [%[asm_data_pointer] + 16 * 7], XMM7;"
		"MOVDQU [%[asm_data_pointer] + 16 * 8], XMM8;"
		"MOVDQU [%[asm_data_pointer] + 16 * 9], XMM9;"
		"MOVDQU [%[asm_data_pointer] + 16 * 10], XMM10;"
		"MOVDQU [%[asm_data_pointer] + 16 * 11], XMM11;"
		"MOVDQU [%[asm_data_pointer] + 16 * 12], XMM12;"
		"MOVDQU [%[asm_data_pointer] + 16 * 13], XMM13;"
		"MOVDQU [%[asm_data_pointer] + 16 * 14], XMM14;"
		"MOVDQU [%[asm_data_pointer] + 16 * 15], XMM15;"
		:
		: [asm_data_pointer] "r" (data)
		: "memory"
	);

	return status;
}

inline void output_byte(uint16_t port, uint8_t value)
{
	sysargs_ht arguments =
	{
		.operation = IO_PORT_OPERATION,
		.argument_1 = port,
		.argument_2 = value
	};

	syscall(&arguments);
}

inline void output_word(uint16_t port, uint16_t value)
{
	sysargs_ht arguments =
	{
		.operation = IO_PORT_OPERATION,
		.argument_1 = port,
		.argument_2 = value,
		.argument_4 = true // Word size.
	};

	syscall(&arguments);
}

inline uint8_t input_byte(uint16_t port)
{
	sysargs_ht arguments =
	{
		.operation = IO_PORT_OPERATION,
		.argument_1 = port,
		.argument_3 = true // Input, not output.
	};

	syscall(&arguments);
	return arguments.argument_2;
}

inline uint16_t input_word(uint16_t port)
{
	sysargs_ht arguments =
	{
		.operation = IO_PORT_OPERATION,
		.argument_1 = port,
		.argument_3 = true, // Input, not output.
		.argument_4 = true // Word size.
	};

	syscall(&arguments);
	return arguments.argument_2;
}

#endif
