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

typedef enum system_call_operation
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
	BUFFER_OPERATION,
	FRAMEBUFFER_ACCESS_OPERATION
} sysop_ht;

typedef enum system_call_error
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

typedef struct system_call_arguments
{
	uint64_t operation; // RAX.
	uint64_t argument_1; // RDI.
	uint64_t argument_2; // RSI.
	uint64_t argument_3; // RDX.
	uint64_t argument_4; // R8.
	uint64_t argument_5; // R9.
} PACKED sysargs_ht;

// The base for the IRQs, starting with the ISA IRQs first for the PIT.
#define IRQ_BASE 32

// Wrappers for system calls. The data region for the specific register set
// must be the full length i.e. the XMM wrapper expects a 256-byte area.
syserr_ht system_call(sysargs_ht *const arguments);
syserr_ht system_call_xmm(sysargs_ht *const arguments, void *const data);

#endif
