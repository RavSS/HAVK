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

// A general system call wrapper to use as a point of reference. Cannot handle
// all calls. For now, avoid making assembly functions for each separate
// operation and instead try to only use this.
ASSEMBLY_BODY
syserr_ht syscall(sysargs_ht *arguments);

// A general system call wrapper that also sends or receives data up to 256
// bytes along with the system call. The data buffer area must be valid up to
// 256 bytes, even if you intend to send less.
ASSEMBLY_BODY
syserr_ht syscall_data(sysargs_ht *arguments, void *data);

#endif
