///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK	                                             //
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

typedef enum
{
	NULL_OPERATION,
	WRITE_OPERATION,
	READ_OPERATION,
	CREATE_THREAD_OPERATION
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
	uint64_t argument_1;
	uint64_t argument_2;
	uint64_t argument_3;
	uint64_t argument_4;
} PACKED sysargs_ht;

// A general system call wrapper.
syserr_ht syscall(syscall_ht operation, sysargs_ht *arguments);


#endif