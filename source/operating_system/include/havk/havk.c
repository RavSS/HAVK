///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System                                  //
// Filename        -- havk.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include "havk.h"

// This is an example of a system call that passes all possible arguments.
// Expects a pointer to an argument structure in the RDI register.
#define SYSCALL \
	"PUSH RDI;"\
	"MOV RSI, [RDI + 8 * 1];"\
	"MOV RDX, [RDI + 8 * 2];"\
	"MOV R8, [RDI + 8 * 3];"\
	"MOV R9, [RDI + 8 * 4];"\
	"MOV R10, [RDI + 8 * 5];"\
	"MOV RDI, [RDI + 8 * 0];"\
	"SYSCALL;"\
	"POP RDI;"\
	"MOV [RDI + 8 * 1], RSI;"\
	"MOV [RDI + 8 * 2], RDX;"\
	"MOV [RDI + 8 * 3], R8;"\
	"MOV [RDI + 8 * 4], R9;"\
	"MOV [RDI + 8 * 5], R10;"

ASSEMBLY_BODY
syserr_ht syscall(sysargs_ht *arguments)
{
	(void) arguments; // RDI.
	__asm__ volatile
	(
		SYSCALL
		"RET;"
	);
}

ASSEMBLY_BODY
syserr_ht syscall_data(sysargs_ht *arguments, void *data)
{
	(void) arguments; // RDI.
	(void) data; // RSI.
	__asm__ volatile
	(
		"PUSH RSI;"
		"MOVDQU XMM0, [RSI + 16 * 0];"
		"MOVDQU XMM1, [RSI + 16 * 1];"
		"MOVDQU XMM2, [RSI + 16 * 2];"
		"MOVDQU XMM3, [RSI + 16 * 3];"
		"MOVDQU XMM4, [RSI + 16 * 4];"
		"MOVDQU XMM5, [RSI + 16 * 5];"
		"MOVDQU XMM6, [RSI + 16 * 6];"
		"MOVDQU XMM7, [RSI + 16 * 7];"
		"MOVDQU XMM8, [RSI + 16 * 8];"
		"MOVDQU XMM9, [RSI + 16 * 9];"
		"MOVDQU XMM10, [RSI + 16 * 10];"
		"MOVDQU XMM11, [RSI + 16 * 11];"
		"MOVDQU XMM12, [RSI + 16 * 12];"
		"MOVDQU XMM13, [RSI + 16 * 13];"
		"MOVDQU XMM14, [RSI + 16 * 14];"
		"MOVDQU XMM15, [RSI + 16 * 15];"
		SYSCALL
		"POP RSI;"
		"MOVDQU [RSI + 16 * 0], XMM0;"
		"MOVDQU [RSI + 16 * 1], XMM1;"
		"MOVDQU [RSI + 16 * 2], XMM2;"
		"MOVDQU [RSI + 16 * 3], XMM3;"
		"MOVDQU [RSI + 16 * 4], XMM4;"
		"MOVDQU [RSI + 16 * 5], XMM5;"
		"MOVDQU [RSI + 16 * 6], XMM6;"
		"MOVDQU [RSI + 16 * 7], XMM7;"
		"MOVDQU [RSI + 16 * 8], XMM8;"
		"MOVDQU [RSI + 16 * 9], XMM9;"
		"MOVDQU [RSI + 16 * 10], XMM10;"
		"MOVDQU [RSI + 16 * 11], XMM11;"
		"MOVDQU [RSI + 16 * 12], XMM12;"
		"MOVDQU [RSI + 16 * 13], XMM13;"
		"MOVDQU [RSI + 16 * 14], XMM14;"
		"MOVDQU [RSI + 16 * 15], XMM15;"
		"RET;"
	);
}
