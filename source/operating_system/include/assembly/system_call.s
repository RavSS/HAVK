###############################################################################
## Program         -- HAVK Operating System                                  ##
## Filename        -- system_call.s                                          ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020                     ##
###############################################################################

.STRUCT 0
	OPERATION: # RDI.
.STRUCT OPERATION + 8
	ARGUMENT_1: # RSI.
.STRUCT ARGUMENT_1 + 8
	ARGUMENT_2: # RDX.
.STRUCT ARGUMENT_2 + 8
	ARGUMENT_3: # R8.
.STRUCT ARGUMENT_3 + 8
	ARGUMENT_4: # R9.
.STRUCT ARGUMENT_4 + 8
	ARGUMENT_5: # R10.
.STRUCT ARGUMENT_5 + 8

.SECTION .text

.GLOBAL system_call
.TYPE system_call, @function
# (RDI => a pointer to the system call state) >> (RAX => an error code)
system_call:
	PUSH RDI # Need to recover it later. Keep it in register/cache for now.
	MOV RSI, [RDI + ARGUMENT_1]
	MOV RDX, [RDI + ARGUMENT_2]
	MOV R8, [RDI + ARGUMENT_3]
	MOV R9, [RDI + ARGUMENT_4]
	MOV R10, [RDI + ARGUMENT_5]
	MOV RDI, [RDI + OPERATION]

	SYSCALL # Enter the kernel. Does not touch the user's stack.

	POP RDI
	MOV [RDI + ARGUMENT_5], R10
	MOV [RDI + ARGUMENT_4], R9
	MOV [RDI + ARGUMENT_3], R8
	MOV [RDI + ARGUMENT_2], RDX
	MOV [RDI + ARGUMENT_1], RSI

	RET

.GLOBAL system_call_xmm
.TYPE system_call_xmm, @function
# (RDI => a pointer to the system call state,
#  RSI => a pointer to the 256-byte memory area) >> (RAX => an error code)
system_call_xmm:
	1:
		CMP RSI, 0x0
		JE 1b
	MOVDQU XMM0, [RSI + 16 * 0]
	MOVDQU XMM1, [RSI + 16 * 1]
	MOVDQU XMM2, [RSI + 16 * 2]
	MOVDQU XMM3, [RSI + 16 * 3]
	MOVDQU XMM4, [RSI + 16 * 4]
	MOVDQU XMM5, [RSI + 16 * 5]
	MOVDQU XMM6, [RSI + 16 * 6]
	MOVDQU XMM7, [RSI + 16 * 7]
	MOVDQU XMM8, [RSI + 16 * 8]
	MOVDQU XMM9, [RSI + 16 * 9]
	MOVDQU XMM10, [RSI + 16 * 10]
	MOVDQU XMM11, [RSI + 16 * 11]
	MOVDQU XMM12, [RSI + 16 * 12]
	MOVDQU XMM13, [RSI + 16 * 13]
	MOVDQU XMM14, [RSI + 16 * 14]
	MOVDQU XMM15, [RSI + 16 * 15]
	PUSH RSI

	CALL system_call

	POP RSI
	MOVDQU [RSI + 16 * 0], XMM0
	MOVDQU [RSI + 16 * 1], XMM1
	MOVDQU [RSI + 16 * 2], XMM2
	MOVDQU [RSI + 16 * 3], XMM3
	MOVDQU [RSI + 16 * 4], XMM4
	MOVDQU [RSI + 16 * 5], XMM5
	MOVDQU [RSI + 16 * 6], XMM6
	MOVDQU [RSI + 16 * 7], XMM7
	MOVDQU [RSI + 16 * 8], XMM8
	MOVDQU [RSI + 16 * 9], XMM9
	MOVDQU [RSI + 16 * 10], XMM10
	MOVDQU [RSI + 16 * 11], XMM11
	MOVDQU [RSI + 16 * 12], XMM12
	MOVDQU [RSI + 16 * 13], XMM13
	MOVDQU [RSI + 16 * 14], XMM14
	MOVDQU [RSI + 16 * 15], XMM15

	RET
