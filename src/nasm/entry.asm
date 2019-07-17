; This file contains the entry function that HAVK's UEFI bootloader
; needs to launch itself into the Ada program.
BITS 64 ; Already in long mode thanks to UEFI.

SECTION .bss
; System V ABI for x86-64 dictates that stacks grow downwards.
ALIGN 16 ; 16 byte alignment required by the ABI.
stack: ; 64 KiB stack.
	.end:
		RESB 65535
	.start:

bootloader: ; Space for bootloader information.
	GLOBAL .arguments
	.arguments: ; Store the bootloader's argument pointer here.
		RESQ 1
	GLOBAL .magic
	.magic: ; Store the bootloader's magic number here.
		RESQ 1

SECTION .text
ALIGN 4 ; 4 byte alignment from here on out.
GLOBAL entry:function (entry.end - entry)
entry:
	; To avoid any clobbering, I'm saving the UEFI application's passed
	; arguments/parameters pointer to a specific location in memory.
	; The pointer was passed in the way of the x86-64 System V ABI.
	MOV [bootloader.arguments], RDI
	MOV [bootloader.magic], RSI

	MOV RSP, stack.start ; Set up the stack as per usual.
	MOV RBP, RSP ; Set the base pointer to the start/base/top of the stack.

	; I've kept the `main` routine and have stopped doing its
	; job manually, as it now (with the current kernel) generates a lot
	; more instructions than just calling `ada_init` and `_ada_havk`.
	EXTERN main

	; Begin to enter HAVK.
	CALL main

	; HAVK should never exit like this, so this should never be reached.
	; It should handle a shutdown properly.

	.spin: ; Endless loop.
		CLI
		HLT
		JMP .spin
	.end:
