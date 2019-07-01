; This file contains the entry function that HAVK's UEFI bootloader
; needs to launch itself into the Ada program.
BITS 64 ; Already in long mode thanks to UEFI.

SECTION .bss
; System V ABI for x86-64 dictates that stacks grow downwards.
ALIGN 16 ; 16 byte alignment.
stack: ; 64 KiB stack.
	.base:
		RESB 65535
	.top:

GLOBAL bootloader
bootloader: ; Store the UEFI bootloader arguments' pointer here.
	RESQ 1

GLOBAL magic
magic: ; Store the bootloader's magic number here.
	RESQ 1

SECTION .text
ALIGN 4 ; 4 byte alignment from here on out.
GLOBAL entry:function (entry.exit - entry)
entry:
	; To avoid any clobbering, I'm saving the UEFI application's passed
	; arguments/parameters pointer to a specific location in memory.
	; The pointer was passed in the way of the x86-64 System V ABI.
	MOV [bootloader], RDI
	MOV [magic], RSI

	MOV RSP, stack.top ; Set up the stack as per usual.

	; The GNAT (Ada specification?) generated "main" section places the
	; name of the Ada program onto the stack, but that seems pointless
	; for our current bare metal environment. I have commanded gnatbind
	; to remove it, and am just doing the "main" section's job myself.

	MOV RBP, RSP ; Set the base pointer to the start/base of the stack.

	; Now initialize the Ada program and the environment it expects.
	EXTERN ada_havk_init
	CALL ada_havk_init

	; Enter HAVK.
	EXTERN _ada_havk
	CALL _ada_havk

	; HAVK should never exit like this, so this should never be reached.
	; It should handle a shutdown properly.

	.exit: ; Endless loop.
		CLI
		HLT
		JMP .exit
