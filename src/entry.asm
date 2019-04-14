; This file contains the entry function that HAVK's UEFI bootloader
; needs to launch itself into the Ada program.
BITS 64 ; Already in long mode thanks to UEFI.

SECTION .bss
; System V ABI for x86-64 dictates that stacks grow downwards.
ALIGN 16 ; 16 byte alignment.
stacks: ; 16 KiB stacks as of now.
	.primary_base:
		RESB 16384
	.primary_top:

	; TODO: The secondary stack is currently disabled. I believe I have
	; to mess around with GNAT to enable it by first removing the
	; restriction and then specifying its fixed size and location?
	; I think this has to be set up by "s-secsta.adb" and "s-secsta.ads"?
	;.secondary_base:
		;RESB 16384
	;.secondary_top:

GLOBAL arguments
arguments: ; Store the UEFI arguments pointer here.
	RESQ 1

SECTION .text
ALIGN 4 ; 4 byte alignment from here out.
GLOBAL entry:function (entry.exit - entry)
entry:
	; To avoid any clobbering, I'm saving the UEFI application's passed
	; arguments and parameters to specific locations in memory.
	MOV [arguments], RDI

	MOV RSP, stacks.primary_top ; Set up the stack as per usual.
	MOV RBP, RSP ; Set the base pointer.
	; The "main" section places the name of the Ada program onto the stack,
	; but that seems pointless for our current bare metal environment.

	; Now initialize the Ada program and the environment it expects.
	EXTERN havk_init
	CALL havk_init

	; Enter HAVK.
	EXTERN _ada_havk
	CALL _ada_havk

	; HAVK should never exit like this, this should never be reached.

	.exit: ; Endless loop. Never "RET" to the UEFI application.
		CLI
		HLT
		JMP .exit
		RET ; Unreachable. Just here for clarity.
