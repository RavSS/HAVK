; This file contains the entry function that HAVK's UEFI bootloader
; needs to launch itself into the Ada program.
BITS 64 ; Already in long mode thanks to UEFI.

SECTION .bss
; System V ABI for x86-64 dictates that stacks grow downwards.
ALIGN 16 ; 16-byte alignment required by the ABI for the stack.
stack: ; 64-KiB stack size.
	GLOBAL .end ; Used for GCC as "-fstack-limit-symbol=stack.end".
	.end:
		RESB 65536 ; Check alignment if you're going to change this.

	GLOBAL .start
	.start:

bootloader: ; Space for bootloader information.
	GLOBAL .arguments
	.arguments: ; Store the bootloader's argument pointer here.
		RESQ 1

	GLOBAL .magic
	.magic: ; Store the bootloader's magic number here.
		RESQ 1

SECTION .text
ALIGN 4 ; 4-byte alignment from here on out.
GLOBAL entry:function (entry.end - entry)
; Passed : (RDI => arguments, RSI => BOOTLOADER_MAGIC, RDX => page_structure)
entry:
	; My UEFI bootloader passes a page structure, but does not enable it.
	MOV CR3, RDX ; Enable the temporary paging structure without changes.

	; Now we're no longer position-independent and are referencing -2 GiB.

	; Don't forget this step to set the RIP or very illusive bugs start
	; occuring. Need to jump to the virtual address of the `entry` function
	; without recursively doing it and getting stuck in a loop.
	MOV RAX, .set_rip ; Store the local label's higher-half address in RAX.
	JMP RAX ; Now we leave the actual physical address for the virtual one.
	.set_rip: ; Use a label instead of counting the JMP instruction's size.

	; The instruction pointer is now at the memory space's higher-half.

	; To avoid any clobbering, I'm saving the UEFI application's passed
	; arguments/parameters pointer to a specific location in memory.
	; The pointer was passed in the way of the x86-64 System V ABI.
	MOV [bootloader.arguments], RDI
	MOV [bootloader.magic], RSI

	; This is more reliable here than in the bootloader. Should adhere to
	; the System V ABI and what the main program expects at the initial
	; state just in case. This shouldn't really be necessary.
	; Firstly, clear nearly all of the registers.
	XOR RAX, RAX
	XOR RBX, RBX
	XOR RCX, RCX
	XOR RDX, RDX
	XOR RSI, RSI
	XOR RDI, RDI
	MOV RSP, stack.start ; Set up the stack as per usual.
	MOV RBP, RSP ; Set the base pointer to the start/base/top of the stack.
	XOR R8, R8
	XOR R9, R9
	XOR R10, R10
	XOR R11, R11
	XOR R12, R12
	XOR R13, R13
	XOR R14, R14
	XOR R15, R15
	XORPD XMM0, XMM0 ; TODO: Does XMM0 hold useful information from UEFI?

	; Set the x87 FPU control word.
	SUB RSP, 2 ; Make room for the FCW register (16-bits).
	FNCLEX ; Clear any FPU exceptions.
	FNSTCW [RSP] ; Store the FCW register's value in the stack.
	MOV WORD [RSP], 0x33F ; Set it to what the System V ABI demands.
	FLDCW [RSP] ; Load the FCW register.
	ADD RSP, 2 ; Clean-up.

	; Set the MXCSR control/status register.
	SUB RSP, 4 ; Make room for the MXCSR register (32-bits).
	MOV DWORD [RSP], 0x1F80 ; Set it to what the System V ABI demands.
	LDMXCSR [RSP] ; Load the MXCSR register.
	ADD RSP, 4 ; Clean-up.

	; Set the appropriate flags in the lower RFLAGS register.
	SUB RSP, 8 ; Make room for the RFLAGS register (64-bits).
	MOV WORD [RSP], 0x6 ; Bits 1 (reserved) and 2 (PF, even parity) set.
	POPFQ ; Clean 8-bytes off the stack and into the RFLAGS register.

	; Begin to enter HAVK.
	EXTERN havk
	CALL havk

	; HAVK should not exit like this in a version at or above V00-01-00.
	; It should handle a shutdown properly instead.
	MOV AL, 0xFE ; Command byte for the PS/2 controller to reset the CPU.
	.shutdown_spin: ; Endless loop. Reset the CPU twice if required.
		OUT 0x64, AL ; 8042 PS/2 controller's CPU reset line pulsed.
		HLT
		CLI ; The next halt will be forever (unless an NMI is raised).
		JMP .shutdown_spin
	.end:
