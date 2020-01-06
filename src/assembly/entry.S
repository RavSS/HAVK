###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- entry.S                                                ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

# This file contains the entry function that HAVK's UEFI bootloader
# needs to launch itself into the Ada program. We're already in long mode.

.SECTION .bss

# System V ABI for x86-64 dictates that stacks grow downwards.
.ALIGN 16 # 16-byte alignment required by the ABI for the stack.
.GLOBAL __stack_end # Used for GCC as "-fstack-limit-symbol=__stack.end".
.GLOBAL __stack_base # 64-KiB stack size.
__stack_end:
	.SPACE 65536 # Check alignment if you're going to change this.
__stack_base:

# Space for bootloader information.
.GLOBAL __bootloader_arguments
.GLOBAL __bootloader_magic
__bootloader_arguments:
	.SPACE 8 # Store the bootloader's argument pointer here.
__bootloader_magic:
	.SPACE 8 # Store the bootloader's magic number here.

.SECTION .text
.ALIGN 4 # 4-byte alignment from here on out.

.GLOBAL assembly__entry
# (RDI => bootloader arguments, RSI => bootloader magic)
assembly__entry:
	# To avoid any clobbering, I'm saving the UEFI application's passed
	# arguments/parameters pointer to a specific location in memory.
	# The pointer was passed in the way of the x86-64 System V ABI.
	MOV [__bootloader_arguments], RDI
	MOV [__bootloader_magic], RSI

	# Something interesting to consider for GAS as opposed to NASM is that
	# NASM assembles e.g. `MOV RAX, symbol` as a MOVABS instruction whereas
	# that doesn't work with GAS in a -2-GiB setup. The size efficient way
	# to load a symbol is to use a LEA instruction with an address relative
	# to RIP. I think it's also possible to accomplish this via sign
	# extension, but this is easier.
	LEA RSP, [RIP + __stack_base]
	MOV RBP, RSP

	# This is more reliable here than in the bootloader. Should adhere to
	# the System V ABI and what the main program expects at the initial
	# state just in case. This shouldn't really be necessary.
	# Firstly, clear nearly all of the registers.
	XOR RAX, RAX
	XOR RBX, RBX
	XOR RCX, RCX
	XOR RDX, RDX
	XOR RSI, RSI
	XOR RDI, RDI
	XOR R8, R8
	XOR R9, R9
	XOR R10, R10
	XOR R11, R11
	XOR R12, R12
	XOR R13, R13
	XOR R14, R14
	XOR R15, R15
	XORPD XMM0, XMM0 # TODO: Does XMM0 hold useful information from UEFI?

	# Set the x87 FPU control word.
	SUB RSP, 2 # Make room for the FCW register (16-bits).
	FNCLEX # Clear any FPU exceptions.
	FNSTCW [RSP] # Store the FCW register's value in the stack.
	MOV WORD PTR [RSP], 0x33F # Set it to what the System V ABI demands.
	FLDCW [RSP] # Load the FCW register.
	ADD RSP, 2 # Clean-up.

	# Set the MXCSR control/status register.
	SUB RSP, 4 # Make room for the MXCSR register (32-bits).
	MOV DWORD PTR [RSP], 0x1F80 # Set it to what the System V ABI demands.
	LDMXCSR [RSP] # Load the MXCSR register.
	ADD RSP, 4 # Clean-up.

	# Set the appropriate flags in the lower RFLAGS register.
	SUB RSP, 8 # Make room for the RFLAGS register (64-bits).
	MOV WORD PTR [RSP], 0x6 # Bits 1 (reserved) and 2 (PF/even parity) set.
	POPFQ # Clean 8-bytes off the stack and into the RFLAGS register.

	# Begin to enter HAVK.
	.EXTERN havk # Not required for GAS. Just compatibility with NASM.
	CALL havk

	# HAVK should not exit like this in a version at or above V00-01-00.
	# It should handle a shutdown properly instead.
	MOV AL, 0xFE # Command byte for the PS/2 controller to reset the CPU.
	.shutdown_spin: # Endless loop. Reset the CPU twice if required.
		OUT 0x64, AL # 8042 PS/2 controller's CPU reset line pulsed.
		HLT
		CLI # The next halt will be forever (unless an NMI is raised).
		JMP .shutdown_spin
