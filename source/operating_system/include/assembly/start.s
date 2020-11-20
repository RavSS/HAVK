###############################################################################
## Program         -- HAVK Operating System                                  ##
## Filename        -- start.s                                                ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020                     ##
###############################################################################

.SECTION .bss

.ALIGN 16 # System V ABI dictates that x86-64 stacks are 16-byte aligned.
_stack_end:
	.SPACE 32768 # Just 32 KiB for now.
_stack_base:

.SECTION .text._start

# The below is taken from the kernel's entry file ("entry.s"). The only missing
# block is the RFLAGS initialisation, as the kernel has done that for us. This
# requires a stack before being used.
.MACRO INITIALISE_REGISTER_STATE
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

	# Set the x87 FPU control word.
	SUB RSP, 2 # Make room for the FCW register (16 bits).
	FNCLEX # Clear any FPU exceptions.
	FNSTCW [RSP] # Store the FCW register's value in the stack.
	MOV WORD PTR [RSP], 0x33F # Set it to what the System V ABI demands.
	FLDCW [RSP] # Load the FCW register.
	ADD RSP, 2 # Clean-up.

	# Set the MXCSR control/status register.
	SUB RSP, 4 # Make room for the MXCSR register (32 bits).
	MOV DWORD PTR [RSP], 0x1F80 # Set it to what the System V ABI demands.
	LDMXCSR [RSP] # Load the MXCSR register.
	ADD RSP, 4 # Clean-up.
.ENDM

.GLOBAL _start
.TYPE _start, @function
_start:
	LEA RSP, [RIP + _stack_base] # Load the stack and set the base pointer.
	MOV RBP, RSP

	INITIALISE_REGISTER_STATE

	CALL main

	MOV RDI, RAX # Store the exit code in the first argument.
	JMP exit # Now exit the task. We will not return.
