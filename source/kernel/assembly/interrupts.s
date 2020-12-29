###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- interrupts.s                                           ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.INCLUDE "macros.s"

# The GCC attribute "interrupt" when applied to an Ada procedure with the
# "Machine_Attribute" pragma is finnicky and often breaks completely on
# certain optimisation levels. It's also not fully guaranteed to expect a CR3
# switch to happen before certain kernel addresses are touched, especially for
# the stack protector. This is a more primitive way of creating ISRs, but it
# allows finer control. It expects e.g. `ada__interrupt_handler_042` to be
# defined for interrupt vector 42 if "042" is passed as the "VECTOR" parameter.
# Should be placed into the isolated text section for obvious reasons.
# If this is for a CPU exception which passes an error code, the second
# parameter must be specified or else `REX.W IRQ` will mistake it as a part of
# the interrupt frame. Third parameter decides whether or not to signal EOI.
.MACRO INTERRUPT_HANDLER_STUB VECTOR:req ERROR_CODE=0 RESET_LAPIC=0
	.IF \VECTOR < 10 # Padding so it remains consistent with my Ada code.
		.GLOBAL assembly__interrupt_handler_stub_00\VECTOR
		.TYPE assembly__interrupt_handler_stub_00\VECTOR, @function
		assembly__interrupt_handler_stub_00\VECTOR:
	.ELSEIF \VECTOR < 100
		.GLOBAL assembly__interrupt_handler_stub_0\VECTOR
		.TYPE assembly__interrupt_handler_stub_0\VECTOR, @function
		assembly__interrupt_handler_stub_0\VECTOR:
	.ELSEIF \VECTOR < 256
		.GLOBAL assembly__interrupt_handler_stub_\VECTOR
		.TYPE assembly__interrupt_handler_stub_\VECTOR, @function
		assembly__interrupt_handler_stub_\VECTOR:
	.ELSE
		.ERROR "the provided vector is too high"
	.ENDIF
		PUSH RBP
		MOV RBP, RSP # Easier to get the arguments and original RSP.
		AND RSP, -16 # Align it for faster XMM register storage.

		# These are used for zeroing out the LAPIC EOI register
		# via `WRMSR`, so it's pushed first. Doesn't matter if
		# EOI isn't signalled.
		PUSH RCX
		PUSH RDX
		PUSH RAX

		PUSH R11
		PUSH R10
		PUSH R9
		PUSH R8
		PUSH RDI
		PUSH RSI

		# Note that an extra 8 bytes is subtracted for alignment so
		# `MOVDQA` (faster) can be used.
		# TODO: Maybe replace this with `FXSAVE` etc.
		SUB RSP, 16 * 17 + 8
		M_SAVE_XMM_REGISTERS RSP
		SUB RSP, 8 * 8 # Make space for the MMX registers.
		M_SAVE_MMX_REGISTERS RSP

		# The ISR can't make an assumption about the direction
		# flag. GCC also clears it before doing anything else.
		CLD # READ: https://reviews.llvm.org/D18725

		# Switch to the kernel's page layout.
		MOV RAX, [RIP + assembly__kernel_page_map_base_address]
		MOV CR3, RAX

		# Now increment the counter used for various statistics.
		LEA RAX, [RIP + ada__interrupt_counters]
		LOCK INC QWORD PTR [RAX + 8 * \VECTOR] # 8-byte elements.

		# Now prepare the arguments, with RDI being the pointer
		# to the interrupt frame and RSI holding the value of
		# the potential error code. The latter is on top of the
		# stack if it's present and it is an 8-byte value, even
		# though it only goes up to 32 bits. Note that I push
		# the RBP first, so they're on top of the stack upon
		# entry and not after the CPU executes `PUSH RBP`.

		.IF \ERROR_CODE
			MOV RSI, [RBP + 8] # Error code value.
			LEA RDI, [RBP + 16] # Interrupt frame address.
		.ELSE
			LEA RDI, [RBP + 8] # Interrupt frame only.
		.ENDIF

		.IF \VECTOR < 10 # Same padding as above.
			CALL ada__interrupt_handler_00\VECTOR
		.ELSEIF \VECTOR < 100
			CALL ada__interrupt_handler_0\VECTOR
		.ELSE
			CALL ada__interrupt_handler_\VECTOR
		.ENDIF

		# Switch to the appropriate page layout if tasking demands it.
		CMP BYTE PTR [RIP + ada__tasking_enabled], 0
		JZ 1f
		# Tasking is enabled if this is reached, so we obtain the
		# register state to load the right CR3 value.
		CALL ada__get_active_task_state
		MOV RAX, [RAX + CR3_STATE]
		MOV CR3, RAX
		1:

		M_LOAD_MMX_REGISTERS RSP
		ADD RSP, 8 * 8
		M_LOAD_XMM_REGISTERS RSP
		ADD RSP, 16 * 17 + 8

		POP RSI # Recover these first before signalling EOI.
		POP RDI
		POP R8
		POP R9
		POP R10
		POP R11

		.IF \RESET_LAPIC
			M_LAPIC_EOI
		.ENDIF

		POP RAX # Now restore them.
		POP RDX
		POP RCX

		MOV RSP, RBP
		POP RBP # No need to recover ring 3 RSP, RFLAGS, etc.

		.IF \ERROR_CODE
			ADD RSP, 8 # Take the error code off.
		.ENDIF

		REX.W IRET
.ENDM

# Unfortunately, the "INT" instruction can only take in a byte constant (imm8),
# so I have to declare a function for every single interrupt I wish to raise.
.MACRO RAISE_INTERRUPT_FUNCTION VECTOR:req
	.IF \VECTOR < 10 # Padding so it remains consistent with my Ada code.
		.GLOBAL assembly__raise_interrupt_00\VECTOR
		.TYPE assembly__raise_interrupt_00\VECTOR, @function
		assembly__raise_interrupt_00\VECTOR:
	.ELSEIF \VECTOR < 100
		.GLOBAL assembly__raise_interrupt_0\VECTOR
		.TYPE assembly__raise_interrupt_0\VECTOR, @function
		assembly__raise_interrupt_0\VECTOR:
	.ELSEIF \VECTOR < 256
		.GLOBAL assembly__raise_interrupt_\VECTOR
		.TYPE assembly__raise_interrupt_\VECTOR, @function
		assembly__raise_interrupt_\VECTOR:
	.ELSE
		.ERROR "the provided vector is too high"
	.ENDIF
		INT \VECTOR
		RET
.ENDM

.SECTION .isolated_text

.GLOBAL assembly__interrupt_handler_spurious
.TYPE assembly__interrupt_handler_spurious, @function
assembly__interrupt_handler_spurious:
	REX.W IRET # TODO: Do I need to signal EOI?

# CPU exception handler stubs.
INTERRUPT_HANDLER_STUB 0 0
INTERRUPT_HANDLER_STUB 1 0
INTERRUPT_HANDLER_STUB 2 0
INTERRUPT_HANDLER_STUB 3 0
INTERRUPT_HANDLER_STUB 4 0
INTERRUPT_HANDLER_STUB 5 0
INTERRUPT_HANDLER_STUB 6 0
INTERRUPT_HANDLER_STUB 7 0
INTERRUPT_HANDLER_STUB 8 1
INTERRUPT_HANDLER_STUB 9 0
INTERRUPT_HANDLER_STUB 10 1
INTERRUPT_HANDLER_STUB 11 1
INTERRUPT_HANDLER_STUB 12 1
INTERRUPT_HANDLER_STUB 13 1
INTERRUPT_HANDLER_STUB 14 1
INTERRUPT_HANDLER_STUB 15 0
INTERRUPT_HANDLER_STUB 16 0
INTERRUPT_HANDLER_STUB 17 1
INTERRUPT_HANDLER_STUB 18 0
INTERRUPT_HANDLER_STUB 19 0
INTERRUPT_HANDLER_STUB 20 0
INTERRUPT_HANDLER_STUB 21 0
INTERRUPT_HANDLER_STUB 22 0
INTERRUPT_HANDLER_STUB 23 0
INTERRUPT_HANDLER_STUB 24 0
INTERRUPT_HANDLER_STUB 25 0
INTERRUPT_HANDLER_STUB 26 0
INTERRUPT_HANDLER_STUB 27 0
INTERRUPT_HANDLER_STUB 28 0
INTERRUPT_HANDLER_STUB 29 0
INTERRUPT_HANDLER_STUB 30 1
INTERRUPT_HANDLER_STUB 31 0

# ISA IRQ handler stubs.
INTERRUPT_HANDLER_STUB 32 0 1
INTERRUPT_HANDLER_STUB 33 0 1
INTERRUPT_HANDLER_STUB 34 0 1
INTERRUPT_HANDLER_STUB 35 0 1
INTERRUPT_HANDLER_STUB 36 0 1
INTERRUPT_HANDLER_STUB 37 0 1
INTERRUPT_HANDLER_STUB 38 0 1
INTERRUPT_HANDLER_STUB 39 0 1
INTERRUPT_HANDLER_STUB 40 0 1
INTERRUPT_HANDLER_STUB 41 0 1
INTERRUPT_HANDLER_STUB 42 0 1
INTERRUPT_HANDLER_STUB 43 0 1
INTERRUPT_HANDLER_STUB 44 0 1
INTERRUPT_HANDLER_STUB 45 0 1
INTERRUPT_HANDLER_STUB 46 0 1
INTERRUPT_HANDLER_STUB 47 0 1
