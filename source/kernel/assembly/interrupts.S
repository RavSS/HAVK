###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- interrupts.S                                           ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

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
# the interrupt frame.
.MACRO DEFINE_INTERRUPT_HANDLER_STUB VECTOR:req ERROR_CODE_SUPPORT
	.IFNDEF assembly__interrupt_handler_stub_\VECTOR
		.GLOBAL assembly__interrupt_handler_stub_\VECTOR
		assembly__interrupt_handler_stub_\VECTOR:
			PUSH RBP
			MOV RBP, RSP # So it's easier to get the arguments.

			# These are used for zeroing out the LAPIC EOI register
			# via `WRMSR`, so it's pushed first.
			PUSH RCX
			PUSH RDX
			PUSH RAX

			PUSH R11
			PUSH R10
			PUSH R9
			PUSH R8
			PUSH RDI
			PUSH RSI

			# I'm not quite sure how costly this is, but it allows
			# the actual part of the handlers to use SSE
			# instructions that don't touch the YMM and ZMM
			# variants (if present). I've used the aligned version
			# of `MOVDQU` as we have a 16-byte aligned stack, so
			# hopefully it's faster. GCC doesn't support this, so
			# just for show, I will. Note that an extra 8 bytes is
			# subtracted for alignment so `MOVDQA` (faster) can be
			# used.
			# TODO: Maybe replace this with `FXSAVE` etc.
			SUB RSP, 16 * 17 + 8
			MOVDQA XMMWORD [RSP], XMM0
			MOVDQA XMMWORD [RSP + 16], XMM1
			MOVDQA XMMWORD [RSP + 16 * 2], XMM2
			MOVDQA XMMWORD [RSP + 16 * 3], XMM3
			MOVDQA XMMWORD [RSP + 16 * 4], XMM4
			MOVDQA XMMWORD [RSP + 16 * 5], XMM5
			MOVDQA XMMWORD [RSP + 16 * 6], XMM6
			MOVDQA XMMWORD [RSP + 16 * 7], XMM7
			MOVDQA XMMWORD [RSP + 16 * 8], XMM8
			MOVDQA XMMWORD [RSP + 16 * 9], XMM9
			MOVDQA XMMWORD [RSP + 16 * 10], XMM10
			MOVDQA XMMWORD [RSP + 16 * 11], XMM11
			MOVDQA XMMWORD [RSP + 16 * 12], XMM12
			MOVDQA XMMWORD [RSP + 16 * 13], XMM13
			MOVDQA XMMWORD [RSP + 16 * 14], XMM14
			MOVDQA XMMWORD [RSP + 16 * 15], XMM15

			# The ISR can't make an assumption about the direction
			# flag. GCC also clears it before doing anything else.
			CLD # READ: https://reviews.llvm.org/D18725

			CALL assembly__switch_to_kernel_cr3

			# Now prepare the arguments, with RDI being the pointer
			# to the interrupt frame and RSI holding the value of
			# the potential error code. The latter is on top of the
			# stack if it's present and it is an 8-byte value, even
			# though it only goes up to 32 bits. They are present
			# on the stack upon entry.

			.IF ERROR_CODE_SUPPORT == 1
				MOV RSI, [RBP] # Error code value on the stack.
				LEA RDI, [RBP + 8] # Interrupt frame address.
			.ELSE
				MOV RDI, RBP # Interrupt frame only.
			.ENDIF

			CALL ada__interrupt_handler_\VECTOR

			CALL assembly__switch_to_task_cr3

			MOVDQA XMM15, XMMWORD [RSP + 16 * 15]
			MOVDQA XMM14, XMMWORD [RSP + 16 * 14]
			MOVDQA XMM13, XMMWORD [RSP + 16 * 13]
			MOVDQA XMM12, XMMWORD [RSP + 16 * 12]
			MOVDQA XMM11, XMMWORD [RSP + 16 * 11]
			MOVDQA XMM10, XMMWORD [RSP + 16 * 10]
			MOVDQA XMM9, XMMWORD [RSP + 16 * 9]
			MOVDQA XMM8, XMMWORD [RSP + 16 * 8]
			MOVDQA XMM7, XMMWORD [RSP + 16 * 7]
			MOVDQA XMM6, XMMWORD [RSP + 16 * 6]
			MOVDQA XMM5, XMMWORD [RSP + 16 * 5]
			MOVDQA XMM4, XMMWORD [RSP + 16 * 4]
			MOVDQA XMM3, XMMWORD [RSP + 16 * 3]
			MOVDQA XMM2, XMMWORD [RSP + 16 * 2]
			MOVDQA XMM1, XMMWORD [RSP + 16]
			MOVDQA XMM0, XMMWORD [RSP]
			ADD RSP, 16 * 17 + 8

			POP RSI # Recover these first before signalling EOI.
			POP RDI
			POP R8
			POP R9
			POP R10
			POP R11

			# Reset the LAPIC by writing zero to the EOI MSR.
			XOR RAX, RAX
			XOR RDX, RDX
			MOV RCX, 0x80B # The EOI MSR index.
			WRMSR

			POP RAX # Now restore them.
			POP RDX
			POP RCX

			POP RBP # No need to recover RSP, RFLAGS, etc.

			.IF ERROR_CODE_SUPPORT == 1
				ADD RSP, 8 # Take the error code off.
			.ENDIF

			REX.W IRET
	.ENDIF
.ENDM

# Unfortunately, the "INT" instruction can only take in a byte constant (imm8),
# so I have to declare a function for every single interrupt I wish to raise.
# TODO: First argument is the padded number for the label, while the second
# argument is the non-padded value so GAS recognises it. Not sure how to handle
# it all in the macro.
.MACRO DEFINE_RAISE_INTERRUPT_FUNCTION DISPLAY_VECTOR:req INT_VECTOR:req
	.IFNDEF assembly__raise_interrupt_\DISPLAY_VECTOR
		.GLOBAL assembly__raise_interrupt_\DISPLAY_VECTOR
		assembly__raise_interrupt_\DISPLAY_VECTOR:
			INT \INT_VECTOR
			RET
	.ENDIF
.ENDM

.SECTION .isolated_text

.GLOBAL assembly__interrupt_handler_spurious
assembly__interrupt_handler_spurious:
	REX.W IRET # TODO: Do I need to signal EOI?

# HAVK-specific interrupt handler stubs.
DEFINE_INTERRUPT_HANDLER_STUB 048 # LAPIC timer.

# CPU exception handler stubs.
DEFINE_INTERRUPT_HANDLER_STUB 000
DEFINE_INTERRUPT_HANDLER_STUB 001
DEFINE_INTERRUPT_HANDLER_STUB 002
DEFINE_INTERRUPT_HANDLER_STUB 003
DEFINE_INTERRUPT_HANDLER_STUB 004
DEFINE_INTERRUPT_HANDLER_STUB 005
DEFINE_INTERRUPT_HANDLER_STUB 006
DEFINE_INTERRUPT_HANDLER_STUB 007
DEFINE_INTERRUPT_HANDLER_STUB 008 1
DEFINE_INTERRUPT_HANDLER_STUB 009
DEFINE_INTERRUPT_HANDLER_STUB 010 1
DEFINE_INTERRUPT_HANDLER_STUB 011 1
DEFINE_INTERRUPT_HANDLER_STUB 012 1
DEFINE_INTERRUPT_HANDLER_STUB 013 1
DEFINE_INTERRUPT_HANDLER_STUB 014 1
DEFINE_INTERRUPT_HANDLER_STUB 015
DEFINE_INTERRUPT_HANDLER_STUB 016
DEFINE_INTERRUPT_HANDLER_STUB 017 1
DEFINE_INTERRUPT_HANDLER_STUB 018
DEFINE_INTERRUPT_HANDLER_STUB 019
DEFINE_INTERRUPT_HANDLER_STUB 020
DEFINE_INTERRUPT_HANDLER_STUB 021
DEFINE_INTERRUPT_HANDLER_STUB 022
DEFINE_INTERRUPT_HANDLER_STUB 023
DEFINE_INTERRUPT_HANDLER_STUB 024
DEFINE_INTERRUPT_HANDLER_STUB 025
DEFINE_INTERRUPT_HANDLER_STUB 026
DEFINE_INTERRUPT_HANDLER_STUB 027
DEFINE_INTERRUPT_HANDLER_STUB 028
DEFINE_INTERRUPT_HANDLER_STUB 029
DEFINE_INTERRUPT_HANDLER_STUB 030 1
DEFINE_INTERRUPT_HANDLER_STUB 031

# ISA IRQ handler stubs.
DEFINE_INTERRUPT_HANDLER_STUB 032
DEFINE_INTERRUPT_HANDLER_STUB 033
DEFINE_INTERRUPT_HANDLER_STUB 034
DEFINE_INTERRUPT_HANDLER_STUB 035
DEFINE_INTERRUPT_HANDLER_STUB 036
DEFINE_INTERRUPT_HANDLER_STUB 037
DEFINE_INTERRUPT_HANDLER_STUB 038
DEFINE_INTERRUPT_HANDLER_STUB 039
DEFINE_INTERRUPT_HANDLER_STUB 040
DEFINE_INTERRUPT_HANDLER_STUB 041
DEFINE_INTERRUPT_HANDLER_STUB 042
DEFINE_INTERRUPT_HANDLER_STUB 043
DEFINE_INTERRUPT_HANDLER_STUB 044
DEFINE_INTERRUPT_HANDLER_STUB 045
DEFINE_INTERRUPT_HANDLER_STUB 046
DEFINE_INTERRUPT_HANDLER_STUB 047

# For raising the context switch function.
DEFINE_RAISE_INTERRUPT_FUNCTION 049 49
