###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- intrinsics.S                                           ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__bit_test
# (RDI => value, RSI => bit location) >> (RAX => the bit itself as a boolean)
assembly__bit_test:
	XOR RAX, RAX
	BT RDI, RSI # Implicit REX prefix for operation on 64-bit operands.
	SETB AL
	RET

.GLOBAL assembly__output_byte
# (RDI => port, RSI => value)
assembly__output_byte:
	MOV DX, DI
	MOV AL, SIL
	OUT DX, AL
	RET

.GLOBAL assembly__input_byte
# (RDI => port) >> (RAX => inputted byte)
assembly__input_byte:
	XOR RAX, RAX
	MOV DX, DI
	IN AL, DX
	RET

.GLOBAL assembly__halt
# ()
assembly__halt:
	HLT
	RET

.GLOBAL assembly__enable_interrupts
# ()
assembly__enable_interrupts:
	STI
	RET

.GLOBAL assembly__disable_interrupts
# ()
assembly__disable_interrupts:
	CLI
	RET
