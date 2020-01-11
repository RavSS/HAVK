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

.GLOBAL assembly__write_model_specific_register
# (RDI => model-specific register, RSI => value)
assembly__write_model_specific_register:
	MOV ECX, EDI # The MSR's index is only 32 bits.
	# WRMSR works as "MSRs[ECX] = EDX:EAX".
	MOV EAX, ESI
	SHR RSI, 32 # Replace the lower 32 bits with the higher 32 bits.
	MOV EDX, ESI
	WRMSR
	RET

.GLOBAL assembly__read_model_specific_register
# (RDI => model-specific register) >> (RAX => value)
assembly__read_model_specific_register:
	MOV ECX, EDI
	# RDMSR works as "EDX:EAX = MSRs[ECX]".
	RDMSR
	SHL RDX, 32 # Move it up 32 bits.
	ADD RAX, RDX # Add it to the lower 32 bits.
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
