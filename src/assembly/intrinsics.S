###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- intrinsics.S                                           ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__output_byte
# (RDI => port, RSI => value)
assembly__output_byte:
	MOV DX, DI
	MOV AL, SIL
	OUT DX, AL
	RET

.GLOBAL assembly__output_word
# (RDI => port, RSI => value)
assembly__output_word:
	MOV DX, DI
	MOV AX, SI
	OUT DX, AX
	RET

.GLOBAL assembly__input_byte
# (RDI => port) >> (RAX => inputted byte)
assembly__input_byte:
	XOR RAX, RAX
	MOV DX, DI
	IN AL, DX
	RET

.GLOBAL assembly__input_word
# (RDI => port) >> (RAX => inputted word)
assembly__input_word:
	XOR RAX, RAX
	MOV DX, DI
	IN AX, DX
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

.PUSHSECTION .text

# Check the record of the "CPUID_return_values" type in the CPUID intrinsics
# package before you modify this assembly structure.
.STRUCT 0
	PASSED_LEAF:
.STRUCT 4
	PASSED_ARGUMENT:
.STRUCT 8
	EAX_INFORMATION:
.STRUCT 12
	EBX_INFORMATION:
.STRUCT 16
	ECX_INFORMATION:
.STRUCT 20
	EDX_INFORMATION:
.STRUCT 24

.POPSECTION

.GLOBAL assembly__cpuid
# (RDI => pointer to structure that we must fill, RSI => CPUID leaf,
#  RDX => CPUID leaf argument)
assembly__cpuid:
	# Using the actual registers instead of smaller registers is faster.
	# Use them when you know the higher bits are cleared etc.
	MOV RAX, RSI
	MOV R8, RBX # Save RBX to R8, as RBX's value is callee-saved.
	XOR RBX, RBX
	MOV RCX, RDX
	XOR RDX, RDX

	MOV DWORD PTR [RDI + PASSED_LEAF], EAX # Store the leaf number.
	MOV DWORD PTR [RDI + PASSED_ARGUMENT], ECX # Store the leaf argument.

	CPUID # RAX, RBX, RCX, and RDX have been clobbered upon execution.

	# Now we store the actual information returned by CPUID.
	MOV DWORD PTR [RDI + EAX_INFORMATION], EAX
	MOV DWORD PTR [RDI + EBX_INFORMATION], EBX
	MOV DWORD PTR [RDI + ECX_INFORMATION], ECX
	MOV DWORD PTR [RDI + EDX_INFORMATION], EDX

	MOV RBX, R8 # Fully restore RBX.
	RET

# This is a cut-down version of the CPUID function so we can prove that I
# never call `CPUID` with an invalid or unsupported leaf value and then use
# some random values returned by the processor.
.GLOBAL assembly__cpuid_highest_leaves
# (RDI => normal leaf mask, RSI => extended leaf mask) >>
# (RAX => highest normal leaf in lower 32 bits and highest extended leaf in
#  higher 32 bits)
assembly__cpuid_highest_leaves:
	# Do not bother checking for `CPUID` instruction support.
	MOV R8, RBX # Save RBX to R8, as RBX's value is callee-saved.

	MOV RAX, 0x80000000 # Highest extended function parameter leaf.
	CPUID # Highest leaf is in RAX.
	AND RAX, RSI # Apply the extended leaf mask.
	SHL RAX, 32 # Shift it up outside EAX.
	MOV R9, RAX # Temporarily store it in R9 so it doesn't get clobbered.

	XOR RAX, RAX # Highest function parameter leaf.
	CPUID # Highest leaf is in RAX as well. Previous leaf limit clobbered.
	AND RAX, RDI # Apply the normal leaf mask.

	ADD RAX, R9 # Now add the previously shifted CPUID value for return.

	MOV RBX, R8 # Fully restore RBX.
	RET
