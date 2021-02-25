###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- macros.s                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020-2021                ##
###############################################################################

# This is a special file that is to be included via the `.INCLUDE` directive
# to introduce some helpful macros. It should not contain anything other than
# macros. All public macros should be prefixed with "M_*" and all
# internal/private macros should be prefixed with a single underscore. If this
# file changes, then all assembly files should be reassembled forcefully.
# Note that some macros use relative local labels (e.g. "1:") for branching
# purposes, so using e.g. "JMP 1b" after a macro might not do what you expect
# it to do.

# # # # # # # # # # # # # # # # # # Public # # # # # # # # # # # # # # # # # #

# Takes in a register which points to a 256-byte buffer. The buffer must be
# placed on a 16-byte boundary.
.MACRO M_SAVE_XMM_REGISTERS BUFFER_POINTER:req
	.ALTMACRO
	.EQU i, 0
	.REPT 16
		_SAVE_XMM_REGISTER \BUFFER_POINTER %i
		.EQU i, i + 1
	.ENDR
	.NOALTMACRO
.ENDM

# Unaligned version of `M_SAVE_XMM_REGISTERS`.
.MACRO M_UNALIGNED_SAVE_XMM_REGISTERS BUFFER_POINTER:req
	.ALTMACRO
	.EQU i, 0
	.REPT 16
		_UNALIGNED_SAVE_XMM_REGISTER \BUFFER_POINTER %i
		.EQU i, i + 1
	.ENDR
	.NOALTMACRO
.ENDM

# Takes in a register which points to a 256-byte buffer. The buffer must be
# placed on a 16-byte boundary.
.MACRO M_LOAD_XMM_REGISTERS BUFFER_POINTER:req
	.ALTMACRO
	.EQU i, 0
	.REPT 16
		_LOAD_XMM_REGISTER \BUFFER_POINTER %i
		.EQU i, i + 1
	.ENDR
	.NOALTMACRO
.ENDM

# Unaligned version of `M_LOAD_XMM_REGISTERS`.
.MACRO M_UNALIGNED_LOAD_XMM_REGISTERS BUFFER_POINTER:req
	.ALTMACRO
	.EQU i, 0
	.REPT 16
		_UNALIGNED_LOAD_XMM_REGISTER \BUFFER_POINTER %i
		.EQU i, i + 1
	.ENDR
	.NOALTMACRO
.ENDM

# Takes in a register which points to a 64-byte buffer.
.MACRO M_SAVE_MMX_REGISTERS BUFFER_POINTER:req
	.ALTMACRO
	.EQU i, 0
	.REPT 8
		_SAVE_MMX_REGISTER \BUFFER_POINTER %i
		.EQU i, i + 1
	.ENDR
	.NOALTMACRO
.ENDM

# Takes in a register which points to a 64-byte buffer.
.MACRO M_LOAD_MMX_REGISTERS BUFFER_POINTER:req
	.ALTMACRO
	.EQU i, 0
	.REPT 8
		_LOAD_MMX_REGISTER \BUFFER_POINTER %i
		.EQU i, i + 1
	.ENDR
	.NOALTMACRO
.ENDM

# Signals end-of-interrupt to the local APIC. Clobbers RAX, RCX, and RDX.
.MACRO M_LAPIC_EOI
	# Reset the LAPIC by writing zero to the EOI MSR. Not needed for CPU
	# exceptions etc.
	XOR RAX, RAX
	XOR RDX, RDX
	MOV RCX, 0x80B # The EOI MSR index.
	WRMSR
.ENDM

# This structure must match the Ada "register_state" record inside the base
# tasking package.
.STRUCT 0
	CR3_STATE:
.STRUCT CR3_STATE + 8
	CS_STATE:
.STRUCT CS_STATE + 8
	MMX_STATE:
.STRUCT MMX_STATE + 64
	R8_STATE:
.STRUCT R8_STATE + 8
	R9_STATE:
.STRUCT R9_STATE + 8
	R10_STATE:
.STRUCT R10_STATE + 8
	R11_STATE:
.STRUCT R11_STATE + 8
	R12_STATE:
.STRUCT R12_STATE + 8
	R13_STATE:
.STRUCT R13_STATE + 8
	R14_STATE:
.STRUCT R14_STATE + 8
	R15_STATE:
.STRUCT R15_STATE + 8
	RAX_STATE:
.STRUCT RAX_STATE + 8
	RBP_STATE:
.STRUCT RBP_STATE + 8
	RBX_STATE:
.STRUCT RBX_STATE + 8
	RCX_STATE:
.STRUCT RCX_STATE + 8
	RDI_STATE:
.STRUCT RDI_STATE + 8
	RDX_STATE:
.STRUCT RDX_STATE + 8
	RFLAGS_STATE:
.STRUCT RFLAGS_STATE + 8
	RIP_STATE:
.STRUCT RIP_STATE + 8
	RSI_STATE:
.STRUCT RSI_STATE + 8
	RSP_STATE:
.STRUCT RSP_STATE + 8
	SS_STATE:
.STRUCT SS_STATE + 8
	XMM_STATE: # 16-byte alignment not required.
.STRUCT XMM_STATE + 256

# Lays out an interrupt frame, along with the CR3 value and temporary registers
# space (I use RAX and RBX).
.STRUCT 0
	RBX_ON_STACK:
.STRUCT RBX_ON_STACK + 8
	CR3_ON_STACK:
.STRUCT CR3_ON_STACK + 8
	RAX_ON_STACK:
.STRUCT RAX_ON_STACK + 8
	RIP_ON_STACK:
.STRUCT RIP_ON_STACK + 8
	CS_ON_STACK:
.STRUCT CS_ON_STACK + 8
	RFLAGS_ON_STACK:
.STRUCT RFLAGS_ON_STACK + 8
	RSP_ON_STACK:
.STRUCT RSP_ON_STACK + 8
	SS_ON_STACK:
.STRUCT SS_ON_STACK + 8

# Transfers the registers which are on the stack as seen above.
.MACRO M_STACK_TRANSFER OPERATION:req REGISTER_NAME:req MOVCC=MOV
	.IFC \OPERATION, LOAD
		\MOVCC RBX, [RAX + \REGISTER_NAME\()_STATE]
		\MOVCC [RSP + \REGISTER_NAME\()_ON_STACK], RBX
		.EXITM
	.ENDIF

	.IFC \OPERATION, SAVE
		\MOVCC RBX, [RSP + \REGISTER_NAME\()_ON_STACK]
		\MOVCC [RAX + \REGISTER_NAME\()_STATE], RBX
		.EXITM
	.ENDIF

	.ERROR "the stack transfer operation is neither SAVE nor LOAD"
.ENDM

# Transfers integer registers and the SIMD registers (limited to MMX and XMM).
.MACRO M_REGISTER_TRANSFER OPERATION:req REGISTER_NAME:req MOVCC=MOV
	.IFC \OPERATION, LOAD
		.IFC \REGISTER_NAME, XMM
			LEA RBX, [RAX + XMM_STATE]
			M_UNALIGNED_LOAD_XMM_REGISTERS RBX
			.EXITM
		.ENDIF

		.IFC \REGISTER_NAME, MMX
			LEA RBX, [RAX + MMX_STATE]
			M_LOAD_MMX_REGISTERS RBX
			.EXITM
		.ENDIF

		\MOVCC \REGISTER_NAME, [RAX + \REGISTER_NAME\()_STATE]
		.EXITM
	.ENDIF

	.IFC \OPERATION, SAVE
		.IFC \REGISTER_NAME, XMM
			LEA RBX, [RAX + XMM_STATE]
			M_UNALIGNED_SAVE_XMM_REGISTERS RBX
			.EXITM
		.ENDIF

		.IFC \REGISTER_NAME, MMX
			LEA RBX, [RAX + MMX_STATE]
			M_SAVE_MMX_REGISTERS RBX
			.EXITM
		.ENDIF

		\MOVCC [RAX + \REGISTER_NAME\()_STATE], \REGISTER_NAME
		.EXITM
	.ENDIF

	.ERROR "the register transfer operation is neither SAVE nor LOAD"
.ENDM

# This macro saves the current task's state to the state structure that RAX
# points to. The stack structure must also be at the address RSP is pointing to
# and RBX is clobbered. "OPERATION" can either be "SAVE" or "LOAD".
.MACRO M_TASK_STATE OPERATION:req STACK_TRANSFERS=1
	.IF \STACK_TRANSFERS
		M_STACK_TRANSFER \OPERATION CR3
		M_STACK_TRANSFER \OPERATION CS
		M_STACK_TRANSFER \OPERATION RAX
		M_STACK_TRANSFER \OPERATION RBX
		M_STACK_TRANSFER \OPERATION RFLAGS
		M_STACK_TRANSFER \OPERATION RIP
		M_STACK_TRANSFER \OPERATION RSP
		M_STACK_TRANSFER \OPERATION SS
	.ENDIF
	M_REGISTER_TRANSFER \OPERATION MMX
	M_REGISTER_TRANSFER \OPERATION R8
	M_REGISTER_TRANSFER \OPERATION R9
	M_REGISTER_TRANSFER \OPERATION R10
	M_REGISTER_TRANSFER \OPERATION R11
	M_REGISTER_TRANSFER \OPERATION R12
	M_REGISTER_TRANSFER \OPERATION R13
	M_REGISTER_TRANSFER \OPERATION R14
	M_REGISTER_TRANSFER \OPERATION R15
	M_REGISTER_TRANSFER \OPERATION RBP
	M_REGISTER_TRANSFER \OPERATION RCX
	M_REGISTER_TRANSFER \OPERATION RDI
	M_REGISTER_TRANSFER \OPERATION RDX
	M_REGISTER_TRANSFER \OPERATION RSI
	M_REGISTER_TRANSFER \OPERATION XMM
.ENDM

# # # # # # # # # # # # # # # # # # Private # # # # # # # # # # # # # # # # # #

.MACRO _SAVE_XMM_REGISTER BUFFER_POINTER:req XMM_INDEX:req
	MOVDQA [\BUFFER_POINTER + 16 * \XMM_INDEX], XMM\XMM_INDEX
.ENDM

.MACRO _UNALIGNED_SAVE_XMM_REGISTER BUFFER_POINTER:req XMM_INDEX:req
	MOVDQU [\BUFFER_POINTER + 16 * \XMM_INDEX], XMM\XMM_INDEX
.ENDM

.MACRO _LOAD_XMM_REGISTER BUFFER_POINTER:req XMM_INDEX:req
	MOVDQA XMM\XMM_INDEX, [\BUFFER_POINTER + 16 * \XMM_INDEX]
.ENDM

.MACRO _UNALIGNED_LOAD_XMM_REGISTER BUFFER_POINTER:req XMM_INDEX:req
	MOVDQU XMM\XMM_INDEX, [\BUFFER_POINTER + 16 * \XMM_INDEX]
.ENDM

.MACRO _SAVE_MMX_REGISTER BUFFER_POINTER:req MMX_INDEX:req
	MOVQ [\BUFFER_POINTER + 8 * \MMX_INDEX], MM\MMX_INDEX
.ENDM

.MACRO _LOAD_MMX_REGISTER BUFFER_POINTER:req MMX_INDEX:req
	MOVQ MM\MMX_INDEX, [\BUFFER_POINTER + 8 * \MMX_INDEX]
.ENDM
