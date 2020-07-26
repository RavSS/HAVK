###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- macros.s                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020                     ##
###############################################################################

# This is a special file that is to be included via the `.INCLUDE` directive
# to introduce some helpful macros. It should not contain anything other than
# macros. All public macros should be prefixed with "M_*" and all
# internal/private macros should be prefixed with a single underscore. If this
# file changes, then all assembly files should be reassembled forcefully.

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

# Switches the page layout to the active task's page layout. Clobbers RAX and
# additionally calls other functions.
.MACRO M_SWITCH_TO_TASK_CR3
	CMP BYTE PTR [RIP + global__tasking_enabled], 0x0
	JZ 1f # If it's not enabled, then we don't do the CR3 switch.

	CALL ada__get_active_task_cr3 # Get the CR3 value and load it.
	MOV CR3, RAX
	XOR RAX, RAX # Don't leak it by accident.

	1:
.ENDM

# # # # # # # # # # # # # # # # # # Private # # # # # # # # # # # # # # # # # #

.MACRO _SAVE_XMM_REGISTER BUFFER_POINTER:req XMM_INDEX:req
	MOVDQA [\BUFFER_POINTER + 16 * \XMM_INDEX], XMM\XMM_INDEX
.ENDM

.MACRO _LOAD_XMM_REGISTER BUFFER_POINTER:req XMM_INDEX:req
	MOVDQA XMM\XMM_INDEX, [\BUFFER_POINTER + 16 * \XMM_INDEX]
.ENDM
