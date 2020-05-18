###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- interrupts.S                                           ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

# Unfortunately, the "INT" instruction can only take in a byte constant (imm8),
# so I have to declare a function for every single interrupt I wish to raise.
.MACRO DEFINE_INTERRUPT_FUNCTION VECTOR:req
	.IFNDEF assembly__raise_interrupt_\VECTOR
		.GLOBAL assembly__raise_interrupt_\VECTOR
		assembly__raise_interrupt_\VECTOR:
			INT \VECTOR
			RET
	.ENDIF
.ENDM

.SECTION .text

.GLOBAL assembly__pic_master_end_interrupt
# ()
assembly__pic_master_end_interrupt:
	MOV AL, 0x20
	OUT 0x20, AL
	RET

.GLOBAL assembly__pic_dual_end_interrupt
# ()
assembly__pic_dual_end_interrupt:
	MOV AL, 0x20
	OUT 0x20, AL
	OUT 0xA0, AL
	RET

.SECTION .isolated_text

DEFINE_INTERRUPT_FUNCTION 100
