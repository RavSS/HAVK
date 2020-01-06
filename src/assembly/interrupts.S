###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- interrupts.S                                           ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

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
