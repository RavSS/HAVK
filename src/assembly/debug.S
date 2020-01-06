###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- debug.S                                                ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__breakpoint
# ()
assembly__breakpoint:
	XCHG BX, BX # For the Bochs magic break.
	INT 0x3 # Breakpoint trap. Goes to ISR handler 3.
	RET
