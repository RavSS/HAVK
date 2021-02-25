###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- debug.s                                                ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2021                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__breakpoint
.TYPE assembly__breakpoint, @function
# ()
assembly__breakpoint:
	XCHG BX, BX # For the Bochs magic break.
	INT3 # Breakpoint trap. Goes to ISR handler 3.
	RET
