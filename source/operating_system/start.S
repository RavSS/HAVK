###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- start.S                                                ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020                     ##
###############################################################################

.SECTION .bss

.ALIGN 16 # See the kernel's "entry.S" for more.
stack_end:
	.SPACE 8192 # Just 8 KiB for now.
stack_base:

.SECTION .text

.GLOBAL _start
_start:
	LEA RSP, [RIP + stack_base]

	CALL main

	# TODO: Make a system call for exiting a task.
	.end:
		JMP .end

