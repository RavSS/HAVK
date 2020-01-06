###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- strings.S                                              ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .text

# A simple C-style null-terminated string length finder.
.GLOBAL assembly__string_length
# (RDI => pointer to null-terminated string) >> (RAX => the length)
assembly__string_length:
	# Forget the stack frame pointer.
	XOR RAX, RAX # RAX will be used to store the string length.
	.compare:
		CMP BYTE PTR [RDI], 0x0 # Compare what's at RDI with null.
		JZ .return # Jump to return if the length has been found.
		# What's better today on CPUs? `INC r64` or `ADD r64, 1`?
		# Does the difference of modifying flags still matter?
		INC RDI # Destination address.
		INC RAX # String length.
		JMP .compare # Jump back if we're still counting.
	.return:
		# The return value (length) is in RAX (System V x86-64 ABI).
		RET
	.end:
