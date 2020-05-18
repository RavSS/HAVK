###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- strings.S                                              ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .isolated_text

# An assembly version of `strnlen_s()`, except it takes in and returns an
# integer for length which is not negative and not unsigned instead of
# "size_t". Will stop at the first null byte, but will also not go over the
# length limit.
.GLOBAL assembly__string_length
# (RDI => pointer to null-terminated string, RSI => maximum length as a
# non-negative C integer) >> (RAX => the length as a non-negative C integer)
assembly__string_length:
	XOR RAX, RAX # RAX will be used to store the string length.

	# Add preconditions to any function imports in Ada. This shouldn't be
	# necessary, but it's here just in case.
	CMP RDI, 0x0 # Check if we were passed a null address.
	JZ .return # If RDI is zero, then return a length of zero.

	.compare:
		CMP RAX, RSI # Compare the count to the maximum length wanted.
		JGE .return # Jump to return if we're at the limit.

		CMP BYTE PTR [RDI], 0x0 # Compare what's at RDI with null.
		JZ .return # Jump to return if the length has been found.

		# Use `INC` over `ADD`, as it's a bit better in terms of size.
		# Don't need to preserve RFLAGS.
		INC RDI # Increment string address.
		INC RAX # Increment string length.

		JMP .compare # Jump back if we're still counting.
	.return:
		AND RAX, 0x7FFFFFFF # The mask is the value of C's "INT_MAX".
		RET # Return value in RAX.
