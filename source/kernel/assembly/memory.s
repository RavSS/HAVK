###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- memory.s                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020-2021                ##
###############################################################################

.SECTION .text

# Allocates memory. Avoid using this for any Ada type which cannot be
# represented as all zeros in memory due to constraints, predicates, etc.
.GLOBAL assembly__memory_allocator
.TYPE assembly__memory_allocator, @function
# (RDI => double pointer, RSI => allocation in bytes)
assembly__memory_allocator:
	PUSH RDI # Save the double pointer to the stack.
	MOV RDI, RSI # Pass the size.
	CALL __gnat_malloc
	POP RDI # Recover the double pointer.
	MOV QWORD PTR [RDI], RAX # Now store the allocated pointer.
	RET

# This is the same as the other memory allocator wrapper, but it's designed
# for discriminant records in particular. You cannot assign values to a
# discriminant component of a record, so this is required. The discriminant
# must be at the beginning of the record and it must be eight bytes.
.GLOBAL assembly__memory_allocator_discriminant_record
.TYPE assembly__memory_allocator_discriminant_record, @function
# (RDI => double pointer with discriminant for the byte size at the start of
#  the record, RSI => allocation in bytes)
assembly__memory_allocator_discriminant_record:
	PUSH RDI # Save the passed arguments.
	PUSH RSI
	CALL assembly__memory_allocator
	POP RSI # Recover them in-case the above changed them.
	POP RDI

	CMP QWORD PTR [RDI], 0x0 # Check if the pointer equals null.
	JE .L_failed_allocation # If it does, then do not dereference it.

	MOV RAX, [RDI] # Dereference the pointer to the allocation pointer.
	MOV [RAX], RSI # Now store the allocation size in the record's start.

	.L_failed_allocation:

	RET
