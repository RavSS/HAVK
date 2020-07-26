###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- descriptors.s                                          ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__load_global_descriptor_table
.TYPE assembly__load_global_descriptor_table, @function
# (RDI => address of GDT, RSI => CS descriptor offset,
#  RDX => DS descriptor offset, RCX => TSS descriptor offset)
assembly__load_global_descriptor_table:
	PUSH RBP
	MOV RBP, RSP

	LGDT [RDI]
	LTR CX # May as well do this here too.

	# This gets more complex due to long mode changes. I can't do
	# the easy x86 way of doing my far jump e.g. `JMP 0x8: 0x1337`, as
	# that is unfortunately invalid in x86-64. So that means I
	# cannot do a direct memory jump at all, and I will have to
	# do an indirect memory jump. `JMP 'ptr16: 64'` isn't a thing.
	# `JMP 'ptr16: 32'` and `JMP 'ptr16: 16'` aren't possible anymore.

	# Push the CS descriptor offset first. Must be two bytes.
	PUSH SI

	# Push the 8-byte address of the local symbol to set the CS register.
	LEA RAX, [RIP + .L_reload_segments]
	PUSH RAX

	# GAS requires you to be rather precise. Meanwhile, the equivalent in
	# NASM is just `JMP FAR [RSP]`, which then assembles into this.
	REX.W JMP FWORD PTR [RSP]

	.L_reload_segments:
		# All the other segment registers have the same offset as DS.
		MOV DS, DX
		MOV ES, DX
		MOV FS, DX
		MOV GS, DX
		MOV SS, DX

	# Clean up the stack and return.
	MOV RSP, RBP
	POP RBP
	RET

.GLOBAL assembly__load_interrupt_descriptor_table
.TYPE assembly__load_interrupt_descriptor_table, @function
# (RDI => address of IDT)
assembly__load_interrupt_descriptor_table:
	LIDT [RDI]
	RET

# NOTE: This isn't really needed or even called from Ada, I've just been toying
# around with ring 3 and this showcases it, so I've kept it for reference.
# READ: https://wiki.osdev.org/Getting_to_Ring_3#Entering_Ring_3
.GLOBAL assembly__user_mode_call
.TYPE assembly__user_mode_call, @function
# (RDI => pre-modified CS descriptor index for user mode,
#  RSI => pre-modified DS descriptor index for user mode,
#  RDX => address of the function to call in user mode)
assembly__user_mode_call:
	MOV DS, SI # Load the DS descriptor index into the segment registers.
	MOV ES, SI
	MOV FS, SI
	MOV GS, SI
	# SS is automatically handled.

	# The 64-bit IRET (IRETQ) instruction pops off segment selectors
	# as 64-bit values and ignores the higher 48 bits. Similarly, it only
	# uses 16-bits of the RFLAGS register. See all the microcode operations
	# for the instruction in any x86 reference manual.

	MOV RCX, RSP

	PUSH RSI # Push the DS selector.
	PUSH RCX # Push the stack.
	PUSHFQ # Push RFLAGS. The mnemonic doesn't matter without prefixes.
	PUSH RDI # Push the CS selector.
	PUSH RDX # Push the function address.

	REX.W IRET
