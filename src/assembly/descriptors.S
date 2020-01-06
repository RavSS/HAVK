###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- descriptors.S                                          ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__load_global_descriptor_table
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
	LEA RAX, [RIP + .reload_segments]
	PUSH RAX

	# GAS requires you to be rather precise. Meanwhile, the equivalent in
	# NASM is just `JMP FAR [RSP]`, which then assembles into this.
	REX.W JMP FWORD PTR [RSP]

	.reload_segments:
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
# (RDI => address of IDT)
assembly__load_interrupt_descriptor_table:
	LIDT [RDI]
	RET
