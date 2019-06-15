SECTION .text

; Loads the GDTR with my GDT.
GLOBAL lgdt:function (lgdt.end - lgdt)
lgdt:
	PUSH RBP
	MOV RBP, RSP

	CLI ; Make sure interrupts are indeed disabled.
	LGDT [RDI] ; Should have received an address from Ada inside RDI.

	; This gets more complex due to long mode changes. I can't do
	; the easy x86 way of doing my far jump e.g. `JMP 0x8: 0x1337`, as
	; that is unfortunately invalid in x86-64. So that means I
	; cannot do a direct memory jump at all, and I will have to
	; do an indirect memory jump. `JMP 'ptr16: 64'` isn't a thing.
	; `JMP 'ptr16: 32'` and `JMP 'ptr16: 16'` aren't possible anymore.

	; 0x8 is my code segment. Second descriptor.
	MOV RAX, 0x8
	; Note to self: don't touch RBX or you will waste 2 days debugging.
	MOV RCX, .reload_segments

	; Space used: address = 8 bytes, CS selector = 2 bytes.
	PUSH RAX
	PUSH RCX

	JMP FAR [RSP]
	.reload_segments:
		MOV DX, 0x10 ; 0x10 is my data segment. Third descriptor.
		MOV DS, DX
		MOV ES, DX
		MOV FS, DX
		MOV GS, DX
		MOV SS, DX

		MOV DX, 0x28 ; 0x28 is my TSS segment. Fifth descriptor.
		LTR DX ; May as well do this here.

		; The jump has hopefully occurred, so now we clean up
		; or else we've corrupted `Prepare_GDT`'s stack.
		POP RCX
		POP RAX
	; Clean up again, just to be very sure.
	MOV RSP, RBP
	POP RBP
	RETQ
	.end:
