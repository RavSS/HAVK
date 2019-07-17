SECTION .text

; A simple C-style null terminated string length finder.
GLOBAL strlen:function (strlen.end - strlen)
strlen:
	; Forget the stack frame pointer.
	; The address should be in the RDI register.
	XOR RAX, RAX ; RAX will be used to store the string length.
	.compare:
		CMP BYTE [RDI], 0x0 ; Compare what's at RDI with a null byte.
		JZ .return ; Jump to return if the length has been found.
		; What's better today on CPUs? `INC r64` or `ADD r64, 1`?
		; Does the difference of modifying flags still matter?
		INC RDI ; Destination address.
		INC RAX ; String length.
		JMP .compare ; Jump back if we're still counting.
	.return:
		; The return value (length) is in RAX (System V x86-64 ABI).
		RETQ
	.end:
