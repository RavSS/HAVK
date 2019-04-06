SECTION .bss
ALIGN 16
stack_bottom: RESB 16384
stack_top: ALIGN 4

SECTION .text
GLOBAL entry:function (entry.exit - entry)
entry:
	EXTERN main
	CALL main

	.exit:
