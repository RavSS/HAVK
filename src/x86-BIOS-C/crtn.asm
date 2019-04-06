SECTION .init
	; crtend.o's SECTION .init gets added here.

	POP EBP ; Stack destroyed.
	RET

SECTION .fini
	; crtend.o's SECTION .fini gets added here.

	POP EBP ; Stack destroyed.
	RET
