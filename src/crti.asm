; Information on paging:
; https://wiki.osdev.org/Paging
; https://wiki.osdev.org/images/7/77/Paging_Structure.gif
; https://wiki.osdev.org/Higher_Half_x86_Bare_Bones
; https://github.com/littleosbook/littleosbook/blob/master/paging.md
SECTION .data
ALIGN 4 ; Directory must be page aligned too.
GLOBAL page_directory
page_directory:
	; An x86 page directory has 1024 entries, each are 4 KiB aligned.
	DD 0x00000083 ; Identity map the first 4 MiB. Unmap it later.
	TIMES (768 - 1) DD 0x0
	DD 0x00000083 ; Map the kernel.
	TIMES (256 - 1) DD 0x0

SECTION .init
GLOBAL _init
GLOBAL _init:function
_init:
	PUSH EBP
	MOV EBP, ESP ; Stack established.

	LEA ECX, [page_directory] ; Load the directory address.
	SUB ECX, 0xC0000000 ; Calculating this above makes NASM complain.
	MOV CR3, ECX

	; Enable 4 MiB page table entries (extended) instead of 4 KiB.
	MOV ECX, CR4
	OR ECX, 0x00000010
	MOV CR4, ECX

	MOV ECX, CR0
	OR ECX, 0x80010000 ; Paging enabled (8) with write-protect bit (1).
	MOV CR0, ECX
	; crtbegin.o's SECTION .init gets added here.

SECTION .fini
GLOBAL _fini
GLOBAL _fini:function
_fini:
	PUSH EBP
	MOV EBP, ESP ; Stack established.
	; crtbegin.o's SECTION .fini gets added here.