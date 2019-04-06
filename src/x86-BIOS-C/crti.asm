; Constants for the kernel addresses.
KERNEL EQU 0xC0100000
VIRADDR_START EQU (KERNEL - 0x00100000)
FREQUENCY_DIVIDER EQU 100 ; Seems to be an acceptable tick rate.

; Information on paging:
; https://wiki.osdev.org/Paging
; https://wiki.osdev.org/images/7/77/Paging_Structure.gif
; https://wiki.osdev.org/Higher_Half_x86_Bare_Bones
; https://github.com/littleosbook/littleosbook/blob/master/paging.md
SECTION .data
ALIGN 4 ; Directory must be page aligned too.
GLOBAL page_directory_boot
page_directory_boot:
	; An x86 page directory has 1024 entries, each are 4 KiB aligned.
	DD 0x00000083 ; Identity map the first 4 MiB. Unmap it later.
	TIMES (768 - 1) DD 0x0
	; Base set to 0xC0000000 (768 * 4 MiB = 3072 MiB, 3 GiB)
	DD 0x00000083 ; Map the kernel.
	TIMES (256 - 1) DD 0x0
_pit_initialization_error:
	DB "PIT failed to initialize.", 0x0
_unintended_clean_exit:
	DB "The kernel has exited cleanly, which should not occur.", 0x0

SECTION .init
GLOBAL _init:function
_init:
	PUSH EBP
	MOV EBP, ESP ; Stack established.

	.paging_initialize:
		; Enable paging.
		LEA ECX, [page_directory_boot]; Load the boot directory address.
		; Calculating the subtraction above makes NASM complain.
		SUB ECX, VIRADDR_START
		MOV CR3, ECX

		; Enable 4 MiB page table entries (extended) instead of 4 KiB.
		MOV ECX, CR4
		OR ECX, 0x00000010
		MOV CR4, ECX

		MOV ECX, CR0
		; Paging enabled (8) with write-protect bit (1).
		OR ECX, 0x80010000
		MOV CR0, ECX

	.screen_clear:
		EXTERN TERMINAL_FOREGROUND
		EXTERN TERMINAL_BACKGROUND
		MOV DWORD [TERMINAL_FOREGROUND], 10 ; Light green.
		MOV DWORD [TERMINAL_BACKGROUND], 1 ; Blue.

		; Setup arguments for `vga_clear()`.
		; System V ABI mandates that they're pushed in reverse.
		MOV EDI, [TERMINAL_BACKGROUND]
		PUSH EDI
		MOV EDI, [TERMINAL_FOREGROUND]
		PUSH EDI
		MOV EDI, 0x0 ; Null character.
		PUSH EDI
		; Clear the entire screen.
		MOV EDI, 0xFF ; Over max height.
		PUSH EDI
		MOV EDI, 0xFF ; Over max width.
		PUSH EDI

		EXTERN vga_clear
		CALL vga_clear ; Begin clearing the screen.

		; Cleanup. Used EDI register prior to make it clearer.
		TIMES 5 POP EDI

	.pic_initialize:
		EXTERN pic_initialize_remap
		CALL pic_initialize_remap ; Call this here to remap IRQs.

	.pit_initialize:
		MOV EDI, FREQUENCY_DIVIDER
		PUSH EDI ; Onto the stack.

		EXTERN pit_initialize_timer
		CALL pit_initialize_timer ; Call this to setup the timer.

		POP EDI ; Cleanup.
		CMP EAX, 0 ; Compare the return value to 0 (false and error).
		JNZ .initialization_end ; Jump if Not Zero (false), no error.

	.pit_error:
		MOV EDI, _pit_initialization_error ; Move address to EDI.
		PUSH EDI
		EXTERN emergency
		CALL emergency ; Raises a seriously terrible error.
		HLT
		POP EDI
		; This jump should never occur, as emergency() is
		; not recoverable for the time being.
		JMP .pit_error

	.initialization_end:

		; crtbegin.o's SECTION .init gets added here.

SECTION .fini
GLOBAL _fini:function
_fini:
	PUSH EBP
	MOV EBP, ESP ; Stack established.

	; The kernel should never exit like this, so raise a warning
	; indicating that something bizarre has happened.
	MOV EDI, _unintended_clean_exit
	PUSH EDI

	EXTERN emergency
	CALL emergency

	; crtbegin.o's SECTION .fini gets added here.
