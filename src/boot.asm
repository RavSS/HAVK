; Constants for detailing the multiboot header.
MAGIC EQU 0x1BADB002
FLAGS EQU ((1 << 0) | (1 << 1))
CHECKSUM EQU -(MAGIC + FLAGS)

; Constants for the kernel addresses.
KERNEL EQU 0xC0100000
VIRADDR_START EQU (KERNEL - 0x00100000)

; Multiboot header declaring this program as a kernel. Looks for the checksum
; as detailed in the "multiboot standard".
; https://www.gnu.org/software/grub/manual/multiboot/multiboot.html
SECTION .multiboot
ALIGN 4
DD MAGIC
DD FLAGS
DD CHECKSUM

; Must create the stack ourselves. x86 stack apparently grows downwards, and it
; must be 16-byte aligned due to the System V ABI according to the example this
; is based on.
SECTION .bss
ALIGN 16
stack_bottom: RESB 16384 ; `RESB` instead of `DB` for unintialized allocation.
stack_top: ALIGN 4

SECTION .text
; "entry" is my symbol specified in the linker file for entering the kernel.
GLOBAL entry:function (entry.exit - entry)
entry:
	; Protected mode 32-bit. Only my stuff from here on, nothing else.
	; Also no interrupts or paging until enabled.
	; Here is where features should be enabled and initialized, like
	; ISA extensions, paging, floating point, GDT loading,
	; exceptions, C++ global constructors, etc. before kernel entry.

	; TODO: Get the address of the multiboot header so I can determine
	; the RAM on the current system for obvious reasons a la mapping.

	.initialize_the_kernel:
		EXTERN _init
		CALL _init

		LEA ECX, [.enter_the_kernel]
		JMP ECX

	; The kernel is linked, so now we call it to enter the kernel.
	.enter_the_kernel:
		EXTERN page_directory
		MOV DWORD [page_directory], 0x0 ; Remove identity mapping.

		; Do a TLB flush. I can also use `INVLPG`, but that depends on
		; the current system's ISA. It's not a part of e.g. i386's ISA,
		; but does exist on the i686 ISA.
		MOV ECX, CR3
		MOV CR3, ECX

		MOV ESP, stack_top ; Set the stack up.

		EXTERN kernel
		CALL kernel

		; There is little use for this, as the kernel's main state
		; has ended. Call it anyway for crtend.o's sake.
		EXTERN _fini
		CALL _fini

		; The kernel program has finished. Nothing below occurs,
		; so I'll keep the infinite loop from the example
		; using the halt instruction.

		CLI ; Disable interrupts. Currently not enabled anyway.
	.halt:
		HLT ; Wait for the next interrupt, which won't come; thus, halt.
		JMP .halt ; Jump back to halt if an interrupt does occur.
	.exit:
