; Constants for detailing the multiboot header.
MAGIC EQU 0x1BADB002
FLAGS EQU ((1 << 0) | (1 << 1))
CHECKSUM EQU -(MAGIC + FLAGS)
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
	; Most of those will be handled in the "_init" section of the 
	; code before the kernel's C code is finally called.

	; The magic number is put in the EAX register by GRUB, and it most
	; certainly will be clobbered by initialization functions etc. before
	; the kernel function is called. I am going to be lazy and use the VGA
	; memory to store the magic number, along with the multiboot data
	; address (just in case EBX gets clobbered too). The address will be
	; mapped when I setup paging, it will be at the end of the kernel page.
	ADD EBX, VIRADDR_START ; Add the virtual address base for later access.
	MOV [0x3FFFF8], EBX ; Multiboot information.
	MOV [0x3FFFFC], EAX ; Magic number.

	.initialize_the_kernel:
		EXTERN _init
		CALL _init

		LEA ECX, [.enter_the_kernel]
		JMP ECX

	; The kernel is linked, so now we call it to enter the kernel.
	.enter_the_kernel:
		EXTERN page_directory_boot
		MOV DWORD [page_directory_boot], 0x0 ; Remove identity mapping.

		; Do a TLB flush. I can also use `INVLPG`, but that depends on
		; the current system's ISA. It's not a part of e.g. i386's ISA,
		; but does exist on i686's ISA.
		MOV ECX, CR3
		MOV CR3, ECX

		MOV ESP, stack_top ; Set the stack up.

		; Multiboot information and magic boot number pushed to the
		; stack. Remember that the identity mapping is removed, so I
		; can't access the VGA memory buffer address directly anymore.
		PUSH DWORD [0xC03FFFFC] ; Magic number.
		PUSH DWORD [0xC03FFFF8] ; Multiboot information.

		EXTERN kernel
		CALL kernel

		; There is little use for this, as the kernel's main state
		; has ended. Call it anyway for crtend.o's sake.
		EXTERN _fini
		CALL _fini

		; The kernel program has finished. Nothing below occurs,
		; so I'll keep the infinite loop from the example
		; using the halt instruction.

		CLI ; Disable interrupts.
	.halt:
		HLT ; Wait for the next interrupt, which won't come; thus, halt.
		JMP .halt ; Jump back to halt if an interrupt does occur.
	.exit:
