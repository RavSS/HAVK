; Constants for detailing the multiboot header.
MAGIC EQU 0x1BADB002
FLAGS EQU ((1 << 0) | (1 << 1))
CHECKSUM EQU -(MAGIC + FLAGS)
; Constants for the kernel's position.
KERNEL EQU 0xC0100000
VIRADDR_START EQU (KERNEL - 0x00100000)
HAVK_MAGIC_NUMBER_ENTRY EQU 0xDEADC0DE ; Magic number for HAVK's bootloader.

; Multiboot header declaring this program as a kernel. Looks for the checksum
; as detailed in the "multiboot standard".
; https://www.gnu.org/software/grub/manual/multiboot/multiboot.html
; Note that my bootloader (or rather HAVK's bootloader) does not care about
; this section whatsoever. This is left here for some compatibility.
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
stack_bottom: RESB 16384 ; `RESB` instead of `DB` for uninitialized allocation.
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

	; This jump is also here for compatibility, so the dead code is not
	; executed. This way, both GRUB and HAVK's bootloader can boot HAVK
	; without corrupting something up by executing non-executable data.
	JMP .initialize_the_kernel ; Skip the next magic number.
	DD HAVK_MAGIC_NUMBER_ENTRY ; Only HAVK's own bootloader relies on this.

	.initialize_the_kernel:
		; The magic number is put in the EAX register by GRUB
		; (and now by my terrible bootloader too). The memory map
		; (if not HAVK's bootloader) and the magic number will be
		; stored at the end of the kernel's page.
		; Update these if the kernel gets much bigger.
		ADD EBX, VIRADDR_START ; Add the base for later access.
		MOV DWORD [0x3FFFF8], EBX ; Bootloader information.
		MOV DWORD [0x3FFFFC], EAX ; Magic number.

		EXTERN _init
		CALL _init ; All initializations must happen in there only.

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
	.halt:
		CLI ; Disable interrupts.
		HLT ; Wait for the next interrupt, which won't come; thus, halt.
		JMP .halt ; Jump back to halt if an NMI does occur.
	.exit:
