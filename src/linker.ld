OUTPUT_FORMAT(elf64-x86-64)
ENTRY(entry)
STARTUP(entry.o)
_DEFAULT_STACK_SIZE = 65536; /* Match the size in "entry.asm". */

SECTIONS
{
	/* TODO: Find a better solution.
	/  I've moved the kernel position up by
	/  32 MiB so hopefully it doesn't collide
	/  with some system reserved memory, which
	/  tends to be the case on real hardware.
	/  I technically could relocate the kernel
	/  during the ELF loading, but this seems
	/  much easier. */
	kernel_physical_base = 0x2000000;

	/* I've gone with a common virtual base of -2 GiB.
	/  This way, I still get to use the R_X86_64_32(S)
	/  relocations. A shame that instructions aside from
	/  MOV/MOVABS don't support 64-bit offsets. I definitely
	/  don't need the entire canonical higher-half. */
	kernel_virtual_base = 0xFFFFFFFF80000000;

	kernel_base = kernel_physical_base + kernel_virtual_base;
	. = kernel_base;

	.text ALIGN(4K) : AT(ADDR(.text) - kernel_virtual_base)
	{
		kernel_text_base = .;
		
		*(.text)
		
		. = ALIGN(4K);
		kernel_text_end = .;
	}

	.rodata ALIGN(4K) : AT(ADDR(.rodata) - kernel_virtual_base)
	{
		kernel_rodata_base = .;
		
		*(.rodata*)
		*(.rodata)
		
		. = ALIGN(4K);
		kernel_rodata_end = .;
	}

	.data ALIGN(4K) : AT(ADDR(.data) - kernel_virtual_base)
	{
		kernel_data_base = .;
		
		*(.data)
		
		. = ALIGN(4K);
		kernel_data_end = .;
	}

	.bss ALIGN(4K) : AT(ADDR(.bss) - kernel_virtual_base)
	{
		kernel_bss_base = .;
		
		*(COMMON)
		*(.bss)
		
		. = ALIGN(4K);
		kernel_bss_end = .;
	}

	. = ALIGN(4K); /* Align the end too for easier paging. */
	kernel_end = .;
}
