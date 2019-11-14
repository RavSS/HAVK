OUTPUT_FORMAT(elf64-x86-64)
ENTRY(entry)
STARTUP(entry.o)

/* Aside from changing output format, the script
/  has not been changed much from the bare bones
/  example. I have removed some segments, they
/  did not seem like they were used. */

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
		section_text = .;
		*(.text)
	}

	.rodata ALIGN(4K) : AT(ADDR(.rodata) - kernel_virtual_base)
	{
		section_rodata = .;
		*(.rodata)
	}

	.data ALIGN(4K) : AT(ADDR(.data) - kernel_virtual_base)
	{
		section_data = .;
		*(.data)
	}

	.bss ALIGN(4K) : AT(ADDR(.bss) - kernel_virtual_base)
	{
		section_bss = .;
		*(COMMON)
		*(.bss)
	}

	kernel_end = .;
}
