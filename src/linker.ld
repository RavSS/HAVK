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
	/  2 MiB so hopefully it doesn't collide
	/  with some system reserved memory, which
	/  tends to be the case on real hardware.
	/  I technically could relocate the kernel
	/  during the ELF loading, but this seems
	/  much easier and less error prone. */
	. = 0x00300000;
	kernel_base = .;

	.text ALIGN(4K) :
	{
		section_text = .;
		*(.text)
	}

	.rodata ALIGN(4K) :
	{
		section_rodata = .;
		*(.rodata)
	}

	.data ALIGN(4K) :
	{
		section_data = .;
		*(.data)
	}

	.bss ALIGN(4K) :
	{
		section_bss = .;
		*(COMMON)
		*(.bss)
	}

	kernel_end = .;
}
