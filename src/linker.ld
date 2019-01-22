OUTPUT_FORMAT(elf32-i386)
ENTRY(entry)

SECTIONS
{
	/* I want to make a higher half kernel. The kernel is placed at
	/  0xC0100000 (3 GiB, but it's physically at 0x00100000 (1 MiB). */
	. = 0xC0100000;

	kernel_addr_start = .;

	.text ALIGN(4K) : AT (ADDR(.text) - 0xC0000000)
	{
		*(.multiboot) /* Multiboot header for the bootloader. */
		*(.text)
	}

	/* Read-only data. */
	.rodata ALIGN(4K) : AT (ADDR(.rodata) - 0xC0000000)
	{
		*(.rodata)
	}

	/* Read-write initialized data. */
	.data ALIGN(4K) : AT (ADDR(.data) - 0xC0000000)
	{
		*(.data)
	}

	/* The stack for C and uninitialized data. */
	.bss ALIGN(4K) : AT (ADDR(.bss) - 0xC0000000)
	{
		*(COMMON)
		*(.bss)
	}

	kernel_addr_end = .;
}
