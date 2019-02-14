OUTPUT_FORMAT(elf32-i386)
ENTRY(entry)

SECTIONS
{
	/* I want to make a higher half kernel. The kernel is placed at
	/  0xC0100000 (3 GiB), but it's physically at 0x00100000 (1 MiB). */
	virtual_address_base = 0xC0000000;
	kernel_address_start = virtual_address_base + 0x00100000;

	. = kernel_address_start;

	.text ALIGN(4K) : AT (ADDR(.text) - virtual_address_base)
	{
		*(.multiboot) /* Multiboot header for the bootloader. */
		*(.text)
	}

	/* Read-only data. */
	.rodata ALIGN(4K) : AT (ADDR(.rodata) - virtual_address_base)
	{
		*(.rodata)
	}

	/* Read-write initialized data. */
	.data ALIGN(4K) : AT (ADDR(.data) - virtual_address_base)
	{
		*(.data)
	}

	/* The stack for C and uninitialized data. */
	.bss ALIGN(4K) : AT (ADDR(.bss) - virtual_address_base)
	{
		*(COMMON)
		*(.bss)
	}

	kernel_address_end = .;
}
