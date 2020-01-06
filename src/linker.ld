/*****************************************************************************/
/* Program         -- The HAVK Operating System                              */
/* Filename        -- linker.ld                                              */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2019-2020                */
/*****************************************************************************/

OUTPUT_FORMAT(elf64-x86-64)
ENTRY(assembly__entry)
STARTUP(entry.o)

SECTIONS
{
	/* TODO: Find a better solution.
	/  I've moved the kernel position up by
	/  32-MiB so hopefully it doesn't collide
	/  with some system reserved memory, which
	/  tends to be the case on real hardware.
	/  I technically could relocate the kernel
	/  during the ELF loading, but this seems
	/  much easier. */
	__kernel_physical_base = 0x2000000;

	/* I've gone with a common virtual base of -2 GiB.
	/  This way, I still get to use the R_X86_64_32(S)
	/  relocations. A shame that instructions aside from
	/  MOV/MOVABS don't support 64-bit offsets. I definitely
	/  don't need the entire canonical higher-half. */
	__kernel_virtual_base = 0xFFFFFFFF80000000;

	__kernel_base = __kernel_physical_base + __kernel_virtual_base;
	. = __kernel_base;

	.text ALIGN(4K) : AT(ADDR(.text) - __kernel_virtual_base)
	{
		__kernel_text_base = .;

		*(.text)

		. = ALIGN(4K);
		__kernel_text_end = .;
	}

	.rodata ALIGN(4K) : AT(ADDR(.rodata) - __kernel_virtual_base)
	{
		__kernel_rodata_base = .;

		*(.rodata*)
		*(.rodata)

		. = ALIGN(4K);
		__kernel_rodata_end = .;
	}

	.data ALIGN(4K) : AT(ADDR(.data) - __kernel_virtual_base)
	{
		__kernel_data_base = .;

		*(.data)

		. = ALIGN(4K);
		__kernel_data_end = .;
	}

	.bss ALIGN(4K) : AT(ADDR(.bss) - __kernel_virtual_base)
	{
		__kernel_bss_base = .;

		*(COMMON)
		*(.bss)

		. = ALIGN(4K);
		__kernel_bss_end = .;
	}

	__kernel_end = .;

	/* Sizes of the sections and the heap. */
	__kernel_text_size = SIZEOF(.text);
	__kernel_rodata_size = SIZEOF(.rodata);
	__kernel_data_size = SIZEOF(.data);
	__kernel_bss_size = SIZEOF(.bss);
	__kernel_size = __kernel_end - __kernel_base;
	__kernel_heap_size = 128M; /* 128 MiB for now. */

	/* Define the addresses for the heap. Use huge pages (2 MiB)
	/  so we waste less time and space mapping the range. */
	. = ALIGN(2M);
	__kernel_heap_base = .;

	. += __kernel_heap_size;

	. = ALIGN(2M);
	__kernel_heap_end = .;

}
