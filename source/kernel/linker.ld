/*****************************************************************************/
/* Program         -- HAVK                                                   */
/* Filename        -- linker.ld                                              */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2019-2021                */
/*****************************************************************************/

OUTPUT_FORMAT(elf64-x86-64)
ENTRY(assembly__entry)
STARTUP(entry.o)

SECTIONS
{
	/* I've gone with a common virtual base of -2 GiB.
	/  This way, I still get to use the R_X86_64_32(S)
	/  relocations. A shame that instructions aside from
	/  MOV/MOVABS don't support 64-bit offsets. I definitely
	/  don't need the entire canonical higher-half. */
	linker__kernel_virtual_base = 0xFFFFFFFF80000000;

	. = linker__kernel_virtual_base;

	.text ALIGN(4K) : AT(ADDR(.text) - linker__kernel_virtual_base)
	{
		linker__kernel_text_base = .;

		*(.text)

		. = ALIGN(4K);
		linker__kernel_text_end = .;
	}

	.rodata ALIGN(4K) : AT(ADDR(.rodata) - linker__kernel_virtual_base)
	{
		linker__kernel_rodata_base = .;

		*(.rodata*)
		*(.rodata)

		. = ALIGN(4K);
		linker__kernel_rodata_end = .;
	}

	.data ALIGN(4K) : AT(ADDR(.data) - linker__kernel_virtual_base)
	{
		linker__kernel_data_base = .;

		*(.data)

		. = ALIGN(4K);
		linker__kernel_data_end = .;
	}

	.bss ALIGN(4K) : AT(ADDR(.bss) - linker__kernel_virtual_base)
	{
		linker__kernel_bss_base = .;

		*(COMMON)
		*(.bss)

		. = ALIGN(4K);
		linker__kernel_bss_end = .;
	}

	/* Below are the isolated sections. They're a weak attempt at
	/  implementing KPTI/KAISER without ASLR. The idea is to keep them in
	/  ring 3 only. The section attributes for the below are not important
	/  during loading, we'll resolve their read-write-execute states
	/  according to their names during runtime. */

	.isolated_text ALIGN(4K) :
		AT(ADDR(.isolated_text) - linker__kernel_virtual_base)
	{
		linker__kernel_isolated_text_base = .;

		*(.isolated_text)

		. = ALIGN(4K);
		linker__kernel_isolated_text_end = .;
	}

	.isolated_data ALIGN(4K) :
		AT(ADDR(.isolated_data) - linker__kernel_virtual_base)
	{
		linker__kernel_isolated_data_base = .;

		*(.isolated_data)

		. = ALIGN(4K);
		linker__kernel_isolated_data_end = .;
	}

	/* TODO: I cannot manage to make this section marked as "NOBITS". As of
	/  now, it's just a data section. Be careful of what you put here. */
	.isolated_bss ALIGN(4K) :
		AT(ADDR(.isolated_bss) - linker__kernel_virtual_base)
	{
		linker__kernel_isolated_bss_base = .;

		*(.isolated_bss)

		. = ALIGN(4K);
		linker__kernel_isolated_bss_end = .;
	}

	. = ALIGN(2M);
	linker__kernel_virtual_end = .;

	/* Sizes of the sections. */
	linker__kernel_text_size = SIZEOF(.text);
	linker__kernel_rodata_size = SIZEOF(.rodata);
	linker__kernel_data_size = SIZEOF(.data);
	linker__kernel_bss_size = SIZEOF(.bss);
	linker__kernel_isolated_text_size = SIZEOF(.isolated_text);
	linker__kernel_isolated_data_size = SIZEOF(.isolated_data);
	linker__kernel_isolated_bss_size = SIZEOF(.isolated_bss);
	linker__kernel_size
		= linker__kernel_virtual_end - linker__kernel_virtual_base;
}
