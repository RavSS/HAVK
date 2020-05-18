/*****************************************************************************/
/* Program         -- HAVK                                                   */
/* Filename        -- linker.ld                                              */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2019-2020                */
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
	var__kernel_virtual_base = 0xFFFFFFFF80000000;

	. = var__kernel_virtual_base;

	.text ALIGN(4K) : AT(ADDR(.text) - var__kernel_virtual_base)
	{
		var__kernel_text_base = .;

		*(.text)

		. = ALIGN(4K);
		var__kernel_text_end = .;
	}

	.rodata ALIGN(4K) : AT(ADDR(.rodata) - var__kernel_virtual_base)
	{
		var__kernel_rodata_base = .;

		*(.rodata*)
		*(.rodata)

		. = ALIGN(4K);
		var__kernel_rodata_end = .;
	}

	.data ALIGN(4K) : AT(ADDR(.data) - var__kernel_virtual_base)
	{
		var__kernel_data_base = .;

		*(.data)

		. = ALIGN(4K);
		var__kernel_data_end = .;
	}

	.bss ALIGN(4K) : AT(ADDR(.bss) - var__kernel_virtual_base)
	{
		var__kernel_bss_base = .;

		*(COMMON)
		*(.bss)

		. = ALIGN(4K);
		var__kernel_bss_end = .;
	}

	/* Below are the isolated sections. They're for "ramdisk" purposes.
	/  The idea is to keep them in ring 3 only. The section attributes
	/  for the below are not important during loading, we'll resolve their
	/  read-write-execute states according to their names at runtime. */

	.isolated_text ALIGN(4K) :
		AT(ADDR(.isolated_text) - var__kernel_virtual_base)
	{
		var__kernel_isolated_text_base = .;

		*(.isolated_text)

		. = ALIGN(4K);
		var__kernel_isolated_text_end = .;
	}

	.isolated_data ALIGN(4K) :
		AT(ADDR(.isolated_data) - var__kernel_virtual_base)
	{
		var__kernel_isolated_data_base = .;

		*(.isolated_data)

		. = ALIGN(4K);
		var__kernel_isolated_data_end = .;
	}

	/* TODO: I cannot manage to make this section marked as "NOBITS". As of
	/  now, it's just a data section. Be careful of what you put here. */
	.isolated_bss ALIGN(4K) :
		AT(ADDR(.isolated_bss) - var__kernel_virtual_base)
	{
		var__kernel_isolated_bss_base = .;

		*(.isolated_bss)

		. = ALIGN(4K);
		var__kernel_isolated_bss_end = .;
	}

	. = ALIGN(2M);
	var__kernel_virtual_end = .;

	/* Sizes of the sections. */
	var__kernel_text_size = SIZEOF(.text);
	var__kernel_rodata_size = SIZEOF(.rodata);
	var__kernel_data_size = SIZEOF(.data);
	var__kernel_bss_size = SIZEOF(.bss);
	var__kernel_isolated_text_size = SIZEOF(.isolated_text);
	var__kernel_isolated_data_size = SIZEOF(.isolated_data);
	var__kernel_isolated_bss_size = SIZEOF(.isolated_bss);
	var__kernel_size = var__kernel_virtual_end - var__kernel_virtual_base;
}
