/*****************************************************************************/
/* Program         -- HAVK Operating System                                  */
/* Filename        -- linker.ld                                              */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020-2021                */
/*****************************************************************************/

OUTPUT_FORMAT(elf64-x86-64)
ENTRY(_start)

SECTIONS
{
	/* For now, this is just a simple configuration all ELF files must
	/  follow if they are to be loadable by HAVK. */
	. = 0x1000;

	.text ALIGN(4K) : AT(ADDR(.text))
	{
		*(.text._start)
		*(.text)
	}

	.rodata ALIGN(4K) : AT(ADDR(.rodata))
	{
		*(.rodata*)
		*(.rodata)
	}

	.data ALIGN(4K) : AT(ADDR(.data))
	{
		*(.data)
	}

	.bss ALIGN(4K) : AT(ADDR(.bss))
	{
		*(COMMON)
		*(.bss)
	}
}
