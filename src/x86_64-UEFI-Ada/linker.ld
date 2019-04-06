OUTPUT_FORMAT(elf64-x86-64)
ENTRY(entry)
STARTUP(build/x86_64-UEFI-Ada/boot.o)

/* Aside from changing output format, the script
/  has not been changed much from the bare bones
/  example. Except the code address locations to
/  change in the future. */

SECTIONS
{
	. = 0x00100000;

	.text ALIGN(4K) :
	{
		code = .;
		_code = .;
		__code = .;
		*(.text)
		*(.rodata)
	}

	.rodata ALIGN(4K) :
	{
		*(.rodata)
	}

	.data ALIGN(4K) :
	{
		data = .;
		_data = .;
		__data = .;
		*(.data)
	}

	.bss ALIGN(4K) :
	{
		sbss = .;
		bss = .;
		_bss = .;
		__bss = .;
		*(COMMON)
		*(.bss)
		ebss = .;
	}

	end = .;
	_end = .;
	__end = .;
}
