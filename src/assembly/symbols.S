###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- symbols.S                                              ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

# This file provides a cheap workaround for getting the symbol address instead
# of what is at the symbol. Simply use "External_Name" on a label (with
# "_address" at the end) below in Ada to get it.
.MACRO DEFINE_SYMBOL_POINTER SYMBOL:req
	.IFNDEF \SYMBOL\()_address
		.GLOBAL \SYMBOL\()_address
		\SYMBOL\()_address:
			.EXTERN \SYMBOL\()_address
			.QUAD \SYMBOL
	.ENDIF
.ENDM

.SECTION .rodata # Kept in read-only data because HAVK shouldn't change them.

# For mapping the entire kernel.
DEFINE_SYMBOL_POINTER __kernel_base
DEFINE_SYMBOL_POINTER __kernel_end
DEFINE_SYMBOL_POINTER __kernel_virtual_base
DEFINE_SYMBOL_POINTER __kernel_physical_base

# Now the sections. These are virtual addresses.
DEFINE_SYMBOL_POINTER __kernel_text_base
DEFINE_SYMBOL_POINTER __kernel_text_end
DEFINE_SYMBOL_POINTER __kernel_rodata_base
DEFINE_SYMBOL_POINTER __kernel_rodata_end
DEFINE_SYMBOL_POINTER __kernel_data_base
DEFINE_SYMBOL_POINTER __kernel_data_end
DEFINE_SYMBOL_POINTER __kernel_bss_base
DEFINE_SYMBOL_POINTER __kernel_bss_end
DEFINE_SYMBOL_POINTER __kernel_heap_base
DEFINE_SYMBOL_POINTER __kernel_heap_end

# The sizes. These don't have to be calculated during execution.
DEFINE_SYMBOL_POINTER __kernel_size
DEFINE_SYMBOL_POINTER __kernel_text_size
DEFINE_SYMBOL_POINTER __kernel_rodata_size
DEFINE_SYMBOL_POINTER __kernel_data_size
DEFINE_SYMBOL_POINTER __kernel_bss_size
DEFINE_SYMBOL_POINTER __kernel_heap_size
