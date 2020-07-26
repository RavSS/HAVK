###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- symbols.s                                              ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

# This file provides a cheap workaround for getting the symbol address instead
# of what is at the symbol. Simply use "External_Name" on a label (with
# "_address" at the end) below in Ada to get it.
.MACRO SYMBOL_POINTER SYMBOL:req
	.IFNDEF \SYMBOL\()_address
		.GLOBAL \SYMBOL\()_address
		\SYMBOL\()_address:
			.QUAD \SYMBOL
	.ENDIF
.ENDM

.SECTION .rodata # Kept in read-only data because HAVK shouldn't change them.

# For mapping the entire kernel in virtual space.
SYMBOL_POINTER global__kernel_virtual_base
SYMBOL_POINTER global__kernel_virtual_end

# Now the sections. These are also virtual addresses.
SYMBOL_POINTER global__kernel_text_base
SYMBOL_POINTER global__kernel_text_end
SYMBOL_POINTER global__kernel_rodata_base
SYMBOL_POINTER global__kernel_rodata_end
SYMBOL_POINTER global__kernel_data_base
SYMBOL_POINTER global__kernel_data_end
SYMBOL_POINTER global__kernel_bss_base
SYMBOL_POINTER global__kernel_bss_end
SYMBOL_POINTER global__kernel_isolated_text_base
SYMBOL_POINTER global__kernel_isolated_text_end
SYMBOL_POINTER global__kernel_isolated_data_base
SYMBOL_POINTER global__kernel_isolated_data_end
SYMBOL_POINTER global__kernel_isolated_bss_base
SYMBOL_POINTER global__kernel_isolated_bss_end

# The sizes. These don't have to be calculated during execution.
SYMBOL_POINTER global__kernel_size
SYMBOL_POINTER global__kernel_text_size
SYMBOL_POINTER global__kernel_rodata_size
SYMBOL_POINTER global__kernel_data_size
SYMBOL_POINTER global__kernel_bss_size
SYMBOL_POINTER global__kernel_isolated_text_size
SYMBOL_POINTER global__kernel_isolated_data_size
SYMBOL_POINTER global__kernel_isolated_bss_size
