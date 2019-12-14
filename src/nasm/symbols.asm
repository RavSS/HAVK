; This file provides a cheap workaround for getting the symbol address instead
; of what is at the symbol. Simply use "External_Name" on a label (with
; "_address" at the end) below in Ada to get it.
%macro DEFINE_ADDRESS_SYMBOL 1
	GLOBAL %1_address:
	%1_address:
		EXTERN %1
		DQ %1
%endmacro

SECTION .rodata ; Kept in read-only data because HAVK should never change them.

; Map the entire kernel.
DEFINE_ADDRESS_SYMBOL kernel_base
DEFINE_ADDRESS_SYMBOL kernel_end
DEFINE_ADDRESS_SYMBOL kernel_virtual_base
DEFINE_ADDRESS_SYMBOL kernel_physical_base

; Now the sections. These are virtual addresses.
DEFINE_ADDRESS_SYMBOL kernel_text_base
DEFINE_ADDRESS_SYMBOL kernel_text_end
DEFINE_ADDRESS_SYMBOL kernel_rodata_base
DEFINE_ADDRESS_SYMBOL kernel_rodata_end
DEFINE_ADDRESS_SYMBOL kernel_data_base
DEFINE_ADDRESS_SYMBOL kernel_data_end
DEFINE_ADDRESS_SYMBOL kernel_bss_base
DEFINE_ADDRESS_SYMBOL kernel_bss_end
