;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program         -- The HAVK Operating System                              ;;
;; Filename        -- symbols.asm                                            ;;
;; License         -- GNU General Public License Version 3.0                 ;;
;; Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file provides a cheap workaround for getting the symbol address instead
; of what is at the symbol. Simply use "External_Name" on a label (with
; "_address" at the end) below in Ada to get it.
%MACRO DEFINE_ADDRESS_SYMBOL 1
	GLOBAL %1_address:
	%1_address:
		EXTERN %1
		DQ %1
%ENDMACRO

SECTION .rodata ; Kept in read-only data because HAVK should never change them.

; For mapping the entire kernel.
DEFINE_ADDRESS_SYMBOL __kernel_base
DEFINE_ADDRESS_SYMBOL __kernel_end
DEFINE_ADDRESS_SYMBOL __kernel_virtual_base
DEFINE_ADDRESS_SYMBOL __kernel_physical_base

; Now the sections. These are virtual addresses.
DEFINE_ADDRESS_SYMBOL __kernel_text_base
DEFINE_ADDRESS_SYMBOL __kernel_text_end
DEFINE_ADDRESS_SYMBOL __kernel_rodata_base
DEFINE_ADDRESS_SYMBOL __kernel_rodata_end
DEFINE_ADDRESS_SYMBOL __kernel_data_base
DEFINE_ADDRESS_SYMBOL __kernel_data_end
DEFINE_ADDRESS_SYMBOL __kernel_bss_base
DEFINE_ADDRESS_SYMBOL __kernel_bss_end
DEFINE_ADDRESS_SYMBOL __kernel_heap_base
DEFINE_ADDRESS_SYMBOL __kernel_heap_end

; The sizes. These don't have to be calculated during execution.
DEFINE_ADDRESS_SYMBOL __kernel_size
DEFINE_ADDRESS_SYMBOL __kernel_text_size
DEFINE_ADDRESS_SYMBOL __kernel_rodata_size
DEFINE_ADDRESS_SYMBOL __kernel_data_size
DEFINE_ADDRESS_SYMBOL __kernel_bss_size
DEFINE_ADDRESS_SYMBOL __kernel_heap_size
