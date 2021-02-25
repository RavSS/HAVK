###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- paging.s                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2021                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__load_page_structure
.TYPE assembly__load_page_structure, @function
# (RDI => address of the page map/level 4 structure/PML4)
assembly__load_page_structure:
	MOV CR3, RDI
	RET

.GLOBAL assembly__get_page_fault_address
.TYPE assembly__get_page_fault_address, @function
# () >> (RAX => page fault address)
assembly__get_page_fault_address:
	MOV RAX, CR2
	RET

.SECTION .isolated_bss

# This is where we store the indicated kernel page layout. We only need the
# level 4 page structure. It must be modified by some Ada code before
# interrupts and/or tasking is enabled.
.GLOBAL assembly__kernel_page_map_base_address
assembly__kernel_page_map_base_address:
	.SPACE 8
