###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- paging.S                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .text

.GLOBAL assembly__load_page_structure
# (RDI => address of the page map/level 4 structure/PML4)
assembly__load_page_structure:
	MOV CR3, RDI
	RET

.GLOBAL assembly__get_page_fault_address
# () >> (RAX => page fault address)
assembly__get_page_fault_address:
	MOV RAX, CR2
	RET

.SECTION .isolated_bss

# This is where we store the indicated kernel page layout. We only need the
# level 4 page structure. It must be modified by some Ada code before
# interrupts and/or tasking is enabled.
.GLOBAL global__kernel_page_map_base_address
global__kernel_page_map_base_address:
	.SPACE 8
