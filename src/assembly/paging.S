###############################################################################
## Program         -- The HAVK Operating System                              ##
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
