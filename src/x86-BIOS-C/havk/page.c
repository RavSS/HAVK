#include <havk/page.h>

/* Just going to have a single directory for now. Using a new one instead
/  of the page directory used to enter the kernel. */
static uint32_t page_directory[1024] __attribute__ ((aligned(4096)));

/* These are to be used by the allocator in order to figure out what
/  memory is actually available; thus, these require a memory map. */
static size_t memory_limit_base;
static size_t memory_limit_end;

void tlb_flush_all(void)
{
	__asm__ volatile
	(
		"MOV ECX, CR3;"
		"MOV CR3, ECX;"
		::: "ecx"
	);
}

void page_directory_load(uintptr_t page_directory_address)
{
	__asm__ volatile ("MOV CR3, %0" :: "r" (page_directory_address));
}

/* END: This function was still under work. As of now, it maps whatever
/  virtual address to the physical address of 0x0. */
uint32_t page_directory_entry_4mib(uint32_t frame,
	bool global, bool never_cache, bool write_through,
	bool user_access, bool write_access, bool present)
{
	uint32_t entry;

	entry = 0;
	entry |= (uint8_t) frame << 22;
	entry |= global << 8;
	entry |= 0x80; /* 1 bit. Identifies size of current entry. */
	entry |= never_cache << 4;
	entry |= write_through << 3;
	entry |= user_access << 2;
	entry |= write_access << 1;
	entry |= present;

	return entry;
}

void paging_initialize(size_t memory_begin, size_t memory_end)
{
	uint32_t i;
	uintptr_t directory_address;

	memory_limit_base = memory_begin;
	memory_limit_base += KERNEL_ADDRESS_END - VIRTUAL_ADDRESS_BASE;
	memory_limit_end = memory_end;

	for (i = 0; i < 1024; ++i)
	{
		page_directory[i] = 0x2;
	}

	page_directory_map_address(VIRTUAL_ADDRESS_BASE,
		page_directory_entry_4mib(0, 0, 0, 0, 0, 1, 1));

	directory_address = (uintptr_t) &page_directory - VIRTUAL_ADDRESS_BASE;

	page_directory_load(directory_address);

	hprintf("Initialized new page directory.", 0);
}

