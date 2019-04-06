#ifndef PAGE_H
#define PAGE_H

#include <havk.h>
#include <havk/memory.h>
#include <havk/io.h>

/* Currently, paging for HAVK is using a PSE no-PAE level scheme. That means
/  there's two levels. One part of the linear address refers to the
/  location in the page directory. The other is the offset to the physical
/  address. This means I don't have to manage page tables, but at the
/  expense of 4 MiB pages, which is quite wasteful compared to 4 KiB pages.
/  Memory efficiency is not a goal of this operating system regardless. */

// extern uint32_t page_directory[1024] __attribute__ ((aligned(4096)));
// extern size_t memory_limit_base;
// extern size_t memory_limit_end;

/* Flushes the whole TLB. Slow. */
void tlb_flush_all(void);

/* Load the physical address for the page directory into the CR3 register. */
void page_directory_load(uintptr_t page_directory_address);

/* Map the physical address specified and give it a specific entry. */
#define page_directory_map_address(x, y) page_directory[(x) >> 0x16] = (y)

/* Creates a 4 MiB entry which can then be passed to the mapping function.
/  The page directory entry layout (32 bits) is described by
/  Figure 4-4 in Intel's architecture manual (Volume 3A, 4-13). */
uint32_t page_directory_entry_4mib
(
	uint32_t frame, /* 20 bits. */
	/* bool pat, / 1 bit. Not currently important. Ignored. */
	/* 3 empty bits. */
	bool global, /* 1 bit. */
	/* bool page_size, / 1 bit. Already set to 1 for obvious reasons. */
	/* bool dirty, / 1 bit. Handled by CPU. Zero for new entries. */
	/* bool accessed, / 1 bit. Handled by CPU. Zero for new entries. */
	bool never_cache, /* 1 bit. */
	bool write_through, /* 1 bit. */
	bool user_access, /* 1 bit. */
	bool write_access, /* 1 bit. */
	bool present /* 1 bit. */
);

/* Setup the pre-designated (as of now) page directory. */
void paging_initialize(size_t memory_begin, size_t memory_end);

#endif
