#ifndef MEMORY_H
#define MEMORY_H

#include <havk.h>
#include <havk/terminal.h>

/* Only to be included here. */
#include <havk/multiboot.h>

/* Declared in the linker script. */
extern uintptr_t virtual_address_base;
extern uintptr_t kernel_address_start;
extern uintptr_t kernel_address_end;

/* These are here so I don't accidentally get what the addresses contain, as
/  that is useless to us, but rather the addresses themselves. */
#define VIRTUAL_ADDRESS_BASE (uintptr_t) &virtual_address_base
#define KERNEL_ADDRESS_START (uintptr_t) &kernel_address_start
#define KERNEL_ADDRESS_END (uintptr_t) &kernel_address_end

/* https://wiki.osdev.org/Detecting_Memory_(x86)#Memory_Map_Via_GRUB */
/* Modified copy of "multiboot_mmap_entry" from "multiboot.h"
/  for reason specified below (so it works). */
struct multiboot_mmap_entry_havk32bit
{
	multiboot_uint32_t size;

	/* The two "uint64_t" variables are split into four "uint32_t".
	/  The reasoning for this is because GCC fails to align them to
	/  4 bytes and instead aligns them to 8 byte multiples, even
	/  when explicitly attribute aligned.
	/  https://forum.osdev.org/viewtopic.php?t=30318 */
	multiboot_uint32_t addr_l;
	multiboot_uint32_t addr_h;
	multiboot_uint32_t len_l;
	multiboot_uint32_t len_h;

	#ifndef MULTIBOOT_HEADER
	#define MULTIBOOT_MEMORY_AVAILABLE 1
	#define MULTIBOOT_MEMORY_RESERVED 2
	#define MULTIBOOT_MEMORY_ACPI_RECLAIMABLE 3
	#define MULTIBOOT_MEMORY_NVS 4
	#define MULTIBOOT_MEMORY_BADRAM 5
	#endif

	multiboot_uint32_t type;
} __attribute__ ((packed, aligned(4)));

/* Use for without dynamic memory. I think 32 entries is all anyone will
/  need... I think. */
struct multiboot_mmaps
{
	struct multiboot_mmap_entry_havk32bit entries[32];
	uint8_t amount;
};

/* Retrives the GRUB provided memory map from the multiboot information.
/  So far there is only a 32-bit or i386 version for this, as I do not expect
/  HAVK to ever go into long mode and/or be a 64-bit kernel. Maybe. */
void get_memory_map(struct multiboot_info *multiboot_data,
	struct multiboot_mmaps mmaps, uint16_t entries);

#endif