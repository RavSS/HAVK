#include <havk/memory.h>

/* Need the maps before a heap is possible, so this method won't work. */
/* void get_memory_map(struct multiboot_info *multiboot_data,
	struct multiboot_mmaps mmaps, uint16_t amount)
{
	uint16_t i;
	struct multiboot_mmap_entry_havk32bit *mmap_tmp;

	i = 0;
	multiboot_data->mmap_addr += (uintptr_t) VIRTUAL_ADDRESS_BASE;
	mmap_tmp = (struct multiboot_mmap_entry_havk32bit*)
		multiboot_data->mmap_addr;

	while ((uintptr_t) mmap_tmp < multiboot_data->mmap_addr +
		multiboot_data->mmap_length)
	{
		mmaps.entries[i].size = mmap_tmp->size;
		mmaps.entries[i].addr_h = mmap_tmp->addr_h;
		mmaps.entries[i].addr_l = mmap_tmp->addr_l;
		mmaps.entries[i].len_h = mmap_tmp->len_h;
		mmaps.entries[i].len_l = mmap_tmp->len_l;
		mmaps.entries[i].type = mmap_tmp->type;

		if (++i >= amount)
		{
			mmaps.amount = --i;
			break;
		}

		mmap_tmp = (struct multiboot_mmap_entry_havk32bit*)
			((uintptr_t) mmap_tmp + mmap_tmp->size
			+ sizeof(mmap_tmp->size));
	}

	return;
} */
