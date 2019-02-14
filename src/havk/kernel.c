#include <havk.h>
#include <havk/standard.h>
#include <havk/error.h>
#include <havk/memory.h>
#include <havk/page.h>
#include <havk/string.h>
#include <havk/terminal.h>
#include <havk/io.h>
#include <havk/descriptors.h>
#include <havk/PIC.h>
#include <havk/PIT.h>

void kernel(struct multiboot_info *multiboot_data, uint32_t multiboot_magic)
{
	uint32_t i;
	struct multiboot_mmaps memory_maps;

	gdt_initialize();
	idt_initialize();
	enable_interrupts();

	if (!ps2_initialize_keyboard(2))
		hprintf("Failed to initialize keyboard.", 0);

	if (multiboot_magic == MULTIBOOT_BOOTLOADER_MAGIC)
	{
		struct multiboot_mmap_entry_havk32bit *mmap_tmp;
		i = 0;
		multiboot_data->mmap_addr += VIRTUAL_ADDRESS_BASE;
		mmap_tmp = (struct multiboot_mmap_entry_havk32bit*)
			multiboot_data->mmap_addr;

		while ((uintptr_t) mmap_tmp < multiboot_data->mmap_addr +
			multiboot_data->mmap_length)
		{
			memory_maps.entries[i].size = mmap_tmp->size;
			memory_maps.entries[i].addr_h = mmap_tmp->addr_h;
			memory_maps.entries[i].addr_l = mmap_tmp->addr_l;
			memory_maps.entries[i].len_h = mmap_tmp->len_h;
			memory_maps.entries[i].len_l = mmap_tmp->len_l;
			memory_maps.entries[i++].type = mmap_tmp->type;

			mmap_tmp = (struct multiboot_mmap_entry_havk32bit*)
				((uintptr_t) mmap_tmp + mmap_tmp->size
				+ sizeof(mmap_tmp->size));
		}

		memory_maps.amount = i;
	}

	/* Center-text via width = (line width / 2) - (string length / 2). */
	shift(VGA_WIDTH_MAX / 2 - 8, VGA_HEIGHT_MAX / 2 - 3);
	print("WELCOME TO HAVK");

	shift(VGA_WIDTH_MAX / 2 - 8, VGA_HEIGHT_MAX / 2 - 2);
	printf("Version: %s", VERSION);

	shift(VGA_WIDTH_MAX / 2 - 14, VGA_HEIGHT_MAX / 2);
	printf("Compiled at time: %s", __TIME__);

	shift(VGA_WIDTH_MAX / 2 - 14, VGA_HEIGHT_MAX / 2 + 1);
	printf("Compiled at date: %s", __DATE__);

	shift(VGA_WIDTH_MAX / 2 - 14, VGA_HEIGHT_MAX / 2 + 2);
	printf("Compiled with C version: %d", __STDC_VERSION__);

	shift(VGA_WIDTH_MAX / 2 - 12, VGA_HEIGHT_MAX / 2 + 4);
	print("This operating system is");

	shift(VGA_WIDTH_MAX / 2 - 15, VGA_HEIGHT_MAX / 2 + 5);
	print("still under early development.");

	shift(VGA_WIDTH_MAX / 2 - 15, VGA_HEIGHT_MAX / 2 + 7);
	print("Press the spacebar to continue.");
	h_getchar('\x20');
	printc('\r');

	hprintf("Now printing memory map entries:", 0);

	/*For checking the memory map entries.*/
	for (i = 0; i < memory_maps.amount; ++i)
	{
		hprintf("\tBASE_HIGH = %H, BASE_LOW = %H\r"
			"    \tLIMIT_HIGH = %H, LIMIT_LOW = %H, TYPE=%H",
			memory_maps.entries[i].addr_h,
			memory_maps.entries[i].addr_l,
			memory_maps.entries[i].len_h,
			memory_maps.entries[i].len_l,
			memory_maps.entries[i].type);
	}

	while (1)
	{
		hprintf("PS/2 Key: %c", ps2_key);
		sleep(1, false);
		shift(0, VGA_HEIGHT - 1);
		HALT;
	}

	while (1) sleep(UINT_MAX, false);
	return;
}
