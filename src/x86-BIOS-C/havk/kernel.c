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

__attribute__ ((noreturn))
void kernel(void *bootloader_data, uint32_t magic_number)
{
	uint32_t i;
	uint64_t memory_size;
	struct multiboot_mmaps memory_maps;

	gdt_initialize();
	idt_initialize();
	interrupt_enable();

	/* END: This was the last change made to the C version of HAVK.
	/  All effort is now going towards the Ada version of HAVK.
	/  This is as far as HAVK for 32-bit systems will go for now.
	/  So finally, this paging function recreates the page
	/  directory and mappings. */
	paging_initialize(0, 0);

	if (!ps2_initialize_keyboard(2))
		hprintf("Failed to initialize keyboard.", 0);

	if (magic_number == MULTIBOOT_BOOTLOADER_MAGIC)
	{
		struct multiboot_mmap_entry_havk32bit *mmap_tmp;
		struct multiboot_info *multiboot_data;

		i = 0;
		multiboot_data = (struct multiboot_info*) bootloader_data;
		multiboot_data->mmap_addr += VIRTUAL_ADDRESS_BASE;
		mmap_tmp = (struct multiboot_mmap_entry_havk32bit*)
			multiboot_data->mmap_addr;

		while ((uintptr_t) mmap_tmp < multiboot_data->mmap_addr +
			multiboot_data->mmap_length)
		{
			memory_maps.entries[i].size
				= mmap_tmp->size;
			memory_maps.entries[i].address_low
				= mmap_tmp->address_low;
			memory_maps.entries[i].address_high
				= mmap_tmp->address_high;
			memory_maps.entries[i].length_low
				= mmap_tmp->length_low;
			memory_maps.entries[i].length_high
				= mmap_tmp->length_high;
			memory_maps.entries[i++].type
				= mmap_tmp->type;

			mmap_tmp = (struct multiboot_mmap_entry_havk32bit*)
				((uintptr_t) mmap_tmp + mmap_tmp->size
				+ sizeof(mmap_tmp->size));
		}

		memory_maps.amount = i;

		for (i = 0, memory_size = 0; i < memory_maps.amount; ++i)
		{
			if (memory_maps.entries[i].type
				== MULTIBOOT_MEMORY_AVAILABLE)
			{
				memory_size
					+= memory_maps.entries[i].length_low;
				memory_size
					-= memory_maps.entries[i].address_high;
			}
		}
	}
	else if (magic_number == HAVK_BOOTLOADER_MAGIC)
	{
		hprintf("HAVK was bootloaded with it's own bootloader.", 0);

		/* TODO: Currently, my bootloader does not supply a memory map.
		/  The other question is if my bootloader will be up to the
		/  multiboot specification. Skip showing the memory for now. */
		memory_size = 0;
	}
	else emergency("Failed to detect what HAVK was bootloaded with.");

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
	clear_line(VGA_HEIGHT);
	printc('\r');

	hprintf("System passed first initialization.", 0);
	hprintf("RAM size is %D MiB", btom(memory_size));

	while (1)
	{
		hprintf("PS/2 Key: %c", PS2_KEY);
		cursor_move(VGA_WIDTH - 1, VGA_HEIGHT);
		do { interrupt_wait(); } while (PS2_KEY == '\r');
		shift(0, VGA_HEIGHT - 1);
	}

	while (1) sleep(UINT_MAX, false);
}
