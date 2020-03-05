///////////////////////////////////////////////////////////////////////////////
// Program         -- The HAVK Operating System                              //
// Filename        -- boot.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2019-2020                //
///////////////////////////////////////////////////////////////////////////////

// Read the UEFI specifications first: https://uefi.org/specifications
// The purpose of this file is to boot HAVK. It utilizes GNU EFI.
// If you're reading this to figure out how to create a UEFI application
// that loads an ELF file to memory, then create a random freestanding
// program via e.g. C and change the "HAVK_LOCATION" to point towards your
// program. There is no guarantee it will work, as I may have parsed it wrong.
// You will also need to provide some additional symbols for virtual addresses.
#include <efi.h>
#include <efilib.h>

// Magic number. ASCII for "UEFI".
#define BOOTLOADER_MAGIC 0x55454649ULL

// This should be defined and controlled by the Makefile.
#ifndef HAVK_VERSION
	#define HAVK_VERSION u"\?\?-\?\?-\?\?" // Avoid the silly C trigraphs.
#endif

// If the Makefile forgets to define the location of HAVK's ELF file
// when passing it to GCC, then define it to a default location.
#ifndef HAVK_LOCATION
	#define HAVK_LOCATION u"\\HAVK\\HAVK.elf"
#endif

// Global variables.
#define BUFFER_SIZE (sizeof(CHAR16) * 16) // Temporary input buffer size.
CHAR16 buffer[BUFFER_SIZE + sizeof(CHAR16)] = {0}; // Temporary input buffer.
EFI_STATUS status = EFI_SUCCESS; // Return status from UEFI functions.
EFI_STATUS attempt = EFI_SUCCESS; // Secondary return status variable.
EFI_LOADED_IMAGE *image = NULL; // Also must be retrieved before utilisation.
EFI_PHYSICAL_ADDRESS havk_physical_base = 0; // A to-be-determined address.
UINT64 havk_memory_size = 0; // Used for mapping the entire kernel.
BOOLEAN have_uefi_boot_services = TRUE; // False after the boot services end.

// EFI protocol variables.
EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *simple_file_system;
EFI_FILE_PROTOCOL *root_directory;
EFI_FILE_PROTOCOL *havk;
EFI_GRAPHICS_OUTPUT_PROTOCOL *graphics_output_protocol;

// Syntactic sugar for declaring macros.
#define MACRO_BEGIN do{
#define MACRO_END }while(0)

// Resets the system due to an unrecoverable error. If the boot services
// are available, then it will explain the reason for the reboot. Otherwise,
// it will be silent.
#define CRITICAL_FAILURE(x, ...)\
MACRO_BEGIN\
	if (have_uefi_boot_services)\
	{\
		uefi_call_wrapper(ST->ConOut->SetAttribute, 2,\
			ST->ConOut,\
			EFI_TEXT_ATTR(EFI_WHITE, EFI_RED));\
		Print(x, ## __VA_ARGS__);\
		Print(u"FAILED TO BOOT HAVK\r\n");\
		Print(u"PRESS ANY KEY TO HARD RESTART...\r\n");\
		uefi_call_wrapper(ST->ConOut->SetAttribute, 2,\
			ST->ConOut,\
			EFI_TEXT_ATTR(EFI_LIGHTRED, EFI_BLACK));\
		Pause();\
	}\
	while (TRUE)\
	{\
		uefi_call_wrapper(RT->ResetSystem, 4\
			EfiResetCold\
			EFI_ERROR((x)),\
			0,\
			NULL);\
	}\
MACRO_END

// Checks the EFI return status for an error and prints debugging messages.
#define ERROR_CHECK(x)\
MACRO_BEGIN\
	if ((x) != EFI_SUCCESS || EFI_ERROR((x)))\
	{\
		CRITICAL_FAILURE(u"UEFI ERROR: %d - \"%r\" (LINE %d)\r\n",\
			x, x, __LINE__);\
	}\
MACRO_END

// A wrapper for the wrapper due to laziness.
#define UEFI(...)\
MACRO_BEGIN\
	status = uefi_call_wrapper(__VA_ARGS__);\
	ERROR_CHECK(status);\
MACRO_END

// GDB debug specific functions, variables, etc. This relies on the Makefile.
#ifdef HAVK_GDB_DEBUG
	// Spinlock that must be modified by GDB to let it continue.
	#define GDB_BREAKPOINT()\
	MACRO_BEGIN\
		CONST VOLATILE BOOLEAN gdb_ready = FALSE;\
		while (!gdb_ready) __builtin_ia32_pause();\
	MACRO_END

	#define BREAK() GDB_BREAKPOINT()

	VOID setup_debug_session(VOID)
	{
		CONST EFI_PHYSICAL_ADDRESS text_section_base
			= (EFI_PHYSICAL_ADDRESS) image->ImageBase
			+ TEXT_OFFSET;
		CONST EFI_PHYSICAL_ADDRESS data_section_base
			= (EFI_PHYSICAL_ADDRESS) image->ImageBase
			+ DATA_OFFSET;

		Print(u"! BOOTLOADER IMAGE BASE: 0x%X\r\n",
			image->ImageBase);
		Print(u"! ESTIMATIONS FOR SECTIONS:\r\n");
		Print(u"        ! BOOTLOADER TEXT BASE: 0x%X\r\n",
			text_section_base);
		Print(u"        ! BOOTLOADER DATA BASE: 0x%X\r\n",
			data_section_base);
		Print(u"! EXECUTE `make uefi-gdb EFI_IMAGE_BASE=0x%X`"
			" TO BEGIN\r\n",
			image->ImageBase);

		GDB_BREAKPOINT();
	}
#else
	#define BREAK() // Defined, but does nothing to stay compatible.
#endif

// A macro for debugging. It indicates if a point was reached before any
// system crash etc. Note that I couldn't figure out how to prefix the
// `__func__` macro with an "L" or a "u", nor do I think it's possible.
// If one could get it to expand into a string first and then use the "##"
// operator, then it could work, but I don't see how.
#define CHECKPOINT()\
MACRO_BEGIN\
	if (have_uefi_boot_services)\
	{\
		Print(u"PASSED CHECKPOINT (%hs:%d)\r\n",\
			ansi_to_wide((CHAR8 *)__func__), __LINE__);\
	}\
MACRO_END

// The poor man's sleep. Sometimes you need a tiny delay until a UEFI function
// actually finishes its job, like for example the `SetMode` function for GOP.
// UEFI timer events is overkill for this purpose. Avoid the built-in pause,
// as it has no effect when "-fno-builtin" is passed to GCC.
#define SPIN_DELAY(x)\
MACRO_BEGIN\
	for (VOLATILE UINT64 __TMP_ ## __LINE__ = 0;\
		__TMP_ ## __LINE__ < ((x) * 100000ULL); __TMP_ ## __LINE__++)\
	{\
		__asm__ VOLATILE ("PAUSE" ::: "memory");\
	}\
MACRO_END

// I did these quickly, so something could be wrong, but it works for me.
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
struct elf64_file_header
{
	UINT8 identity[16];
	UINT16 type;
	UINT16 isa;
	UINT32 version;
	UINT64 entry_address;
	UINT64 program_header_offset;
	UINT64 section_header_offset;
	UINT32 flags;
	UINT16 file_header_size;
	UINT16 program_header_size;
	UINT16 program_header_entries;
	UINT16 section_header_size;
	UINT16 section_header_entries;
	UINT16 section_header_name_index;
} __attribute__((packed));

struct elf64_program_header
{
	UINT32 type;
	UINT32 flags;
	UINT64 offset;
	UINT64 virtual_address;
	UINT64 physical_address;
	UINT64 size;
	UINT64 memory_size;
	UINT64 alignment;
} __attribute__((packed));

struct elf64_section_header
{
	UINT32 name;
	UINT32 type;
	UINT64 flags;
	UINT64 virtual_address;
	UINT64 offset;
	UINT64 size;
	UINT32 index;
	UINT32 information;
	UINT64 alignment;
	UINT64 entry_size;
} __attribute__((packed));

struct elf64_symbol
{
	UINT32 name;
	UINT8 information;
	UINT8 visibility;
	UINT16 section_index;
	UINT64 virtual_address;
	UINT64 size;
} __attribute__((packed));

struct elf64_file
{
	struct elf64_file_header file_header;
	struct elf64_program_header *program_headers;
	struct elf64_section_header *section_headers;
	struct elf64_symbol *symbols;
	UINT64 symbol_entries;
	CHAR8 *symbol_string_table;
};

// This gets passed to HAVK's entry function later on. I've just made it
// into a structure for the sake of less parameters needing to be passed.
// All "UINTN" types should be changed to "UINT64" to avoid ambiguity.
struct uefi_arguments
{
	UINT32 graphics_mode_current;
	UINT32 graphics_mode_max;
	EFI_PHYSICAL_ADDRESS framebuffer_address;
	UINT64 framebuffer_size;
	UINT32 horizontal_resolution;
	UINT32 vertical_resolution;
	UINT32 pixels_per_scanline;
	EFI_GRAPHICS_PIXEL_FORMAT pixel_format;
	EFI_PIXEL_BITMASK pixel_bitmask;
	EFI_MEMORY_DESCRIPTOR *memory_map;
	UINT64 memory_map_key;
	UINT64 memory_map_size;
	UINT64 memory_map_descriptor_size;
	UINT32 memory_map_descriptor_version;
	EFI_PHYSICAL_ADDRESS root_system_description_pointer;
	EFI_PHYSICAL_ADDRESS physical_base;
} __attribute__((packed));

// Can't modify the default page structure, so I need to make my own.
// See the "HAVK_Kernel.Paging" Ada package for the idea of this. Only suitable
// for huge/2-MiB pages. Don't pass this to HAVK, it's only temporary.
// TODO: A limitation of this structure is that it requires one huge block, as
// the page level objects are right next to each other. That can be improved.
struct uefi_page_structure
{
	#define HUGE_PAGE_SIZE 0x200000
	#define HUGE_PAGE_ALIGN(x) ((x) - ((x) % HUGE_PAGE_SIZE))
	#define HUGE_PAGE_SHIFT 21
	#define HUGE_PAGE_MASK 511
	#define HUGE_PAGE_POINTER 3 // Present, writable, user access.
	#define HUGE_PAGE_PRESENT 131 // Present, writable, user access, huge.
	#define SIZE_TO_HUGE_PAGES(x) \
		(((x) >> HUGE_PAGE_SHIFT) + (((x) & HUGE_PAGE_MASK) ? 1 : 0))

	VOLATILE UINT64 map[512] ALIGN(EFI_PAGE_SIZE);
	VOLATILE UINT64 pointers[512][512] ALIGN(EFI_PAGE_SIZE);
	VOLATILE UINT64 table[512][512] ALIGN(EFI_PAGE_SIZE);
};

// Since I am unable to give a prefix to the `__func__` macro, I think I have
// to convert it at runtime. UEFI is too Microsoft-centric. This only respects
// EASCII, it merely pads out the rest of the 16-bit character's size. The
// "efilib.h" header could at least provide a real method for this conversion.
// I couldn't figure out how to do this with the many print functions in there.
// This passes back an internal static buffer to avoid the need for freeing.
CHAR16 *ansi_to_wide(CHAR8 *ansi_string)
{
	CONST UINT64 ansi_length = strlena(ansi_string);
	static CHAR8 wide_string[128 * sizeof(CHAR16)]; // Max 128 characters.

	if (ansi_length * sizeof(CHAR16) > 128 * sizeof(CHAR16))
	{
		wide_string[0] = (UINT32) 0U;
	}
	else
	{
		for (UINT64 a = 0, w = 0; a < ansi_length; ++a, ++w)
		{
			wide_string[w] = ansi_string[a];
			wide_string[++w] = 0;
		}
	}

	return (CHAR16 *)wide_string;
}

// A function that prints some welcome information and sets up the environment.
VOID welcome(EFI_HANDLE image_handle, EFI_SYSTEM_TABLE *system_table)
{
	// Set up both "system table" and "boot services" access shortcuts.
	// Note that you will encounter both UEFI function calls with my
	// wrapper or the shortcut functions found in "efilib.h".
	// See all references to "Allocate(Zero)Pool" for the general idea.
	InitializeLib(image_handle, system_table);

	// Set the colours for the text.
	UEFI(ST->ConOut->SetAttribute, 2,
		ST->ConOut,
		EFI_TEXT_ATTR(EFI_LIGHTRED, EFI_BLACK));

	// Clear the screen.
	UEFI(ST->ConOut->ClearScreen, 1,
		ST->ConOut);

	// Disable the watchdog timer just in case.
	UEFI(BS->SetWatchdogTimer, 4,
		0,
		0,
		0,
		NULL);

	// Retrieve the loaded image. Useful for things that are later shown.
	UEFI(BS->HandleProtocol, 3,
		image_handle,
		&LoadedImageProtocol,
		&image);

	// Print the version notification in the centre of the screen.
	// TODO: Do the minor revision values as well, they're encoded in BCD
	// and are in the lower 16-bits of "FirmwareRevision".
	PrintAt(40 - 8, 1, u"NOW BOOTING HAVK\r\n");
	PrintAt(40 - 8, 2, u"VERSION %s\r\n\n", HAVK_VERSION);
	Print(u"UEFI FIRMWARE VENDOR: %s (REVISION %u)\r\n",
		system_table->FirmwareVendor,
		system_table->FirmwareRevision >> 16);

	#ifdef HAVK_GDB_DEBUG
		Print(u"! COMPILED WITH GDB SPECIFIC DEBUGGING FUNCTIONS\r\n");
	#endif
}

EFI_HANDLE get_sfs_handle(VOID)
{
	// Prepare to get the correct simple file system handle.
	CONST EFI_GUID simple_file_system_guid
		= EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;
	EFI_HANDLE *handles = NULL;
	UINTN handles_found = 0;

	// Find handles that support the simple file system protocol.
	UEFI(BS->LocateHandleBuffer, 5,
		ByProtocol,
		&simple_file_system_guid,
		NULL,
		&handles_found,
		&handles);

	if (!handles_found)
	{
		CRITICAL_FAILURE(u"UNAVAILABLE TO FIND A "
			"SIMPLE FILE SYSTEM HANDLE\r\n");
	}
	else
	{
		Print(u"SIMPLE FILE SYSTEM PROTOCOL HANDLES: %llu\r\n",
			handles_found);
	}

	UINTN handle_selected = 0; // EFI_HANDLE index.

	if (handles_found == 1)
	{
		handle_selected = 0; // Only a single element, so index zero.
	}
	else
	{
		Print(u"ENTER THE HANDLE NUMBER (1 to %llu)", handles_found);
		Input(u": ", buffer, BUFFER_SIZE);
		Print(u"\r\n");
		handle_selected = Atoi(buffer) - 1;
	}

	return handles[handle_selected];
}

VOID open_havk(EFI_HANDLE drive_handle)
{
	CONST EFI_GUID simple_file_system_guid
		= EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;

	// Use the simple file system protocol to explore the FAT volume.
	UEFI(BS->HandleProtocol, 3,
		drive_handle,
		&simple_file_system_guid,
		&simple_file_system);

	// Open the root directory of the volume.
	UEFI(simple_file_system->OpenVolume, 2,
		simple_file_system,
		&root_directory);

	// Open HAVK's ELF file in read only mode.
	UEFI(root_directory->Open, 5,
		root_directory,
		&havk,
		HAVK_LOCATION,
		EFI_FILE_MODE_READ,
		EFI_FILE_READ_ONLY);

	Print(u"HAVK OPENED AT \"%s\"\r\n", HAVK_LOCATION);
	Print(u"HAVK KERNEL FILE SIZE: %llu KIBIBYTES\r\n",
		LibFileInfo(havk)->FileSize / 1024);

	// Reset the HAVK file's position just to be sure.
	UEFI(havk->SetPosition, 2,
		havk,
		0);
}

VOID read_file_header(struct elf64_file *havk_elf)
{
	CONST UINTN elf64_file_header_size = sizeof(struct elf64_file_header);

	// Read HAVK's ELF file header.
	UEFI(havk->Read, 3,
		havk,
		&elf64_file_header_size,
		&havk_elf->file_header);

	// Check for the ELF magic numbers.
	if (strncmpa(havk_elf->file_header.identity, (CHAR8 *)"\x7F" "ELF", 3))
	{
		CRITICAL_FAILURE(u"HAVK ELF FILE'S MAGIC "
			"NUMBERS ARE CORRUPT\r\n");
	}

	if (havk_elf->file_header.isa != 62)
	{
		CRITICAL_FAILURE(u"HAVK ELF FILE WAS NOT "
			"CREATED FOR x86-64\r\n");
	}

	// The entry address will always be the virtual one and in
	// the file header.
	Print(u"HAVK VIRTUAL ENTRY ADDRESS: 0x%llX\r\n",
		havk_elf->file_header.entry_address);
}

VOID read_program_headers(struct elf64_file *havk_elf)
{
	// Now it's time to load HAVK's ELF program headers.
	CONST UINTN havk_program_headers_size
		= havk_elf->file_header.program_header_size
		* havk_elf->file_header.program_header_entries;

	// Set the position to the offset of the program headers.
	UEFI(havk->SetPosition, 2,
		havk,
		havk_elf->file_header.program_header_offset);

	// Allocate pool memory for all of the program headers.
	UEFI(BS->AllocatePool, 3,
		EfiLoaderData,
		havk_program_headers_size,
		&havk_elf->program_headers);

	// Now get all of the program headers.
	UEFI(havk->Read, 3,
		havk,
		&havk_program_headers_size,
		havk_elf->program_headers);
}

VOID read_section_headers(struct elf64_file *havk_elf)
{
	// I'm also going to get the section headers for finding symbols.
	UINTN havk_section_headers_size
		= havk_elf->file_header.section_header_size
		* havk_elf->file_header.section_header_entries;

	// Set the position to the offset of the section headers.
	UEFI(havk->SetPosition, 2,
		havk,
		havk_elf->file_header.section_header_offset);

	// Allocate pool memory for all of the section headers.
	UEFI(BS->AllocatePool, 3,
		EfiLoaderData,
		havk_section_headers_size,
		&havk_elf->section_headers);

	// Now get all of the section headers.
	UEFI(havk->Read, 3,
		havk,
		&havk_section_headers_size,
		havk_elf->section_headers);
}

VOID read_symbols(struct elf64_file *havk_elf)
{
	for (UINT64 i = 0;
		i < havk_elf->file_header.section_header_entries; ++i)
	{
		// Look for the "SHT_SYMTAB" type, which is 0x2.
		if (havk_elf->section_headers[i].type != 0x2)
		{
			continue;
		}

		UEFI(havk->SetPosition, 2,
			havk,
			havk_elf->section_headers[i].offset);

		UEFI(BS->AllocatePool, 3,
			EfiLoaderData,
			havk_elf->section_headers[i].size,
			&havk_elf->symbols);

		UEFI(havk->Read, 3,
			havk,
			&havk_elf->section_headers[i].size,
			havk_elf->symbols);

		havk_elf->symbol_entries = havk_elf->section_headers[i].size
			/ havk_elf->section_headers[i].entry_size;

		// Only one symbol table should be present.
		break;
	}

	for (UINT64 i = 0;
		i < havk_elf->file_header.section_header_entries; ++i)
	{
		// Look for the "SHT_STRTAB" type, which is 0x3.
		// The ".strtab" and ".shstrtab" have identical sections apart
		// from size and address, but I want the former (symbol names).
		// Thankfully, the file header tells me where the index of the
		// string table is for sections, so we can avoid it.
		if (havk_elf->section_headers[i].type != 0x3 ||
			i == havk_elf->file_header.section_header_name_index)
		{
			continue;
		}

		UEFI(havk->SetPosition, 2,
			havk,
			havk_elf->section_headers[i].offset);

		// Allocate pool memory for all of the section headers.
		UEFI(BS->AllocatePool, 3,
			EfiLoaderData,
			havk_elf->section_headers[i].size,
			&havk_elf->symbol_string_table);

		// Obtain the symbol string table.
		UEFI(havk->Read, 3,
			havk,
			&havk_elf->section_headers[i].size,
			havk_elf->symbol_string_table);

		// Only one string table for symbols should be present.
		break;
	}
}

EFI_VIRTUAL_ADDRESS symbol_address(struct elf64_file *havk_elf,
	CHAR8 *symbol_name)
{
	for (UINT64 i = 0; i < havk_elf->symbol_entries; ++i)
	{
		// The string is guaranteed to be null-terminated. Giving
		// a length will confuse the search, as e.g. "entry" and
		// "entry.shutdown_spin" both match "entry" when the size
		// passed to a `strnlen` variant is five. This way, we don't
		// need to search for the period after the symbol name.
		CONST UINT64 string_offset = havk_elf->symbols[i].name;

		if (!strcmpa(&havk_elf->symbol_string_table[string_offset],
			symbol_name))
		{
			return havk_elf->symbols[i].virtual_address;
		}
	}

	CRITICAL_FAILURE(u"UNKNOWN SYMBOL REQUESTED: %s\r\n",
		ansi_to_wide(symbol_name));
	return 0;
}

VOID load_havk(struct elf64_file *havk_elf)
{
	// Since the physical base can be everywhere as long as the data is
	// loaded in a consecutive manner, we will have to check if the
	// segments can all be loaded from a specific base address onwards.
	// If they can only be loaded partially before an allocation failure,
	// then we will need to backtrack and deallocate the previous segments.
	// The below variable counts that. When true, a segment was allocated
	// successfully. When false, there was no allocation.
	BOOLEAN allocated[UINT8_MAX]; // Avoid VLAs. 255 is more than enough.

	// Begin from 16-MiB. May need the lower space for old ISA devices or
	// whatever other random requirement. A reminder that the physical
	// base must match up from the start at the virtual base, which as of
	// now, means both must be aligned to 2-MiB (an x86-64 huge page).
	havk_physical_base = 8 * HUGE_PAGE_SIZE;

	if (havk_elf->file_header.program_header_entries > UINT8_MAX)
	{
		CRITICAL_FAILURE(u"TOO MANY ELF PROGRAM HEADER ENTRIES (%hu)"
			"\r\n", havk_elf->file_header.program_header_entries);
	}

	// Set a default value without using designated initialisers.
	for (UINT8 i = 0; i < UINT8_MAX; ++i)
	{
		allocated[i] = FALSE;
	}

	// Time to load the segments into physical memory.
	for (INT32 i = 0; // Sign of the integer matters. See the inner loop.
		i < havk_elf->file_header.program_header_entries; ++i)
	{
		// If the type is not "LOAD", then skip it.
		if (havk_elf->program_headers[i].type != 0x1)
		{
			allocated[i] = FALSE;
			continue;
		}

		#define SEGMENT_PHYSICAL_ADDRESS \
			(havk_elf->program_headers[i].physical_address\
				+ havk_physical_base)
		#define SEGMENT_MEMORY_SIZE \
			(havk_elf->program_headers[i].memory_size)

		// For referencing purposes. Only use the macro variant in the
		// deallocation loop.
		CONST EFI_PHYSICAL_ADDRESS segment_physical_address
			= SEGMENT_PHYSICAL_ADDRESS;

		// Try to allocate a page in order to store the segment.
		// Need custom error handling, so my wrapper is not used here.
		status = uefi_call_wrapper(BS->AllocatePages, 4,
			AllocateAddress,
			EfiLoaderData,
			EFI_SIZE_TO_PAGES(SEGMENT_MEMORY_SIZE),
			&segment_physical_address);

		// As previously explained, if allocation failed, then we have
		// to deallocate previous allocations. It must be consecutive.
		if (status == EFI_NOT_FOUND)
		{
			for (--i; i >= 0; --i) // Reverse the loop.
			{
				if (allocated[i])
				{
					UEFI(BS->FreePages, 2,
						SEGMENT_PHYSICAL_ADDRESS,
						EFI_SIZE_TO_PAGES(
							SEGMENT_MEMORY_SIZE));
					allocated[i] = FALSE;
				}
			}

			// Move the base up by a huge page and try again.
			// Right now, my temporary UEFI paging functionality
			// only supports 2-MiB alignments (huge pages), so the
			// physical base absolutely must begin on a 2-MiB
			// alignment or else the bootloader will jump to trash.
			havk_physical_base += HUGE_PAGE_SIZE;
			havk_memory_size = 0;

			// Negative one so the loop is back at zero after
			// continuing. Set it explicitly just in case.
			i = -1;
			continue;
		}

		// Check if anything else went wrong. If anything did, then it
		// is completely unexpected. The "EFI_NOT_FOUND" error is the
		// only one that could appear in a normal run.
		ERROR_CHECK(status);

		allocated[i] = TRUE;

		// Zero out the segment's memory space just in case.
		// Really only required for the BSS segment, but do it anyway.
		ZeroMem((VOID *)segment_physical_address, SEGMENT_MEMORY_SIZE);

		// Go to the offset of the segment in the file.
		UEFI(havk->SetPosition, 2,
			havk,
			havk_elf->program_headers[i].offset);

		// Load the segment.
		UEFI(havk->Read, 3,
			havk,
			&havk_elf->program_headers[i].size,
			segment_physical_address);

		havk_memory_size += SEGMENT_MEMORY_SIZE;

		#undef SEGMENT_PHYSICAL_ADDRESS
		#undef SEGMENT_MEMORY_SIZE
	}

	Print(u"HAVK VIRTUAL END ADDRESS: 0x%llX\r\n"
		"HAVK PHYSICAL BASE ADDRESS: 0x%llX\r\n"
		"HAVK PHYSICAL END ADDRESS: 0x%llX\r\n"
		"HAVK KERNEL MEMORY SIZE: %llu MEBIBYTES\r\n",
		havk_elf->file_header.entry_address + havk_memory_size,
		havk_physical_base,
		havk_physical_base + havk_memory_size,
		havk_memory_size / 1024 / 1024);
}

EFI_VIRTUAL_ADDRESS prepare_entry(struct elf64_file *havk_elf)
{
	CONST VOLATILE EFI_VIRTUAL_ADDRESS entry
		= havk_elf->file_header.entry_address;

	// Close the files, as we (should) already have HAVK in memory at our
	// specified physical address now.
	UEFI(havk->Close, 1,
		havk);

	UEFI(root_directory->Close, 1,
		root_directory);

	return entry;
}

EFI_HANDLE get_gop_handle(VOID)
{
	// Now to get the framebuffer. GOP is the new version of UGP.
	CONST EFI_GUID graphics_output_protocol_guid
		= EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;
	// Get the graphics output protocol handle. Get rid of the previously
	// found handles for the simple file system protocol.
	EFI_HANDLE *handles = NULL;
	UINTN handles_found = 0;

	// Find handles that support the graphics output protocol.
	UEFI(BS->LocateHandleBuffer, 5,
		ByProtocol,
		&graphics_output_protocol_guid,
		NULL,
		&handles_found,
		&handles);

	if (!handles_found)
	{
		CRITICAL_FAILURE(u"UNABLE TO FIND A GRAPHICS OUTPUT "
			"PROTOCOL HANDLE\r\n");
	}

	Print(u"GRAPHICS OUTPUT PROTOCOL HANDLES: %llu\r\n", handles_found);

	return handles[0]; // TODO: Return the first handle for now.
}

UINTN read_screen_resolutions(EFI_HANDLE gop_handle)
{
	CONST EFI_GUID graphics_output_protocol_guid
		= EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;

	UEFI(BS->HandleProtocol, 3,
		gop_handle,
		&graphics_output_protocol_guid,
		&graphics_output_protocol);

	// The true mode range is 0 to "MaxMode", but I'll display it
	// from the start of 1.
	CONST UINT32 max_graphics_mode
		= graphics_output_protocol->Mode->MaxMode - 1;

	Print(u"AVAILABLE GRAPHICS MODES:\r\n");

	// Display every single resolution possible.
	for (UINT32 i = 1; i <= max_graphics_mode; i++)
	{
		EFI_GRAPHICS_OUTPUT_MODE_INFORMATION *queried_mode_information;

		UEFI(graphics_output_protocol->QueryMode, 4,
			graphics_output_protocol,
			i - 1,
			sizeof(queried_mode_information),
			&queried_mode_information);

		// Don't bother showing non-framebuffer resolutions. Even
		// if the user specifies its mode, later code logic should
		// query it again and state why it was not shown. Anything
		// higher than this enumeration is also unsupported.
		if (queried_mode_information->PixelFormat >= PixelBltOnly)
			continue;

		// What follows is just pretty printing. Specific spaces are
		// in their hexidecimal representation for easier viewing.

		Print((i < 10) ? u"\x20%llu" : u"%llu", i);

		SPrint(buffer, BUFFER_SIZE, u": %ux%u",
			queried_mode_information->HorizontalResolution,
			queried_mode_information->VerticalResolution);
		Print(buffer);

		// Instead of relying on tabs (which don't work everywhere),
		// format it with spaces.
		UINTN display_length = StrnLen(buffer, BUFFER_SIZE);
		for (UINTN x = display_length; x < 13; x++)
		{
			Print(u"\x20");
		}

		// Conserve screen space instead of printing each
		// mode and resolution on a separate line. A minimum
		// of 80x25 is guaranteed for console size.
		if (i % 5 == 0 || i == max_graphics_mode)
		{
			Print(u"\r\n");
		}
	}

	return max_graphics_mode;
}

VOID set_screen_resolution(UINTN max_graphics_mode)
{
	EFI_GRAPHICS_OUTPUT_MODE_INFORMATION *selected_mode_information;
	UINTN selected_mode = 0;

	// Enable the cursor. Helpful for user input. Ignore if it succeeded
	// or not, as it's not vital. It can sometimes fail.
	uefi_call_wrapper(ST->ConOut->EnableCursor, 2,
		ST->ConOut,
		TRUE);

	do // Now handle the inputted graphics mode from the user.
	{
		// Ignore this option when debugging, as the resolution
		// probably doesn't matter and this directive saves some time.
		#ifdef HAVK_GDB_DEBUG
			selected_mode = 1;
			Print(u"! GRAPHICS MODE AUTOMATICALLY SELECTED.\r\n");
			continue;
		#endif

		Print(u"ENTER THE GRAPHICS MODE (1 to %u)", max_graphics_mode);

		Input(u": ", buffer, BUFFER_SIZE);
		Print(u"\r\n");
		selected_mode = Atoi(buffer);

		if (selected_mode < 1 || selected_mode > max_graphics_mode)
		{
			Print(u"GRAPHICS MODE IS OUT OF RANGE\r\n");
			continue;
		}

		// Now validate the graphics mode's settings.
		UEFI(graphics_output_protocol->QueryMode, 4,
			graphics_output_protocol,
			selected_mode - 1,
			sizeof(selected_mode_information),
			&selected_mode_information);

		// Don't use any resolutions where a direct framebuffer is not
		// granted, as the bit block transfer function is only
		// usable during UEFI's boot services. Anything higher than
		// the BLT enumeration is also invalid as of writing this,
		// along with the pixel bitmask format.
		switch (selected_mode_information->PixelFormat)
		{
			case PixelBitMask:
				// TODO: I intend to support pixel bitmasks,
				// but most systems return BGR or RGB, so it
				// seems to not be a high priority.
				Print(u"GRAPHICS MODE %llu USES AN UNSUPPORTED"
					" PIXEL FORMAT\r\n", selected_mode);
				selected_mode = 0;
				break;
			case PixelBltOnly:
			case PixelFormatMax:
				Print(u"GRAPHICS MODE %llu DOES NOT GIVE A "
					"FRAMEBUFFER\r\n", selected_mode);
				selected_mode = 0;
				break;
			default:
				Print(u"GRAPHICS MODE %llu IS VALID\r\n",
					selected_mode);
				break;
		}
	}
	while (selected_mode < 1 || selected_mode > max_graphics_mode);

	UEFI(graphics_output_protocol->SetMode, 2,
		graphics_output_protocol,
		selected_mode - 1);

	// This call to disable the cursor fails, but it still does what it is
	// supposed to do on OVMF, oddly enough. Ignore the returned status.
	uefi_call_wrapper(ST->ConOut->EnableCursor, 2,
		ST->ConOut,
		FALSE);

	// Print the version header again, just for consistency. This also
	// often fails likely to do with the graphics mode switch in effect.
	uefi_call_wrapper(ST->ConOut->ClearScreen, 1,
		ST->ConOut);

	PrintAt(40 - 8, 1, u"PREPARING ENTRY\r\n\n");

	SPIN_DELAY(10); // Wait for the graphics mode to finish switching.
}

VOID get_graphics_information(VOLATILE struct uefi_arguments *arguments)
{
	EFI_GRAPHICS_OUTPUT_MODE_INFORMATION *graphics_information;

	// Get information for the user's selected mode.
	UEFI(graphics_output_protocol->QueryMode, 4,
		graphics_output_protocol,
		graphics_output_protocol->Mode->Mode,
		sizeof(graphics_information),
		&graphics_information);

	// Now fill in our self-defined "graphics map".
	arguments->graphics_mode_current
		= graphics_output_protocol->Mode->Mode + 1;
	arguments->graphics_mode_max
		= graphics_output_protocol->Mode->MaxMode - 1;
	arguments->framebuffer_address
		= graphics_output_protocol->Mode->FrameBufferBase;
	arguments->framebuffer_size
		= graphics_output_protocol->Mode->FrameBufferSize;
	arguments->horizontal_resolution
		= graphics_information->HorizontalResolution;
	arguments->vertical_resolution
		= graphics_information->VerticalResolution;
	arguments->pixels_per_scanline
		= graphics_information->PixelsPerScanLine;
	arguments->pixel_format
		= graphics_information->PixelFormat;

	switch (arguments->pixel_format)
	{
		case PixelBlueGreenRedReserved8BitPerColor:
			arguments->pixel_bitmask.RedMask = 0xFF0000;
			arguments->pixel_bitmask.GreenMask = 0xFF00;
			arguments->pixel_bitmask.BlueMask = 0xFF;
			arguments->pixel_bitmask.ReservedMask = 0;
			break;
		case PixelRedGreenBlueReserved8BitPerColor:
			arguments->pixel_bitmask.RedMask = 0xFF;
			arguments->pixel_bitmask.GreenMask = 0xFF00;
			arguments->pixel_bitmask.BlueMask = 0xFF0000;
			arguments->pixel_bitmask.ReservedMask = 0;
			break;
		case PixelBitMask:
			arguments->pixel_bitmask
				= graphics_information->PixelInformation;
			break;
		default:
			CRITICAL_FAILURE(u"PIXEL FORMAT IS CORRUPT "
				"OR UNKNOWN\r\n");
			break;
	}

	Print(u"CURRENT GRAPHICS MODE: %u\r\n",
		arguments->graphics_mode_current);

	Print(u"VIDEO RESOLUTION: %ux%u\r\n",
		arguments->horizontal_resolution,
		arguments->vertical_resolution);

	Print(u"VIDEO FRAMEBUFFER RANGE: 0x%llX to 0x%llX\r\n",
		arguments->framebuffer_address,
		arguments->framebuffer_address + arguments->framebuffer_size);
}

// Retrieves the RSDP for ACPI 2.0. ACPI 1.0 is not supported, as it is
// too old and it does not even support x86-64 due to it lacking the XSDT.
EFI_PHYSICAL_ADDRESS get_rsdp(EFI_SYSTEM_TABLE *system_table)
{
	CONST EFI_GUID acpi_guid = ACPI_20_TABLE_GUID;

	for (UINTN i = 0; i < system_table->NumberOfTableEntries; i++)
	{
		CONST EFI_GUID table_guid
			= system_table->ConfigurationTable[i].VendorGuid;
		CONST EFI_PHYSICAL_ADDRESS table
			= (EFI_PHYSICAL_ADDRESS)
			system_table->ConfigurationTable[i].VendorTable;

		if (!CompareMem(&table_guid, &acpi_guid, sizeof(EFI_GUID)))
		{
			Print(u"ACPI 2.0 RSDP ADDRESS: 0x%llX\r\n", table);
			return table;
		}
	}

	// TODO: For now, HAVK expects an system with minimum ACPI 2.0 support.
	// If ACPI 2.0 isn't supported, then something is wrong.
	CRITICAL_FAILURE(u"ACPI 2.0 IS NOT SUPPORTED ON YOUR SYSTEM\r\n");

	return 0;
}

// This also exits boot services, or else the memory map would quickly become
// invalid for exiting boot services and also inaccurate.
VOID get_memory_map(VOLATILE struct uefi_arguments *arguments,
	EFI_HANDLE image_handle)
{
	// From here on out, do not call anything if it is not compulsory.
	// Otherwise it can mess with the memory map and whatnot.

	// Begin to get the memory map, which is needed before we exit UEFI
	// and its boot services. Initialize to non-garbage defaults.
	arguments->memory_map = NULL;
	arguments->memory_map_key = 0;
	arguments->memory_map_size = 0;
	arguments->memory_map_descriptor_size = 0; // Don't use `sizeof()`.
	arguments->memory_map_descriptor_version = 0;

	// TODO: Figure out the memory map's size some proper way.
	// So far, I can't figure out how to do it from looking
	// at the official UEFI specifications themselves. For now, I'm
	// completely (over)estimating the amount I will need.
	// You need memory to store the memory map, so allocating space
	// for the memory map should change the memory map itself?
	//
	// After doing more reading, the required memory map size is returned
	// by the `GetMemoryMap()` UEFI function if the buffer is too small.
	// I don't think it's necessary to work with it, as my current method
	// does the job, albeit not very tidily.
	//
	// Guess the memory map's size for now so we can make space for it.
	// I'm going to go with 10 KiB.
	arguments->memory_map_size = 10240;

	// Allocate pool memory for the memory map.
	UEFI(BS->AllocatePool, 3,
		EfiLoaderData,
		arguments->memory_map_size,
		&arguments->memory_map);

	// Now get the memory map itself.
	do attempt = uefi_call_wrapper(BS->GetMemoryMap, 5,
		&arguments->memory_map_size,
		arguments->memory_map,
		&arguments->memory_map_key,
		&arguments->memory_map_descriptor_size,
		&arguments->memory_map_descriptor_version);
	while (attempt == EFI_BUFFER_TOO_SMALL);
	ERROR_CHECK(attempt);

	// Finally exit UEFI and its boot services. A valid fresh memory map
	// is mandatory or else this will completely fail to succeed.
	UEFI(BS->ExitBootServices, 2,
		image_handle,
		arguments->memory_map_key);

	have_uefi_boot_services = FALSE;
}

// `malloc()` that returns an address which is page-aligned to 0x1000 (4 KiB).
// The highest level of the page structure must be aligned, so that is used
// to align the page structure, where the first member of the struct itself
// is the highest level (the map/PML4). `AllocatePool()` and its zeroed
// shortcut variant both return an 8-KiB aligned value if I remember correctly.
VOID *page_malloc(UINT64 size)
{
	CONST EFI_PHYSICAL_ADDRESS zeroed_pool = (EFI_PHYSICAL_ADDRESS)
		AllocateZeroPool(size + EFI_PAGE_SIZE);

	return (VOID *)((zeroed_pool + 1 + EFI_PAGE_MASK) & ~EFI_PAGE_MASK);
}

// This maps the kernel's virtual space. Instead of creating a new dictionary,
// I've just reused the UEFI dictionary. This must be called after the boot
// services have ended, not before. Will always map the base address.
VOID map_address_range(VOLATILE struct uefi_page_structure *structure,
	EFI_VIRTUAL_ADDRESS virtual, EFI_PHYSICAL_ADDRESS physical,
	UINT64 size)
{
	// This is a very rushed and less detailed version of the functionality
	// inside "HAVK_Kernel.Paging". I've done this quick.

	// Align all the addresses if required and at least map one huge page.
	virtual = virtual % HUGE_PAGE_SIZE
		? HUGE_PAGE_ALIGN(virtual) : virtual;
	physical = physical % HUGE_PAGE_SIZE
		? HUGE_PAGE_ALIGN(physical) : physical;
	size = size > HUGE_PAGE_SIZE
		? SIZE_TO_HUGE_PAGES(size) : 1;

	for (UINT64 i = 0; i < size; ++i)
	{
		#define INCREMENT(x) ((x) + HUGE_PAGE_SIZE * i)

		CONST UINT16 map_offset
			= (INCREMENT(virtual) >> 39)
			& HUGE_PAGE_MASK;
		CONST UINT16 pointer_offset
			= (INCREMENT(virtual) >> 30)
			& HUGE_PAGE_MASK;
		CONST UINT16 table_offset
			= (INCREMENT(virtual) >> 21)
			& HUGE_PAGE_MASK;

		structure->map[map_offset]
			= (UINT64) &structure->pointers[map_offset]
			| HUGE_PAGE_POINTER;

		structure->pointers[map_offset][pointer_offset]
			= (UINT64) &structure->table[pointer_offset]
			| HUGE_PAGE_POINTER;

		structure->table[pointer_offset][table_offset]
			= INCREMENT(physical)
			| HUGE_PAGE_PRESENT;

		#undef INCREMENT
	}
}

// After the the UEFI boot services have ended, we then obtain
// the right to mess with the MMU. Unfortunately, the UEFI page
// structure is only mapped to a virtual address which I cannot
// write to (modify), at least for x86-64 on OVMF with QEMU.
// These are the default mappings for now. Not bothered to precisely
// map UEFI loader data, the kernel will do it soon enough.
VOID default_mappings(struct elf64_file *havk_elf,
	VOLATILE struct uefi_page_structure *structure)
{
	CONST EFI_VIRTUAL_ADDRESS havk_virtual_base
		= symbol_address(havk_elf, (CHAR8 *)"__kernel_virtual_base");

	CONST UINT64 havk_size // Will almost never be aligned to 2-MiB.
		= HUGE_PAGE_ALIGN(symbol_address(havk_elf,
		(CHAR8 *)"__kernel_size")) + HUGE_PAGE_SIZE;

	// I've given up trying to map every important thing one by one, so
	// I've just mapped 0-GiB to 4-GiB entirely. OVMF does this anyway, but
	// adds special pages (essentially restrictions) for certain ranges.
	map_address_range(structure, 0, 0, 0x100000000);

	// Map the kernel itself. This includes the BSS. Note that the
	// memory size sum of all the segments can be smaller than the
	// "__kernel_size" symbol address/value due to alignment, so it's
	// wiser to use the latter (and align it up) rather than the former.
	// To be safe, I will just use the biggest value at runtime.
	map_address_range(structure,
		havk_virtual_base,
		havk_physical_base,
		havk_size > havk_memory_size ? havk_size : havk_memory_size);

	Print(u"TEMPORARY PAGE STRUCTURE ADDRESS: 0x%llX\r\n", structure);
}

// This function allocates at the lowest possible address and it works up
// from there.
VOID *low_malloc(UINT64 size)
{
	static EFI_PHYSICAL_ADDRESS lowest_address;
	EFI_STATUS allocation;

	do
	{
		allocation = uefi_call_wrapper(BS->AllocatePages, 4,
			AllocateAddress,
			EfiLoaderData,
			EFI_SIZE_TO_PAGES(size),
			&lowest_address);
		lowest_address += size;
	}
	while (allocation != EFI_SUCCESS);

	return (VOID *)lowest_address;
}

EFIAPI
EFI_STATUS efi_main(EFI_HANDLE image_handle, EFI_SYSTEM_TABLE *system_table)
{
	struct elf64_file havk_elf = {0};
	VOLATILE struct uefi_arguments *arguments = NULL;
	VOLATILE struct uefi_page_structure *page_structure = NULL;

	welcome(image_handle, system_table);

	#if HAVK_GDB_DEBUG
		setup_debug_session();
	#endif

	set_screen_resolution(read_screen_resolutions(get_gop_handle()));

	// TODO: For now, just find the device where the bootloader is
	// also located. This should save a lot of time guessing, but I aim
	// to revise this in the future for more user options.
	/* open_havk(get_sfs_handle()); */
	open_havk(image->DeviceHandle);

	// Process the ELF headers.
	read_file_header(&havk_elf);
	read_program_headers(&havk_elf);
	read_section_headers(&havk_elf);
	read_symbols(&havk_elf);

	// Load HAVK to memory.
	load_havk(&havk_elf);

	arguments = low_malloc(sizeof(arguments));

	get_graphics_information(arguments);

	// Get the RSDP, as HAVK will need ACPI info for PCIe enumeration etc.
	arguments->root_system_description_pointer = get_rsdp(system_table);

	// Since the physical base of the kernel can be placed anywhere as long
	// as it can line up with -2-GiB of the 64-bit address space, this can
	// vary wildly on different systems. The kernel needs it for remapping
	// any parts of itself.
	arguments->physical_base = havk_physical_base;

	// Now prepare the temporary page mappings so we can have
	// a higher-half kernel, but don't switch it yet.
	page_structure = page_malloc(sizeof(page_structure));
	default_mappings(&havk_elf, page_structure);

	// I want to pass the memory map etc. to HAVK, so instead of using
	// inline assembly, I can just tell GCC to use the System V ABI way
	// of passing arguments to our assembly entry function which is
	// using the ELF format and adheres to the System V ABI.
	__attribute__((sysv_abi))
	VOID (*enter_havk) (VOLATILE struct uefi_arguments *arguments,
		UINT64 magic) = (VOLATILE VOID *)prepare_entry(&havk_elf);

	Print(u"EXITING UEFI BOOT SERVICES\r\n");

	get_memory_map(arguments, image_handle);
	// UEFI boot services have ended. Can't e.g. `Print()` anymore.

	// Load the temporary page structure. Intel syntax breaks compilation.
	__asm__ VOLATILE ("MOVQ %0, %%CR3" :: "r" (page_structure) : "memory");

	// TODO: Set this up for the runtime services inside the OS if they
	// are desired. You can only do it once, apparently.
	/* UEFI(RT->SetVirtualAddressMap, 4,
		arguments->memory_map_size,
		arguments->memory_map_descriptor_size,
		arguments->memory_map_descriptor_version,
		arguments->memory_map); */

	// The last stop for the debugger before the session becomes pointless.
	BREAK();

	// Now we leave at last.
	enter_havk(arguments, BOOTLOADER_MAGIC);

	return EFI_SUCCESS;
}
