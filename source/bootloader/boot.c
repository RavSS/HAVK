///////////////////////////////////////////////////////////////////////////////
// Program         -- UEFI bootloader for HAVK                               //
// Filename        -- boot.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2019-2020                //
///////////////////////////////////////////////////////////////////////////////

// Read the UEFI specifications first: https://uefi.org/specifications
// The purpose of this file is to boot HAVK. It utilizes GNU EFI.
// If you're reading this to figure out how to create a UEFI application
// that loads an ELF file to memory, then create a random freestanding
// program via e.g. C and modify the configuration file to point towards it, as
// the bootloader uses the configuration file to start the kernel up.
// TODO: This file is becoming a mess. A rewrite in Ada might be a good idea.
#include <efi.h>
#include <efilib.h>

// Magic number. ASCII for "UEFI".
#define BOOTLOADER_MAGIC 0x55454649ULL

// This should be defined and controlled by the Makefile.
#ifndef HAVK_VERSION
	#define HAVK_VERSION u"\?\?-\?\?-\?\?" // Avoid the silly C trigraphs.
#endif

// This will be the default location of the configuration file.
#ifndef CONFIGURATION_LOCATION
	#define CONFIGURATION_LOCATION "\\EFI\\BOOT\\BOOT.CFG"
#endif

// These are not required to be present, but they're just for sanity checking.
#ifndef VIRTUAL_BASE_SYMBOL // Should be placed at the virtual address base.
	#define VIRTUAL_BASE_SYMBOL "global__kernel_virtual_base"
#endif
#ifndef KERNEL_SIZE_SYMBOL // Total aligned size of all allocated sections.
	#define KERNEL_SIZE_SYMBOL "global__kernel_size"
#endif

// Global variables.
#define BUFFER_SIZE (sizeof(CHAR16) * 16) // Temporary input buffer size.
CHAR16 buffer[BUFFER_SIZE + sizeof(CHAR16)] = {0}; // Temporary input buffer.
EFI_STATUS status = EFI_SUCCESS; // Return status from UEFI functions.
EFI_STATUS attempt = EFI_SUCCESS; // Secondary return status variable.
EFI_LOADED_IMAGE *image = NULL; // Also must be retrieved before utilisation.
EFI_PHYSICAL_ADDRESS havk_physical_base = 0; // A to-be-determined address.
EFI_VIRTUAL_ADDRESS havk_virtual_base = UINTPTR_MAX; // Higher-half expected.
UINT64 havk_memory_size = 0; // Used for mapping the entire kernel.
BOOLEAN have_uefi_boot_services = TRUE; // False after the boot services end.

// EFI protocol variables.
EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *simple_file_system;
EFI_FILE_PROTOCOL *havk;
EFI_GRAPHICS_OUTPUT_PROTOCOL *graphics_output_protocol;

// Syntactic sugar for declaring macros and other helper macros.
#define MACRO_BEGIN do{
#define MACRO_END }while(0)
#define PACKED __attribute__((packed))
#define ARRAY_LENGTH(x) (sizeof((x)) / sizeof((x)[0]))

// Redefine inline so it's not just a hint. I've defined this now so it won't
// cause a problem later on when it's doing something opposite to what we want.
#define inline __inline__ __attribute__((always_inline))

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
		Print(u"%E" "FAILED TO BOOT HAVK\r\n");\
		Print(u"%E" "PRESS ANY KEY TO HARD RESTART...\r\n");\
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
		CRITICAL_FAILURE(\
			u"%E" "UEFI ERROR: %d - \"%r\" (LINE %d)\r\n",\
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
		while (!gdb_ready) __asm__ VOLATILE ("PAUSE" ::: "memory");\
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
		Print(u"! EXECUTE `make uefi-gdb UEFI_IMAGE_BASE=0x%X`"
			" TO BEGIN\r\n",
			image->ImageBase);

		GDB_BREAKPOINT();
	}
#else
	#define BREAK() // Defined, but does nothing to stay compatible.
#endif

// A macro for debugging. It indicates if a point was reached before any
// system crash etc.
#define CHECKPOINT()\
MACRO_BEGIN\
	if (have_uefi_boot_services)\
	{\
		Print(u"PASSED CHECKPOINT (%a:%d)\r\n", __func__, __LINE__);\
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
} PACKED;

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
} PACKED;

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
} PACKED;

struct elf64_symbol
{
	UINT32 name;
	UINT8 information;
	UINT8 visibility;
	UINT16 section_index;
	UINT64 virtual_address;
	UINT64 size;
} PACKED;

struct elf64_file
{
	struct elf64_file_header file_header;
	struct elf64_program_header *program_headers;
	struct elf64_section_header *section_headers;
	struct elf64_symbol *symbols;
	UINT64 symbol_entries;
	CHAR8 *symbol_string_table;
} PACKED;

// I made this little library to parse key-value pair files. See the header for
// more details.
struct boot_file_data_layout
{
	struct elf64_file_header *boot_file_address;
	UINT64 boot_file_size;
} PACKED;
union options_data
{
	struct boot_file_data_layout boot_file_data;
} PACKED;
#define KVP_ENTRY_DATA_FIELDS union options_data data;
#define KVP_ENTRY_ATTRIBUTES PACKED
#define KVP_INDEX_TYPE UINT32
#include <key_value_pair.h>
#define HAVK_KEY "HAVK"
#define BOOT_FILE_KEY "BOOT_FILE."

// This gets passed to HAVK's entry function later on. I've just made it
// into a structure for the sake of less parameters needing to be passed.
// All "UINTN" types should be changed to "UINT64" to avoid ambiguity.
struct bootloader_arguments
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
	kvp_entry *options;
	CHAR8 *options_string;
	UINT16 options_count;
} PACKED;

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

// Basic `malloc()` function which also takes in a memory type/tag, which will
// be seen by HAVK later on. This stores the page size allocation behind the
// returned address. Currently, this halts the entire bootloader if an
// allocation fails.
VOID *malloc(CONST UINT64 size, CONST EFI_MEMORY_TYPE memory_type)
{
	// I believe this won't change on a failed allocation.
	EFI_PHYSICAL_ADDRESS address = (EFI_PHYSICAL_ADDRESS) NULL;
	CONST UINTN page_size = EFI_SIZE_TO_PAGES(size + sizeof(UINTN));

	attempt = uefi_call_wrapper(BS->AllocatePages, 4,
		AllocateAnyPages,
		memory_type,
		page_size,
		&address); // Input does not matter for "AllocateAnyPages".

	if (!address)
	{
		Print(u"MEMORY ALLOCATION FAILED\r\n");
		ERROR_CHECK(attempt);
	}

	*(UINTN *)address = page_size;
	address += sizeof(UINTN);
	ZeroMem((VOID *)address, size);

	return (VOID *)address;
}

// Frees an address allocated by my `malloc()`. Do note that it only works with
// addresses returned by that function, not with any other memory allocation
// function I've made so far.
VOID free(CONST VOID *CONST address)
{
	UEFI(BS->FreePages, 2,
		address - sizeof(UINTN),
		*(UINTN *)(address - sizeof(UINTN)));
}

// This function allocates at the lowest possible address and it works up from
// there. UEFI memory allocation seems to be extremely finnicky. If pool memory
// allocation is behaving eratic, then use this instead.
// TODO: I'm currently not making any attempts to reclaim the memory that this
// returns, but with better usage of the memory types (along with a custom
// one), the kernel itself could do that.
VOID *low_malloc(CONST UINT64 size)
{
	static EFI_PHYSICAL_ADDRESS lowest_address;
	EFI_STATUS allocation;
	CONST UINTN page_size = EFI_SIZE_TO_PAGES(size);

	do
	{
		Print(u"ALLOCATING MEMORY: TRYING 0x%llX\r", lowest_address);
		allocation = uefi_call_wrapper(BS->AllocatePages, 4,
			AllocateAddress,
			EfiLoaderData,
			page_size,
			&lowest_address);

		lowest_address += size;

		if (lowest_address >= UINT32_MAX)
		{
			CRITICAL_FAILURE(u"NO FREE MEMORY LEFT UNDER "
				"4 GiB\r\n");
		}
	}
	while (allocation != EFI_SUCCESS);

	// Clear the allocation wait message with a large space string.
	Print(u"\r                                                        \r");

	ZeroMem((VOID *)lowest_address, size);
	return (VOID *)lowest_address;
}

// `malloc()` that returns an address which is page-aligned to 0x1000 (4 KiB).
// The highest level of the page structure must be aligned, so that is used
// to align the page structure, where the first member of the struct itself
// is the highest level (the map/PML4). `AllocatePool()` and its zeroed
// shortcut variant both return an 8 KiB aligned value if I remember correctly,
// but I'm using `AllocateAddress()` instead.
VOID *page_malloc(CONST UINT64 size)
{
	CONST EFI_PHYSICAL_ADDRESS zeroed_memory
		= (EFI_PHYSICAL_ADDRESS) low_malloc(size + EFI_PAGE_SIZE);

	return (VOID *)((zeroed_memory + 1 + EFI_PAGE_MASK) & ~EFI_PAGE_MASK);
}

// A function that prints some welcome information and sets up the environment.
VOID welcome(CONST EFI_HANDLE CONST image_handle,
	EFI_SYSTEM_TABLE *system_table)
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
	PrintAt(40 - 8, 1, u"NOW BOOTING HAVK\r\n");
	PrintAt(40 - 8, 2, u"VERSION %s\r\n\n", HAVK_VERSION);
	Print(u"UEFI FIRMWARE VENDOR: %s (REVISION %u.%u%u)\r\n",
		system_table->FirmwareVendor,
		system_table->FirmwareRevision >> 16,
		BCDtoDecimal((system_table->FirmwareRevision >> 8) & 0xFF),
		BCDtoDecimal(system_table->FirmwareRevision & 0xFF));

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

EFI_FILE_PROTOCOL *open_root_directory(CONST EFI_HANDLE CONST drive_handle)
{
	CONST EFI_GUID simple_file_system_guid
		= EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;
	EFI_FILE_PROTOCOL *root_directory = NULL;

	// Use the simple file system protocol to explore the FAT volume.
	UEFI(BS->HandleProtocol, 3,
		drive_handle,
		&simple_file_system_guid,
		&simple_file_system);

	// Open the root directory of the volume.
	UEFI(simple_file_system->OpenVolume, 2,
		simple_file_system,
		&root_directory);

	return root_directory;
}

// TODO: Only opens files in read-only mode.
EFI_FILE_PROTOCOL *open_file(CONST EFI_FILE_PROTOCOL *CONST root_directory,
	CONST CHAR8 *CONST file_path)
{
	EFI_FILE_PROTOCOL *file = NULL;

	CHAR16 wide_file_path[256 + 1]; // Max FAT(32) path length.
	wide_file_path[ARRAY_LENGTH(wide_file_path) - 1] = u'\0';
	SPrint(wide_file_path, ARRAY_LENGTH(wide_file_path) - 1,
		u"%a\0", file_path);

	attempt = uefi_call_wrapper(root_directory->Open, 5,
		root_directory,
		&file,
		wide_file_path,
		EFI_FILE_MODE_READ,
		EFI_FILE_READ_ONLY);

	if (attempt == EFI_NOT_FOUND)
	{
		CRITICAL_FAILURE(u"FILE NOT FOUND: \"%s\"\r\n",
			wide_file_path);
	}
	else if (attempt == EFI_INVALID_PARAMETER)
	{
		CRITICAL_FAILURE(u"BAD FILE PATH: \"%s\"\r\n",
			wide_file_path);
	}
	else
	{
		ERROR_CHECK(attempt);
	}

	// Reset the file's position just to be sure.
	UEFI(file->SetPosition, 2,
		file,
		0);

	return file;
}

// TODO: The logic here is not robust, as with most C code that deals with
// parsing raw files. Check it over more finely, as a badly formatted key-value
// pair file will without a doubt make the bootloader crash.
VOID parse_configuration(EFI_FILE_PROTOCOL *CONST root_directory,
	struct bootloader_arguments *CONST arguments)
{
	Print(u"CONFIGURATION FILE: \"%a\"\r\n", CONFIGURATION_LOCATION);

	EFI_FILE_PROTOCOL *CONST configuration
		= open_file(root_directory,
		(CONST CHAR8 *)CONFIGURATION_LOCATION);

	CONST UINTN size = LibFileInfo(configuration)->FileSize; // One-based.

	// Since it will be stored as a string, we need an extra null byte.
	CHAR8 *CONST options_string = malloc(size + 1, EfiLoaderData);

	UEFI(configuration->Read, 3,
		configuration,
		&size,
		options_string);
	options_string[size] = '\0'; // Just to be sure.

	CONST UINTN options_count = kvp_count((char *)options_string, size);

	if (!options_count)
	{
		CRITICAL_FAILURE(u"THE BOOT CONFIGURATION FILE IS EMPTY\r\n");
	}

	arguments->options = malloc(options_count * sizeof(kvp_entry),
		EfiLoaderData);

	kvp_parse((char *)options_string, size, arguments->options,
		options_count);

	UEFI(configuration->Close, 1,
		configuration);

	CONST CHAR8 *spacing = (CONST CHAR8 *)"    "; // For display purposes.
	for (UINTN i = 0; i < options_count; ++i)
	{
		CHAR8 old_end;
		Print(u"OPTION %u:\r\n", i + 1);

		Print(u"%aKEY:   \"", spacing);
		old_end = options_string[arguments->options[i].key_end + 1];
		options_string[arguments->options[i].key_end + 1] = '\0';
		Print(u"%a\"\r\n",
			&options_string[arguments->options[i].key_start]);
		options_string[arguments->options[i].key_end + 1] = old_end;

		Print(u"%aVALUE: \"", spacing);
		old_end = options_string[arguments->options[i].value_end + 1];
		options_string[arguments->options[i].value_end + 1] = '\0';
		Print(u"%a\"\r\n",
			&options_string[arguments->options[i].value_start]);
		options_string[arguments->options[i].value_end + 1] = old_end;
	}

	arguments->options_string = options_string;
	arguments->options_count = options_count;
}

VOID read_file_header(struct elf64_file *CONST havk_elf)
{
	CONST UINTN elf64_file_header_size = sizeof(struct elf64_file_header);

	// Read HAVK's ELF file header.
	UEFI(havk->Read, 3,
		havk,
		&elf64_file_header_size,
		&havk_elf->file_header);

	// Check for the ELF magic numbers.
	if (strncmpa(havk_elf->file_header.identity,
		(CONST CHAR8 *)("\x7F" "ELF"), 4))
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

VOID read_program_headers(struct elf64_file *CONST havk_elf)
{
	// Now it's time to load HAVK's ELF program headers.
	CONST UINTN havk_program_headers_size
		= havk_elf->file_header.program_header_size
		* havk_elf->file_header.program_header_entries;

	// Set the position to the offset of the program headers.
	UEFI(havk->SetPosition, 2,
		havk,
		havk_elf->file_header.program_header_offset);

	// Allocate memory for all of the program headers.
	havk_elf->program_headers
		= malloc(havk_program_headers_size, EfiLoaderData);

	// Now get all of the program headers.
	UEFI(havk->Read, 3,
		havk,
		&havk_program_headers_size,
		havk_elf->program_headers);
}

VOID read_section_headers(struct elf64_file *CONST havk_elf)
{
	// I'm also going to get the section headers for finding symbols.
	UINTN havk_section_headers_size
		= havk_elf->file_header.section_header_size
		* havk_elf->file_header.section_header_entries;

	// Set the position to the offset of the section headers.
	UEFI(havk->SetPosition, 2,
		havk,
		havk_elf->file_header.section_header_offset);

	// Allocate memory for all of the section headers.
	havk_elf->section_headers
		= malloc(havk_section_headers_size, EfiLoaderData);

	// Now get all of the section headers.
	UEFI(havk->Read, 3,
		havk,
		&havk_section_headers_size,
		havk_elf->section_headers);
}

VOID read_symbols(struct elf64_file *CONST havk_elf)
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

		havk_elf->symbols = malloc(havk_elf->section_headers[i].size,
			EfiLoaderData);

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

		// Allocate memory for all of the section headers.
		havk_elf->symbol_string_table = malloc(
			havk_elf->section_headers[i].size, EfiLoaderData);

		// Obtain the symbol string table.
		UEFI(havk->Read, 3,
			havk,
			&havk_elf->section_headers[i].size,
			havk_elf->symbol_string_table);

		// Only one string table for symbols should be present.
		break;
	}
}

EFI_VIRTUAL_ADDRESS symbol_address(struct elf64_file *CONST havk_elf,
	CONST CHAR8 *CONST symbol_name, CONST BOOLEAN required)
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

	if (required)
	{
		CRITICAL_FAILURE(u"UNKNOWN SYMBOL REQUESTED: \"%a\"\r\n",
			symbol_name);
	}

	return 0;
}

VOID load_havk(struct elf64_file *CONST havk_elf)
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
	for (UINTN i = 0; i < ARRAY_LENGTH(allocated); ++i)
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

		// The first "LOAD" segment usually has the virtual base
		// address as the place where it's supposed to be loaded, but
		// I'll be a bit more careful.
		if (havk_elf->program_headers[i].virtual_address <
			havk_virtual_base)
		{
			havk_virtual_base
				= havk_elf->program_headers[i].virtual_address;
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

UINTN read_screen_resolutions(CONST EFI_HANDLE CONST gop_handle)
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

VOID set_screen_resolution(CONST UINTN max_graphics_mode)
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

VOID get_graphics_information(struct bootloader_arguments *CONST arguments)
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
			arguments->pixel_bitmask.BlueMask = 0xFF;
			arguments->pixel_bitmask.GreenMask = 0xFF00;
			arguments->pixel_bitmask.RedMask = 0xFF0000;
			arguments->pixel_bitmask.ReservedMask = 0xFF000000;
			break;
		case PixelRedGreenBlueReserved8BitPerColor:
			arguments->pixel_bitmask.RedMask = 0xFF;
			arguments->pixel_bitmask.GreenMask = 0xFF00;
			arguments->pixel_bitmask.BlueMask = 0xFF0000;
			arguments->pixel_bitmask.ReservedMask = 0xFF000000;
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
EFI_PHYSICAL_ADDRESS get_rsdp(CONST EFI_SYSTEM_TABLE *CONST system_table)
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
VOID get_memory_map(struct bootloader_arguments *CONST arguments,
	CONST EFI_HANDLE CONST image_handle)
{
	// From here on out, do not call anything if it is not compulsory.
	// Otherwise it can mess with the memory map and whatnot.

	// Disable interrupts from here as well so the UEFI-provided IDT
	// entries don't mess with any memory (in case they do).
	__asm__ VOLATILE ("CLI");

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

	// Any UEFI function calls here should be limited to critical failures
	// or else the memory map will most likely become outdated.

	// HAVK depends on the memory map (obviously) so it has to be perfect
	// and without any errors, unknown changes, or corruption.
	#if EFI_MEMORY_DESCRIPTOR_VERSION != 1
		#error "The GNU-EFI descriptor version has statically changed."
	#endif
	if (arguments->memory_map_descriptor_version != 1)
	{
		CRITICAL_FAILURE(u"UNSUPPORTED MEMORY MAP DESCRIPTOR VERSION "
			"(%u).\r\n", arguments->memory_map_descriptor_version);
	}

	// Iterate over the memory map and check the types just to be sure.
	for (VOID *i = arguments->memory_map;
		i < (VOID *)arguments->memory_map + arguments->memory_map_size;
		i += arguments->memory_map_descriptor_size)
	{
		CONST EFI_MEMORY_DESCRIPTOR *region = i;

		if (region->Type > 14) // Maximum region type as of version 1.
		{
			CRITICAL_FAILURE(u"MEMORY MAP IS CORRUPT.\r\n");
		}
	}

	// Finally exit UEFI and its boot services. A valid fresh memory map
	// is mandatory or else this will completely fail to succeed.
	UEFI(BS->ExitBootServices, 2,
		image_handle,
		arguments->memory_map_key);

	have_uefi_boot_services = FALSE;
}

// This maps the kernel's virtual space. Instead of creating a new dictionary,
// I've just reused the UEFI dictionary. This must be called after the boot
// services have ended, not before. Will always map the base address.
VOID map_address_range(struct uefi_page_structure *CONST structure,
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
VOID default_mappings(struct elf64_file *CONST havk_elf,
	struct uefi_page_structure *CONST structure)
{
	CONST EFI_VIRTUAL_ADDRESS havk_virtual_base_check
		= symbol_address(havk_elf,
		(CONST CHAR8 *)VIRTUAL_BASE_SYMBOL, FALSE);
	#undef VIRTUAL_BASE_SYMBOL

	CONST UINT64 havk_size // Will almost never be aligned to 2-MiB.
		= HUGE_PAGE_ALIGN(symbol_address(havk_elf,
		(CONST CHAR8 *)KERNEL_SIZE_SYMBOL, FALSE)) + HUGE_PAGE_SIZE;
	#undef KERNEL_SIZE_SYMBOl

	// The symbol value is zero if it's not found; however, that's
	// acceptable. Check the size later.
	if (havk_virtual_base_check
		&& havk_virtual_base_check != havk_virtual_base)
	{
		Print(u"VIRTUAL BASE IS AMBIGIOUS (%llX)\r\n",
			havk_virtual_base_check);
	}

	// Zero the area first to avoid any false mappings.
	ZeroMem((VOID *)structure, sizeof(struct uefi_page_structure));

	// I've given up trying to map every important thing one by one, so
	// I've just mapped 0 GiB to 4 GiB entirely. OVMF does this anyway, but
	// adds special pages (essentially restrictions) for certain ranges.
	// TODO: I believe it is possible that UEFI loader data may be above
	// 4 GiB, as I've seen some UEFI BIOS settings that allow MMIO above
	// 4 GiB too, but I'm not sure.
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

// Enables some necessary CPU features and switches to the new page structure.
VOID switch_page_layout(struct uefi_page_structure *CONST page_structure)
{
	// For some reason, on some (even recent) machines, the ability for NX
	// page mappings is not enabled by default, even when it has been
	// supported on many various x86-64 CPUs since 2003. QEMU, Bochs, and
	// VirtualBox all have it enabled as a part of the initial CPU state,
	// but for example, my 2012 laptop (ThinkPad T430 i5-3320M) doesn't.
	// I think it's guaranteed to be included in the AMD64 instruction set.
	// I'll quickly enable it here instead of in the kernel, since we
	// manage virtual memory in the bootloader too.
	__asm__ VOLATILE
	(
		".INTEL_SYNTAX noprefix;"
		"MOV ECX, 0xC0000080;" // Move the EFER's MSR index to ECX.
		"RDMSR;" // The MSR's value is now in EDX:EAX (high:low).
		"OR EAX, 0x800;" // Set bit 11 (2048) to enable NXE.
		"WRMSR;" // Write EDX:EAX to the EFER.
		"MOV CR3, %0;" // Load the temporary page structure.
		".ATT_SYNTAX"
		:
		: "r" (page_structure)
		: "eax", "ecx", "edx", "cc", "memory"
	);
}

// For reasons unknown, GNU-EFI's `strncmpa()` (`strncmp()` for ASCII strings)
// returns an unsigned integer and thus is not an accurate implementation of
// `strncmp()` or the similar `strcmp()`. Casting the `strncmpa()` result to
// "INTN" seems to work depending on how the compiler feels during compilation,
// so I've replaced it with a normal `strncmp()`.
INTN strncmp(CONST CHAR8 *string_1, CONST CHAR8 *string_2,
	UINTN bytes)
{
	while (bytes-- && *string_1++ == *string_2++);

	return bytes ? *string_1 - *string_2 : 0;
}

// The boot files are mapped upon entry if they're in the range that I cover
// when I'm building my own paging structure. The kernel will map them
// according to the memory map if they're not mapped anyway.
VOID load_boot_files(CONST EFI_FILE_PROTOCOL *CONST root_directory,
	struct bootloader_arguments *CONST arguments)
{
	// If the key is smaller than or equal to the boot file prefix
	// indicator I use, then it's not a boot_file or the boot file name
	// itself is not specified.
	for (UINTN i = 0; i < arguments->options_count; ++i)
	{
		CHAR8 *CONST key = &arguments->options_string
			[arguments->options[i].key_start];
		CONST UINTN key_length = arguments->options[i].key_end
			- arguments->options[i].key_start;
		CHAR8 *CONST value = &arguments->options_string
			[arguments->options[i].value_start];
		CONST UINTN value_length = arguments->options[i].value_end
			- arguments->options[i].value_start;

		// If it's the key we're looking for, then it has to longer
		// than the "base key".
		if (key_length <= ARRAY_LENGTH(BOOT_FILE_KEY))
		{
			continue;
		}

		// Only needs to match the first part of the key, even if the
		// whole key is longer. The "sub-key" at minimum is at least a
		// single character.
		if (strncmp(key, (CONST CHAR8 *CONST) BOOT_FILE_KEY,
			ARRAY_LENGTH(BOOT_FILE_KEY)))
		{
			continue;
		}

		// Change the end of the value string to a null terminator so
		// we can pass it around more easily. Need to change it back
		// when we're done.
		CHAR8 old_end = value[value_length + 1];
		value[value_length + 1] = '\0';

		EFI_FILE_PROTOCOL *CONST boot_file = open_file(root_directory,
			value);

		if (!boot_file)
		{
			CRITICAL_FAILURE(u"FAILED TO OPEN BOOT FILE: "
				"\"%a\"\r\n", value);
		}

		CONST UINT64 boot_file_size = LibFileInfo(boot_file)->FileSize;

		Print(u"BOOT FILE OPENED AT \"%a\"\r\n"
			"BOOT FILE SIZE: %llu KIBIBYTES\r\n",
			value,
			boot_file_size / 1024);

		value[value_length + 1] = old_end; // Restore the end.

		UEFI(boot_file->SetPosition, 2,
			boot_file,
			0);

		// The ELF itself is not loaded. I'm just reusing this
		// structure as a pointer holder for future expansion.
		struct elf64_file_header *CONST boot_file_area
			= malloc(boot_file_size, EfiLoaderData);

		UEFI(boot_file->Read, 3,
			boot_file,
			&boot_file_size,
			boot_file_area);

		// The data field will carry the address and size for the
		// loadable files.
		arguments->options[i].data.boot_file_data.boot_file_address
			= boot_file_area;
		arguments->options[i].data.boot_file_data.boot_file_size
			= boot_file_size;

		// Do the same trick as I did for the value string.
		old_end = key[key_length + 1];
		key[key_length + 1] = '\0';
		Print(u"BOOT FILE SPECIFIED BY \"%a\" LOADED AT: 0x%llX\r\n",
			key, boot_file_area);
		key[key_length + 1] = old_end;

		UEFI(boot_file->Close, 1,
			boot_file);
	}
}

// Similar logic to `load_boot_files()`, but specialised for the kernel ELF.
// Same comments and notes mostly apply.
EFI_FILE_PROTOCOL *open_havk(CONST EFI_FILE_PROTOCOL *CONST root_directory,
	CONST struct bootloader_arguments *CONST arguments)
{
	EFI_FILE_PROTOCOL *open_attempt = NULL;

	for (UINT16 i = 0; i < arguments->options_count; ++i)
	{
		CHAR8 *CONST key = &arguments->options_string
			[arguments->options[i].key_start];
		CONST UINTN key_length = arguments->options[i].key_end
			- arguments->options[i].key_start;
		CHAR8 *CONST value = &arguments->options_string
			[arguments->options[i].value_start];
		CONST UINTN value_length = arguments->options[i].value_end
			- arguments->options[i].value_start;

		// Must match "HAVK" fully.
		if (strncmp(key, (CONST CHAR8 *CONST) HAVK_KEY,
			key_length < ARRAY_LENGTH(HAVK_KEY)
			? key_length : ARRAY_LENGTH(HAVK_KEY)))
		{
			continue;
		}

		CONST CHAR8 old_end = value[value_length + 1];
		value[value_length + 1] = '\0';

		open_attempt = open_file(root_directory, value);
		Print(u"HAVK OPENED AT \"%a\"\r\n"
			"HAVK KERNEL FILE SIZE: %llu KIBIBYTES\r\n", value,
			LibFileInfo(open_attempt)->FileSize / 1024);

		value[value_length + 1] = old_end;

		// Only load the first kernel ELF file specified in case it
		// appears more than once.
		break;
	}

	if (!open_attempt)
	{
		CRITICAL_FAILURE(u"HAVK WAS NOT SPECIFIED IN THE BOOT "
			"CONFIGURATION FILE\r\n");
	}

	return open_attempt;
}

EFIAPI
EFI_STATUS efi_main(EFI_HANDLE image_handle, EFI_SYSTEM_TABLE *system_table)
{
	struct elf64_file havk_elf = {0};
	struct bootloader_arguments *arguments = NULL;
	struct uefi_page_structure *page_structure = NULL;
	EFI_FILE_PROTOCOL *root_directory = NULL;

	welcome(image_handle, system_table);

	#if HAVK_GDB_DEBUG
		setup_debug_session();
	#endif

	// TODO: For now, just find the device where the bootloader is
	// also located. This should save a lot of time guessing, but I aim
	// to revise this in the future for more user options.
	/* open_havk(get_sfs_handle()); */
	// Open the root directory of the ESP.
	root_directory = open_root_directory(image->DeviceHandle);

	// Parse the configuration file, which contains the location of the
	// kernel ELF and other boot files.
	arguments = malloc(sizeof(arguments), EfiLoaderData);
	parse_configuration(root_directory, arguments);

	// Let the user set the screen resolution. I just find the first GOP
	// handle.
	// TODO: Is it possible to use multiple GOP handles for multiple
	// monitor setups?
	set_screen_resolution(read_screen_resolutions(get_gop_handle()));

	// Open the kernel's ELF file.
	havk = open_havk(root_directory, arguments);

	// Process the ELF headers.
	read_file_header(&havk_elf);
	read_program_headers(&havk_elf);
	read_section_headers(&havk_elf);
	read_symbols(&havk_elf);

	// Load HAVK to memory so it's ready to be entered.
	load_havk(&havk_elf);

	// Store framebuffer information in the argument's structure.
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

	// Load the initialisation tasks or "boot files" into memory.
	load_boot_files(root_directory, arguments);

	// I want to pass the memory map etc. to HAVK, so instead of using
	// inline assembly, I can just tell GCC to use the System V ABI way
	// of passing arguments to our assembly entry function which is
	// using the ELF format and adheres to the System V ABI.
	__attribute__((sysv_abi))
	VOID (*enter_havk) (struct bootloader_arguments *arguments,
		UINT64 magic) = (VOID *)havk_elf.file_header.entry_address;

	// We're done reading all the files we need.
	UEFI(havk->Close, 1,
		havk);
	UEFI(root_directory->Close, 1,
		root_directory);

	// Deallocate the ELF headers and symbol-related structures, as we do
	// not need them anymore. Note that they must be allocated by my
	// `malloc()` wrapper, as the page size is stored behind/below the
	// returned base address.
	free(havk_elf.program_headers);
	free(havk_elf.section_headers);
	free(havk_elf.symbols);
	free(havk_elf.symbol_string_table);

	Print(u"EXITING UEFI BOOT SERVICES\r\n");

	get_memory_map(arguments, image_handle);
	// UEFI boot services have ended. Can't e.g. `Print()` anymore.

	switch_page_layout(page_structure);

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
