// Read the UEFI specifications first: https://uefi.org/specifications
// The purpose of this file is to boot HAVK. It utilizes GNU EFI.
// If you're reading this to figure out how to create a UEFI application
// that loads an ELF file to memory, then create a random freestanding
// program via e.g. C and change the "HAVK_LOCATION" to point towards your
// program. There is no guarantee it will work, as I may have parsed it wrong.
#include <efi.h>
#include <efilib.h>

// If the Makefile forgets to define the location of HAVK's ELF file
// when passing it to GCC, then define it to a default location.
#ifndef HAVK_LOCATION
	#define HAVK_LOCATION L"\\HAVK\\HAVK.elf"
#endif

// Checks the EFI return status for an error and prints debugging messages.
#define ERROR_CHECK(x) do\
{\
	if ((x) != EFI_SUCCESS || EFI_ERROR((x)))\
	{\
		Print(L"UEFI ERROR: %d - \"%r\" (LINE %d)\r\n",\
			(x), (x), __LINE__);\
		Print(L"FAILED TO BOOT HAVK\r\n");\
		Pause();\
	}\
}\
while (0)

// A wrapper for the wrapper due to laziness.
#define UEFI(...) do\
{\
	ret = uefi_call_wrapper(__VA_ARGS__);\
	ERROR_CHECK(ret);\
}\
while (0)

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
// Do I need to pack it? Do I need bit-fields? The types sizes seem to fit it.

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

// The section header structure goes unused, but I've kept it anyway,
// in case I need it later on.
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

EFIAPI
EFI_STATUS efi_main(EFI_HANDLE image_handle, EFI_SYSTEM_TABLE *system_table)
{
	// Set up both "system table" and "boot services" access shortcuts.
	InitializeLib(image_handle, system_table);
	EFI_STATUS ret = EFI_SUCCESS;

	// Clear the screen.
	UEFI(ST->ConOut->ClearScreen, 1,
		ST->ConOut);

	Print(L"NOW BOOTING HAVK\r\n");

	EFI_GUID simple_file_system_guid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;

	// Get the simple file system handle.
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
		Print(L"UNAVAILABLE TO FIND A HANDLE\r\n");
		Print(L"FAILED TO BOOT HAVK\r\n");
		Pause();
	}

	Print(L"SIMPLE FILE SYSTEM PROTOCOL HANDLES: %d\r\n",
		handles_found);

	EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *simple_file_system;

	// Use the simple file system protocol to explore the FAT32 volume.
	UEFI(BS->HandleProtocol, 3,
		handles[0], // TODO: Assume it's the first one, but fix this.
		&simple_file_system_guid,
		&simple_file_system);

	EFI_FILE_PROTOCOL *root_dir;
	EFI_FILE_PROTOCOL *havk;

	// Open the root directory of the volume.
	UEFI(simple_file_system->OpenVolume, 2,
		simple_file_system,
		&root_dir);

	// Open HAVK's ELF file in read only mode.
	UEFI(root_dir->Open, 5,
		root_dir,
		&havk,
		HAVK_LOCATION,
		EFI_FILE_MODE_READ,
		EFI_FILE_READ_ONLY);

	Print(L"HAVK OPENED AT \"%s\"\r\n", HAVK_LOCATION);

	EFI_GUID file_info_guid = EFI_FILE_INFO_ID;
	EFI_STATUS attempt = EFI_SUCCESS;
	EFI_FILE_INFO havk_info;
	UINT64 havk_info_size = 0;

	// Sometimes the buffer is too small and the function increases
	// it for next time, so it must be attempted again.
	do attempt = uefi_call_wrapper(havk->GetInfo, 4,
		havk,
		&file_info_guid,
		&havk_info_size,
		&havk_info);
	while (attempt == EFI_BUFFER_TOO_SMALL);

	// If the `GetInfo()` function truly failed, then raise the error.
	ERROR_CHECK(attempt);

	Print(L"HAVK KERNEL SIZE: %llu BYTES\r\n", havk_info.FileSize);

	// Reset the HAVK file's position, as we just read HAVK's size.
	UEFI(havk->SetPosition, 2,
		havk,
		0);

	// Load HAVK's ELF file header.
	struct elf64_file_header havk_file_header;
	UINTN elf64_file_header_size = sizeof(struct elf64_file_header);

	UEFI(havk->Read, 3,
		havk,
		&elf64_file_header_size,
		&havk_file_header);

	// Check for the ELF magic numbers.
	if (havk_file_header.identity[0] != 0x7F
		|| havk_file_header.identity[1] != 0x45
		|| havk_file_header.identity[2] != 0x4C
		|| havk_file_header.identity[3] != 0x46)
	{
		Print(L"ELF MAGIC NUMBERS ARE MISSING OR CORRUPTED\r\n");
		Print(L"FAILED TO BOOT HAVK\r\n");
		Pause();
	}

	// Now it's time to load HAVK's ELF program headers.
	struct elf64_program_header *havk_program_headers;
	UINTN havk_program_headers_size = havk_file_header.program_header_size
		* havk_file_header.program_header_entries;

	// Set the position to the offset of the program headers.
	UEFI(havk->SetPosition, 2,
		havk,
		havk_file_header.program_header_offset);

	// Allocate pool memory for all of the program headers.
	UEFI(BS->AllocatePool, 3,
		EfiLoaderData,
		havk_program_headers_size,
		&havk_program_headers);

	// Now get all of the program headers.
	UEFI(havk->Read, 3,
		havk,
		&havk_program_headers_size,
		havk_program_headers);

	// Time to load the segments. I'm not too familiar with ELF
	// loading and the System V ABI, so there may be mistakes here.
	for (UINTN i = 0; i < havk_file_header.program_header_entries; i++)
	{
		// If the type is not "LOAD", then skip it.
		if (havk_program_headers[i].type != 1) continue;

		Print(L"HAVK SEGMENT ADDRESS LOADED TO: 0x%X\r\n",
			havk_program_headers[i].physical_address);

		// Allocate a page to store the segment.
		UEFI(BS->AllocatePages, 4,
			AllocateAddress,
			EfiLoaderCode, // Correct memory type, I think?
			EFI_SIZE_TO_PAGES(havk_program_headers[i].memory_size),
			&havk_program_headers[i].physical_address);

		// Go to the offset of the segment in the file.
		UEFI(havk->SetPosition, 2,
			havk,
			havk_program_headers[i].offset);

		// Load the segment.
		UEFI(havk->Read, 3,
			havk,
			&havk_program_headers[i].size,
			havk_program_headers[i].physical_address);
	}

	// Close the files, as we already have HAVK in memory at our
	// specified physical address now.
	UEFI(havk->Close, 1,
		havk);

	UEFI(root_dir->Close, 1,
		root_dir);

	Print(L"HAVK ENTRY POSITION: 0x%X\r\n", havk_file_header.entry_address);
	Print(L"PREPARING TO ENTER HAVK\r\n");

	// TODO: Get ACPI tables etc. before the memory map.

	// Begin to get the memory map, which is needed before we
	// exit UEFI and its boot services.
	EFI_MEMORY_DESCRIPTOR *memory_map = NULL;
	UINTN memory_map_key = 0;
	UINTN memory_map_size = 0;
	UINTN memory_map_descriptor_size = 0;
	UINT32 memory_map_descriptor_version = 0;

	// TODO: Figure out the memory map's size some proper way.
	// So far, I can't figure out how to do it from looking
	// at the official UEFI specifications themselves. For now, I'm
	// completely (over)estimating the amount I will need.

	// You need memory to store the memory map, so allocating space
	// for the memory map should change the memory map itself?

	// Guess the memory map's size so we can make space for it.
	// I'm going to go with 10 KiB.
	memory_map_size = 10240;

	// Allocate pool memory for the memory map.
	UEFI(BS->AllocatePool, 3,
		EfiLoaderData,
		memory_map_size,
		&memory_map);

	// Now get the memory map itself.
	do attempt = uefi_call_wrapper(BS->GetMemoryMap, 5,
		&memory_map_size,
		memory_map,
		&memory_map_key,
		&memory_map_descriptor_size,
		&memory_map_descriptor_version);
	while (attempt == EFI_BUFFER_TOO_SMALL);
	ERROR_CHECK(attempt);

	// Finally exit UEFI and its boot services.
	UEFI(BS->ExitBootServices, 2,
		image_handle,
		memory_map_key);

	// I want to pass the memory map to HAVK, so instead of using
	// inline assembly, I can just tell GCC to use the System V ABI way
	// of passing arguments to our assembly entry function which is
	// using the ELF format and adheres to the System V ABI.
	__attribute__((sysv_abi))
	void (*enter_havk) (UINTN memory_map_size,
		EFI_MEMORY_DESCRIPTOR *memory_map,
		UINTN memory_map_descriptor_size)
		= (void *) havk_file_header.entry_address;

	// Now we leave at least.
	enter_havk(memory_map_size, memory_map, memory_map_descriptor_size);

	return EFI_SUCCESS;
}
