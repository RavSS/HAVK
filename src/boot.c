// Read the UEFI specifications first: https://uefi.org/specifications
// The purpose of this file is to boot HAVK. It utilizes GNU EFI.
// If you're reading this to figure out how to create a UEFI application
// that loads an ELF file to memory, then create a random freestanding
// program via e.g. C and change the "HAVK_LOCATION" to point towards your
// program. There is no guarantee it will work, as I may have parsed it wrong.
#include <efi.h>
#include <efilib.h>

// TODO: The entire bootloader needs to be refactored into multiple functions.

// This should be defined and controlled by the Makefile.
#ifndef HAVK_VERSION
	#define HAVK_VERSION L"\?\?-\?\?-\?\?" // Avoid the silly C trigraphs.
#endif

// If the Makefile forgets to define the location of HAVK's ELF file
// when passing it to GCC, then define it to a default location.
#ifndef HAVK_LOCATION
	#define HAVK_LOCATION L"\\HAVK\\HAVK.elf"
#endif

// HAVK is going to expect itself at a higher address, even when it will be
// placed at e.g. 0x100000. See the x86 BIOS C version of HAVK for something
// similiar, which has itself placed at virtual address 0xC0100000, but is
// actually at physical address 0x100000 or something close to it.
#ifndef HAVK_VIRTUAL_MEMORY_BASE
	#define HAVK_VIRTUAL_MEMORY_BASE 0x0
#endif

#define CRITICAL_FAILURE() do\
{\
	Print(L"FAILED TO BOOT HAVK\r\n");\
	Print(L"PRESS ANY KEY TO HARD RESTART...\r\n");\
	Pause();\
	uefi_call_wrapper(RT->ResetSystem, 4\
		EfiResetCold\
		EFI_ERROR((x)),\
		0,\
		NULL);\
}\
while (0)

// Checks the EFI return status for an error and prints debugging messages.
#define ERROR_CHECK(x) do\
{\
	if ((x) != EFI_SUCCESS || EFI_ERROR((x)))\
	{\
		Print(L"UEFI ERROR: %d - \"%r\" (LINE %d)\r\n",\
			(x), (x), __LINE__);\
		CRITICAL_FAILURE();\
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

#define BUF_SIZE (sizeof(CHAR16) * 32)

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

// This gets passed to HAVK's entry function later on. I've just made it
// into a structure for the sake of less parameters needing to be passed.
struct uefi_arguments
{
	UINT32 graphics_mode_current;
	UINT32 graphics_mode_max;
	EFI_PHYSICAL_ADDRESS framebuffer_address;
	UINTN framebuffer_size;
	UINT32 horizontal_resolution;
	UINT32 vertical_resolution;
	UINT32 pixels_per_scanline;
	EFI_GRAPHICS_PIXEL_FORMAT pixel_format;
	EFI_PIXEL_BITMASK pixel_bitmask;
	EFI_MEMORY_DESCRIPTOR *memory_map;
	UINTN memory_map_key;
	UINTN memory_map_size;
	UINTN memory_map_descriptor_size;
	UINT32 memory_map_descriptor_version;
} __attribute__((packed));

EFIAPI
EFI_STATUS efi_main(EFI_HANDLE image_handle, EFI_SYSTEM_TABLE *system_table)
{
	// Set up both "system table" and "boot services" access shortcuts.
	// Note that you will encounter both UEFI function calls with my
	// wrapper or the shortcut functions found in "efilib.h".
	// See all references to "AllocatePool" for the general idea.
	InitializeLib(image_handle, system_table);

	EFI_STATUS ret = EFI_SUCCESS;
	CHAR16 *buf = AllocatePool(BUF_SIZE + 1);

	// Set the colours for the text.
	UEFI(ST->ConOut->SetAttribute, 2,
		ST->ConOut,
		EFI_TEXT_ATTR(EFI_LIGHTRED, EFI_BLACK));

	// Enable the cursor. Helpful for user input.
	UEFI(ST->ConOut->EnableCursor, 2,
		ST->ConOut,
		TRUE);

	// Clear the screen.
	UEFI(ST->ConOut->ClearScreen, 1,
		ST->ConOut);

	// Print the version notification in the centre of the screen.
	PrintAt(40 - 12, 1, L"NOW BOOTING HAVK %s\r\n\r\n", HAVK_VERSION);

	EFI_GUID simple_file_system_guid
		= EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;

	// Prepare to get the correct simple file system handle.
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
		Print(L"UNAVAILABLE TO FIND A SIMPLE FILE SYSTEM HANDLE\r\n");
		CRITICAL_FAILURE();
	}
	else
	{
		Print(L"SIMPLE FILE SYSTEM PROTOCOL HANDLES: %llu\r\n",
			handles_found);
	}

	EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *simple_file_system;
	UINTN handle_selected = 0; // EFI_HANDLE index.

	if (handles_found == 1)
	{
		handle_selected = 0; // Only a single element, so index zero.
	}
	else
	{
		Print(L"ENTER THE HANDLE NUMBER (1 to %llu)", handles_found);
		Input(L": ", buf, BUF_SIZE);
		Print(L"\r\n");
		handle_selected = Atoi(buf) - 1;
	}

	// Use the simple file system protocol to explore the FAT volume.
	UEFI(BS->HandleProtocol, 3,
		handles[handle_selected],
		&simple_file_system_guid,
		&simple_file_system);

	EFI_FILE_PROTOCOL *root_directory;
	EFI_FILE_PROTOCOL *havk;

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

	Print(L"HAVK KERNEL FILE SIZE: %llu BYTES\r\n", havk_info.FileSize);

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

	// Check for the ELF magic numbers. Less confusing to just do
	// straight comparisons instead of using `StrnCmp` or `strncmpa`
	// thanks to the irregular character sizes.
	if (havk_file_header.identity[0] != 0x7F
		|| havk_file_header.identity[1] != 0x45
		|| havk_file_header.identity[2] != 0x4C
		|| havk_file_header.identity[3] != 0x46)
	{
		Print(L"ELF MAGIC NUMBERS ARE MISSING OR CORRUPTED\r\n");
		CRITICAL_FAILURE();
	}

	Print(L"HAVK ENTRY POSITION: 0x%X\r\n",
		havk_file_header.entry_address);

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

		EFI_PHYSICAL_ADDRESS segment_physical_address
			= havk_program_headers[i].physical_address
			- HAVK_VIRTUAL_MEMORY_BASE;

		// Allocate a page to store the segment.
		UEFI(BS->AllocatePages, 4,
			AllocateAddress,
			EfiLoaderCode, // Correct memory type, I think?
			EFI_SIZE_TO_PAGES(havk_program_headers[i].memory_size),
			&segment_physical_address);

		// Zero out the segment's memory space just in case.
		// Really only required for the BSS segment, but do it anyway.
		// `ZeroMem()` exists as a shortcut, but it requires a cast
		// as it doesn't take in a "EFI_PHYSICAL_ADDRESS" type.
		uefi_call_wrapper(BS->SetMem, 3, // Returns nothing.
			segment_physical_address,
			havk_program_headers[i].memory_size,
			0);

		// Go to the offset of the segment in the file.
		UEFI(havk->SetPosition, 2,
			havk,
			havk_program_headers[i].offset);

		// Load the segment.
		UEFI(havk->Read, 3,
			havk,
			&havk_program_headers[i].size,
			segment_physical_address);

		Print(L"HAVK SEGMENT ADDRESS: 0x%X (%llu bytes)\r\n",
			segment_physical_address,
			havk_program_headers[i].memory_size);
	}

	// Close the files, as we already have HAVK in memory at our
	// specified physical address now.
	UEFI(havk->Close, 1,
		havk);

	UEFI(root_directory->Close, 1,
		root_directory);

	// Free the program headers' memory. Since I allocated it without
	// the shortcut function and with my `UEFI()` wrapper, I'll free it
	// with the wrapper.
	UEFI(BS->FreePool, 1,
		havk_program_headers);

	// Now to get the framebuffer. GOP is the new version of UGP, so
	// I guess I will use that instead.
	EFI_GUID graphics_output_protocol_guid
		= EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID;

	// Get the graphics output protocol handle. Get rid of the previously
	// found handles for the simple file system protocol.
	*handles = NULL;
	handles_found = 0;

	// Find handles that support the graphics output protocol.
	UEFI(BS->LocateHandleBuffer, 5,
		ByProtocol,
		&graphics_output_protocol_guid,
		NULL,
		&handles_found,
		&handles);

	if (!handles_found)
	{
		Print(L"UNABLE TO FIND A GRAPHICS OUTPUT PROTOCOL HANDLE\r\n");
		Print(L"FAILED TO BOOT HAVK\r\n");
		Pause();
	}

	Print(L"GRAPHICS OUTPUT PROTOCOL HANDLES: %llu\r\n", handles_found);

	EFI_GRAPHICS_OUTPUT_PROTOCOL *graphics_output_protocol;
	EFI_GRAPHICS_OUTPUT_MODE_INFORMATION *graphics_information;

	UEFI(BS->HandleProtocol, 3,
		handles[0], // TODO: Assume it's the first one.
		&graphics_output_protocol_guid,
		&graphics_output_protocol);

	// The true mode range is 0 to "MaxMode", but I'll display it
	// from the start of 1.
	UINTN max_graphics_mode = graphics_output_protocol->Mode->MaxMode - 1;

	Print(L"POSSIBLE GRAPHICS MODES: 1 to %u\r\n", max_graphics_mode);

	// Display every single resolution possible.
	for (UINTN i = 1; i <= max_graphics_mode; i++)
	{
		EFI_GRAPHICS_OUTPUT_MODE_INFORMATION *queried_mode_information;

		UEFI(graphics_output_protocol->QueryMode, 4,
			graphics_output_protocol,
			i - 1,
			sizeof(queried_mode_information),
			&queried_mode_information);

		// What follows is just pretty printing. Specific spaces are
		// in their hexidecimal representation for easier viewing.

		if (i < 10) Print(L"\x20%llu", i);
		else Print(L"%llu", i);

		SPrint(buf, BUF_SIZE, L": %ux%u",
			queried_mode_information->HorizontalResolution,
			queried_mode_information->VerticalResolution);
		Print(buf);

		// Instead of relying on tabs (which don't work everywhere),
		// format it with spaces.
		UINTN display_length = StrnLen(buf, BUF_SIZE);
		for (UINTN x = display_length; x < 13; x++) Print(L"\x20");

		// Conserve screen space instead of printing each
		// mode and resolution on a separate line. A minimum
		// of 80x25 is guaranteed for console size.
		if (i % 5 == 0 || i == max_graphics_mode) Print(L"\r\n");
	}

	// Default is mode zero, the smallest resolution.
	UINTN selected_mode = 0;

	do // Now handle the inputted graphics mode from the user.
	{
		Print(L"ENTER THE GRAPHICS MODE (1 to %u)", max_graphics_mode);

		Input(L": ", buf, BUF_SIZE);
		Print(L"\r\n");
		selected_mode = Atoi(buf);

		if (selected_mode < 1 || selected_mode > max_graphics_mode)
			Print(L"GRAPHICS MODE IS OUT OF RANGE\r\n");
	}
	while (selected_mode < 1 || selected_mode > max_graphics_mode);

	UEFI(graphics_output_protocol->SetMode, 2,
		graphics_output_protocol,
		selected_mode - 1);

	// Get information for the user's selected mode.
	UEFI(graphics_output_protocol->QueryMode, 4,
		graphics_output_protocol,
		graphics_output_protocol->Mode->Mode,
		sizeof(graphics_information),
		&graphics_information);

	// This will get passed to HAVK later on, because we obviously
	// need to show things on the screen.
	struct uefi_arguments *arguments;

	// Pool memory for the UEFI arguments.
	UEFI(BS->AllocatePool, 3,
		EfiLoaderData,
		sizeof(arguments),
		&arguments);

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
	arguments->pixel_bitmask
		= graphics_information->PixelInformation;

	Print(L"CURRENT GRAPHICS MODE: %u\r\n",
		arguments->graphics_mode_current);

	Print(L"VIDEO RESOLUTION: %ux%u\r\n",
		arguments->horizontal_resolution,
		arguments->vertical_resolution);

	Print(L"VIDEO FRAMEBUFFER RANGE: 0x%X to 0x%X\r\n",
		arguments->framebuffer_address,
		arguments->framebuffer_address +
		arguments->framebuffer_size);

	Print(L"PREPARING TO ENTER HAVK\r\n");
	// From here on out, do not call anything if it is not compulsory.
	// Otherwise it can mess with the memory map and whatnot.

	// TODO: Get ACPI tables etc. before the memory map.

	// Free the input string buffer, as it is not needed now.
	FreePool(buf);

	// Begin to get the memory map, which is needed before we
	// exit UEFI and its boot services.
	arguments->memory_map = NULL;
	arguments->memory_map_key = 0;
	arguments->memory_map_size = 0;
	arguments->memory_map_descriptor_size = 0;
	arguments->memory_map_descriptor_version = 0;

	// TODO: Figure out the memory map's size some proper way.
	// So far, I can't figure out how to do it from looking
	// at the official UEFI specifications themselves. For now, I'm
	// completely (over)estimating the amount I will need.

	// You need memory to store the memory map, so allocating space
	// for the memory map should change the memory map itself?

	// Guess the memory map's size so we can make space for it.
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

	// Finally exit UEFI and its boot services.
	UEFI(BS->ExitBootServices, 2,
		image_handle,
		arguments->memory_map_key);

	// Set the runtime stuff's addresses from physical to virtual.
	// Don't believe this is necessary most of the time, since the
	// everything is identity mapped, but let's do it just in case in the
	// event that physical addresses does not equal virtual addresses.
	UEFI(RT->SetVirtualAddressMap, 4,
		arguments->memory_map_size,
		arguments->memory_map_descriptor_size,
		arguments->memory_map_descriptor_version,
		arguments->memory_map);

	// I want to pass the memory map etc. to HAVK, so instead of using
	// inline assembly, I can just tell GCC to use the System V ABI way
	// of passing arguments to our assembly entry function which is
	// using the ELF format and adheres to the System V ABI.
	__attribute__((sysv_abi))
	void (*enter_havk) (struct uefi_arguments *arguments)
		= (void *) havk_file_header.entry_address;

	// Now we leave at last.
	enter_havk(arguments);

	return EFI_SUCCESS;
}