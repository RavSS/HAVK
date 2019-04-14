WITH System;
WITH Interfaces;

USE Interfaces;

PACKAGE HAVK_Kernel.UEFI
IS
	TYPE UEFI_Arguments IS RECORD
		Graphics_Mode_Current : unsigned_32;
		Graphics_Mode_Max : unsigned_32;
		Framebuffer_Address : System.address;
		Framebuffer_Size : unsigned_64;
		Horizontal_Resolution : unsigned_32;
		Vertical_Resolution : unsigned_32;
		Pixels_Per_Scanline : unsigned_32;

		-- These two below aren't properly "defined".
		Pixel_Format : unsigned_32;
		Pixel_Bitmask : unsigned_32;

		-- Memory map needs to be defined in a separate record.
		Memory_Map_Address : System.address;

		Memory_Map_Key : unsigned_64;
		Memory_Map_Size : unsigned_64;
		Memory_Map_Descriptor_Size : unsigned_64;
		Memory_Map_Descriptor_Version : unsigned_32;
	END RECORD WITH Convention => C;

	-- A pointer is passed by the UEFI bootloader.
	TYPE Access_UEFI_Arguments IS ACCESS UEFI_Arguments;
END HAVK_Kernel.UEFI;
