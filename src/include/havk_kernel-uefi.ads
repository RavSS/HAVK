WITH System;
WITH HAVK_Kernel;

PACKAGE HAVK_Kernel.UEFI
IS
   -- See the UEFI specifications for more about the types of pixel
   -- formats and bitmasks.
   TYPE Pixel_Formats IS(
      RGB,     -- PixelRedGreenBlueReserved8BitPerColor.
      BGR,     -- PixelBlueGreenRedReserved8BitPerColor.
      Bitmask, -- PixelBitMask.
      BLT,     -- PixelBltOnly (no framebuffer access = failure).
      Max);    -- PixelFormatMax (should never get this from boot).

   TYPE Pixel_Bitmasks IS RECORD
      Red : u32;
      Green : u32;
      Blue : u32;
      Reserved : u32;
   END RECORD WITH Convention => C;

   TYPE UEFI_Arguments IS RECORD
      Graphics_Mode_Current : u32;
      Graphics_Mode_Max : u32;
      Framebuffer_Address : System.Address;
      Framebuffer_Size : u64;
      Horizontal_Resolution : u32;
      Vertical_Resolution : u32;
      Pixels_Per_Scanline : u32;
      Pixel_Format : Pixel_Formats;
      Pixel_Bitmask : Pixel_Bitmasks;

      -- TODO: Memory map needs to be defined in a separate record.
      Memory_Map_Address : System.Address;

      Memory_Map_Key : u64;
      Memory_Map_Size : u64;
      Memory_Map_Descriptor_Size : u64;
      Memory_Map_Descriptor_Version : u32;
   END RECORD WITH Convention => C;

   -- A pointer is passed by the UEFI bootloader.
   TYPE Access_UEFI_Arguments IS ACCESS UEFI_Arguments;
END HAVK_Kernel.UEFI;
