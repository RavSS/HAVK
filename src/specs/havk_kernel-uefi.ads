WITH
   System,
   HAVK_Kernel;

PACKAGE HAVK_Kernel.UEFI
IS
   -- See the UEFI specifications for more about the types of pixel
   -- formats and bitmasks.
   TYPE pixel_formats IS(
      RGB,     -- PixelRedGreenBlueReserved8BitPerColor.
      BGR,     -- PixelBlueGreenRedReserved8BitPerColor.
      Bitmask, -- PixelBitMask.
      BLT,     -- PixelBltOnly (no framebuffer access = failure).
      Max)     -- PixelFormatMax (should never get this from boot).
   WITH Convention => C;

   TYPE pixel_bitmasks IS RECORD
      Red        : num RANGE 0 .. 16#FFFFFFFF#;
      Green      : num RANGE 0 .. 16#FFFFFFFF#;
      Blue       : num RANGE 0 .. 16#FFFFFFFF#;
      Reserved   : num RANGE 0 .. 16#FFFFFFFF#;
   END RECORD WITH Convention => C;
   FOR pixel_bitmasks'size USE 32 * 4;
   FOR pixel_bitmasks      USE RECORD
      Red        AT  0 RANGE 0 .. 31;
      Green      AT  4 RANGE 0 .. 31;
      BLUE       AT  8 RANGE 0 .. 31;
      Reserved   AT 12 RANGE 0 .. 31;
   END RECORD;

   TYPE UEFI_arguments IS RECORD
      Graphics_Mode_Current         : num RANGE 0 .. 16#FFFFFFFF#;
      Graphics_Mode_Max             : num RANGE 0 .. 16#FFFFFFFF#;
      Framebuffer_Address           : System.Address;
      Framebuffer_Size              : num;
      Horizontal_Resolution         : num RANGE 0 .. 16#FFFFFFFF#;
      Vertical_Resolution           : num RANGE 0 .. 16#FFFFFFFF#;
      Pixels_Per_Scanline           : num RANGE 0 .. 16#FFFFFFFF#;
      Pixel_Format                  : pixel_formats;
      Pixel_Bitmask                 : pixel_bitmasks;

      -- TODO: Memory map needs to be defined in a separate record.
      Memory_Map_Address            : System.Address;

      Memory_Map_Key                : num;
      Memory_Map_Size               : num;
      Memory_Map_Descriptor_Size    : num;
      Memory_Map_Descriptor_Version : num;
   END RECORD                         WITH Convention => C;
   FOR UEFI_arguments                 USE RECORD
      Graphics_Mode_Current           AT  0 RANGE 0 .. 31;
      Graphics_Mode_Max               AT  4 RANGE 0 .. 31;
      Framebuffer_Address             AT  8 RANGE 0 .. 63;
      Framebuffer_Size                AT 16 RANGE 0 .. 63;
      Horizontal_Resolution           AT 24 RANGE 0 .. 31;
      Vertical_Resolution             AT 28 RANGE 0 .. 31;
      Pixels_Per_Scanline             AT 32 RANGE 0 .. 31;
      Pixel_Format                    AT 36 RANGE 0 .. 31;
      Memory_Map_Address              AT 40 RANGE 0 .. 63;
      Memory_Map_Key                  AT 48 RANGE 0 .. 63;
      Memory_Map_Size                 AT 56 RANGE 0 .. 63;
      Memory_Map_Descriptor_Size      AT 64 RANGE 0 .. 63;
      Memory_Map_Descriptor_Version   AT 72 RANGE 0 .. 63;
   END RECORD;

   -- A pointer is passed by the UEFI bootloader.
   TYPE access_UEFI_arguments IS ACCESS CONSTANT UEFI_arguments;
END HAVK_Kernel.UEFI;
