WITH
   System;

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
   WITH
      Convention => C;

   TYPE pixel_bitmasks IS RECORD
      Red        : num RANGE 0 .. 16#FFFFFFFF#;
      Green      : num RANGE 0 .. 16#FFFFFFFF#;
      Blue       : num RANGE 0 .. 16#FFFFFFFF#;
      Reserved   : num RANGE 0 .. 16#FFFFFFFF#;
   END RECORD
   WITH
      Convention => C;
   FOR pixel_bitmasks USE RECORD
      Red        AT  0 RANGE 0 .. 31;
      Green      AT  4 RANGE 0 .. 31;
      Blue       AT  8 RANGE 0 .. 31;
      Reserved   AT 12 RANGE 0 .. 31;
   END RECORD;

   -- TODO: Maybe make this into a record, what is the "best Ada way" here?
   -- READ: UEFI Specification, Version 2.8 - Page 166.
   -- Multiple attributes are allowed, they can be OR'd together.
   TYPE memory_attributes IS(
      uncacheable,              -- Memory region is not cacheable.
      write_combining,          -- Write combining capable.
      write_through,            -- Cache hits go through to the memory.
      write_back,               -- Cache hits go back before going to memory.
      uncacheable_inexportable, -- Memory region supports fetch-and-add.
      write_protected,          -- Writing memory is not allowed.
      read_protected,           -- Reading memory is not allowed.
      execution_protected,      -- Executing memory is not allowed.
      persistent,               -- Special NVRAM region for UEFI firmware etc.
      high_reliability,         -- Memory region is more reliable than others.
      read_only,                -- Memory region can only be read.
      specific_purpose,         -- Memory region is for a specific purpose.
      crypto_protected);        -- Protected via CPU's cryptographic features.
   FOR memory_attributes USE
   (
      uncacheable              =>     16#1#,
      write_combining          =>     16#2#,
      write_through            =>     16#4#,
      write_back               =>     16#8#,
      uncacheable_inexportable =>    16#10#,
      write_protected          =>  16#1000#,
      read_protected           =>  16#2000#,
      execution_protected      =>  16#4000#,
      persistent               =>  16#8000#,
      high_reliability         => 16#10000#,
      read_only                => 16#20000#,
      specific_purpose         => 16#40000#,
      crypto_protected         => 16#80000#
   );

   -- TODO: Is there any way to jam this up into the type above?
   -- UEFI runtime services memory region. This is here because it's too large
   -- to fit inside the enumeration type, as apparently it goes over the range
   -- of the "System.Max_Binary_Modulus" type. Lovely.
   memory_attribute_runtime         : CONSTANT num  := 16#8000000000000000#;

   TYPE memory_descriptor IS RECORD
      Memory_Type_Bitmask           : num   RANGE 0 .. 16#FFFFFFFF#;
      Start_Address_Physical        : num;
      Start_Address_Virtual         : num;
      Number_Of_Pages               : num   RANGE 1 .. num'last;
      Memory_Attribute_Bitmask      : num;
   END RECORD
   WITH
      Convention => C;
   FOR memory_descriptor USE RECORD
      Memory_Type_Bitmask             AT  0 RANGE 0 .. 63;
      -- TODO: There's 32 bits of padding right here from what GDB tells me,
      -- so I've padded out "Memory_Type_Bitmask" with an extra 32 bits while
      -- restricting its range to half of its true capacity. The UEFI firmware
      -- seems to lie, descriptors are truly 40 bytes, they are not 48 bytes.
      -- No idea what is going on, but this works.
      Start_Address_Physical          AT  8 RANGE 0 .. 63;
      Start_Address_Virtual           AT 16 RANGE 0 .. 63;
      Number_Of_Pages                 AT 24 RANGE 0 .. 63;
      Memory_Attribute_Bitmask        AT 32 RANGE 0 .. 63;
   END RECORD;

   TYPE memory_map       IS ARRAY(num RANGE <>) OF ALIASED memory_descriptor
   WITH
      Convention     => C,
      Component_Size => 320;

   TYPE arguments IS RECORD
      -- Graphics related variables.
      Graphics_Mode_Current         : num   RANGE 0 .. 16#FFFFFFFF#;
      Graphics_Mode_Max             : num   RANGE 0 .. 16#FFFFFFFF#;
      Framebuffer_Address           : System.Address;
      Framebuffer_Size              : num;
      Horizontal_Resolution         : num   RANGE 0 .. 16#FFFFFFFF#;
      Vertical_Resolution           : num   RANGE 0 .. 16#FFFFFFFF#;
      Pixels_Per_Scanline           : num   RANGE 0 .. 16#FFFFFFFF#;
      Pixel_Format                  : pixel_formats;
      Pixel_Bitmask                 : pixel_bitmasks;
      -- Memory related variables.
      Memory_Map_Address            : System.Address;
      Memory_Map_Key                : num;
      Memory_Map_Size               : num;
      Memory_Map_Descriptor_Size    : num;
      Memory_Map_Descriptor_Version : num   RANGE 0 .. 16#FFFFFFFF#;
   END RECORD
   WITH
      Convention => C;
   FOR arguments USE RECORD
      Graphics_Mode_Current           AT  0 RANGE 0 ..  31;
      Graphics_Mode_Max               AT  4 RANGE 0 ..  31;
      Framebuffer_Address             AT  8 RANGE 0 ..  63;
      Framebuffer_Size                AT 16 RANGE 0 ..  63;
      Horizontal_Resolution           AT 24 RANGE 0 ..  31;
      Vertical_Resolution             AT 28 RANGE 0 ..  31;
      Pixels_Per_Scanline             AT 32 RANGE 0 ..  31;
      Pixel_Format                    AT 36 RANGE 0 ..  31;
      Pixel_Bitmask                   AT 40 RANGE 0 .. 127;
      Memory_Map_Address              AT 56 RANGE 0 ..  63;
      Memory_Map_Key                  AT 64 RANGE 0 ..  63;
      Memory_Map_Size                 AT 72 RANGE 0 ..  63;
      Memory_Map_Descriptor_Size      AT 80 RANGE 0 ..  63;
      Memory_Map_Descriptor_Version   AT 88 RANGE 0 ..  31;
   END RECORD;

   -- A pointer is passed by the UEFI bootloader.
   TYPE access_arguments IS NOT NULL ACCESS CONSTANT arguments;
END HAVK_Kernel.UEFI;
