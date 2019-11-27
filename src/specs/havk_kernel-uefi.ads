-- This package contains information about UEFI, which is used to interpret
-- the arguments passed to the kernel by HAVK's bootloader. Make sure that
-- this package specification accurately reflects the structures passed by
-- the bootloader itself.
-- TODO: The UEFI specification (2.8) has oddities in it surrounding the memory
-- map, where it doesn't match up with the firmware or it contradicts itself.
PACKAGE HAVK_Kernel.UEFI
IS
   -- See the UEFI specifications for more about the types of pixel
   -- formats and bitmasks.
   TYPE pixel_formats IS(
      RGB,     -- PixelRedGreenBlueReserved8BitPerColor.
      BGR,     -- PixelBlueGreenRedReserved8BitPerColor.
      bitmask, -- PixelBitMask.
      BLT,     -- PixelBltOnly (no framebuffer access = failure).
      max)     -- PixelFormatMax (should never get this from boot).
   WITH
      Size       => 32,
      Convention =>  C;

   TYPE pixel_bitmasks IS RECORD
      Red      : num RANGE 0 .. 16#FFFFFFFF#;
      Green    : num RANGE 0 .. 16#FFFFFFFF#;
      Blue     : num RANGE 0 .. 16#FFFFFFFF#;
      Reserved : num RANGE 0 .. 16#FFFFFFFF#;
   END RECORD
   WITH
      Convention => C_Pass_By_Copy;
   FOR  pixel_bitmasks USE RECORD
      Red      AT  0 RANGE 0 .. 31;
      Green    AT  4 RANGE 0 .. 31;
      Blue     AT  8 RANGE 0 .. 31;
      Reserved AT 12 RANGE 0 .. 31;
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
   FOR memory_attributes USE(
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
      crypto_protected         => 16#80000#);

   -- TODO: Is there any way to jam this up into the type above?
   -- UEFI runtime services memory region. This is here because it's too large
   -- to fit inside the enumeration type, as apparently it goes over the range
   -- of the "System.Max_Binary_Modulus" type. Lovely.
   memory_attribute_runtime : CONSTANT num := 16#8000000000000000#;

   -- Describes what type of memory a region/descriptor is. The current UEFI
   -- specification says to "never" use an enumerated type in a structure,
   -- which is understandable as the size isn't defined too well, yet
   -- they have one directly inside the memory descriptor structure. ???
   -- READ: UEFI Specification, Version 2.8 - Page 21.
   TYPE memory_type IS(
      reserved_data,      -- Unusable memory. It cannot be used by HAVK.
      loader_code,        -- Usable memory. Previously held UEFI loader code.
      loader_data,        -- Usable memory. Previously held UEFI loader data.
      boot_code,          -- Usable memory. Previously held UEFI boot code.
      boot_data,          -- Usable memory. Previously held UEFI boot data.
      runtime_code,       -- Unusable memory. Holds code for the UEFI runtime.
      runtime_data,       -- Unusable memory. Holds data for the UEFI runtime.
      conventional_data,  -- Usable memory. No special purpose, only general.
      unusable_data,      -- Unusable memory. Region marked as erroneous.
      ACPI_table_data,    -- Partially unusable memory until ACPI is enabled.
      ACPI_firmware_data, -- Unusable memory. Contains ACPI firmware data.
      MMIO_data,          -- Unusable memory. Mapped to system IO.
      MMIO_port_data,     -- Unusable memory. Mapped to system IO port space.
      processor_code,     -- Unusable memory. Contains vital CPU firmware.
      persistent_data)    -- Usable memory. Essentially conventional memory.
   WITH
      Size       => 32,
      Convention =>  C;

   -- These make up the memory map. Each one explains what a region of memory
   -- is, what its attributes are, and how big it is.
   TYPE memory_descriptor IS RECORD
      Memory_Region_Type            : memory_type;
      -- See the representation clause for information about this component.
      Padding_1                     : num RANGE 0 .. 16#FFFFFFFF#;
      -- The physical address of where the region starts.
      Start_Address_Physical        : num;
      -- The virtual address of where the region starts. This is essentially
      -- useless for the operating system and is truly for UEFI applications.
      Start_Address_Virtual         : num;
      -- Each descriptor has at least one page. Each one is 4 KiB.
      -- The maximum number of pages cannot represent memory over the limit
      -- of 0xFFFFFFFFFFFFF000. If this is zero, then there's a massive error.
      Number_Of_Pages               : num RANGE 1 .. num'last;
      -- Attributes of the memory region that are OR'd together to
      -- create a bitmask.
      Memory_Attribute_Bitmask      : num;
      -- See the representation clause for information about this component.
      Padding_2                     : num;
   END RECORD
   WITH
      Convention => C_Pass_By_Copy;
   FOR memory_descriptor USE RECORD
      Memory_Region_Type            AT  0 RANGE 0 .. 31;
      -- There's 32 bits of padding right here. This is foolishly unexplained
      -- in the UEFI specification (2.8), but it's for architectural purposes.
      -- You can check it out via debugging the UEFI application with GDB.
      Padding_1                     AT  4 RANGE 0 .. 31;
      Start_Address_Physical        AT  8 RANGE 0 .. 63;
      Start_Address_Virtual         AT 16 RANGE 0 .. 63;
      Number_Of_Pages               AT 24 RANGE 0 .. 63;
      Memory_Attribute_Bitmask      AT 32 RANGE 0 .. 63;
      -- Yet another unexplained padding variable, and this one does not even
      -- show up in the structure. The `sizeof()` for the memory descriptor is
      -- 40, but there's 8 bytes of padding between the descriptors, so UEFI
      -- says it is actually 48 bytes. The reason why this exists is so there
      -- is room for future entries. Why not make a new descriptor version?
      -- READ: https://narkive.com/BMqVNNak
      Padding_2                     AT 40 RANGE 0 .. 63;
   END RECORD;

   TYPE arguments IS RECORD
      -- Graphics related variables.
      Graphics_Mode_Current         : num RANGE 0 .. 16#FFFFFFFF#;
      Graphics_Mode_Max             : num RANGE 0 .. 16#FFFFFFFF#;
      Framebuffer_Address           : num RANGE 1 .. num'last;
      Framebuffer_Size              : num RANGE 4 .. num'last; -- 32-bit pixel.
      Horizontal_Resolution         : num RANGE 1 .. 16#FFFFFFFF#;
      Vertical_Resolution           : num RANGE 1 .. 16#FFFFFFFF#;
      Pixels_Per_Scanline           : num RANGE 1 .. 16#FFFFFFFF#;
      Pixel_Format                  : pixel_formats RANGE RGB .. bitmask;
      Pixel_Bitmask                 : pixel_bitmasks;
      -- Memory related variables.
      Memory_Map_Address            : num RANGE 1 .. num'last;
      Memory_Map_Key                : num RANGE 1 .. num'last;
      Memory_Map_Size               : num RANGE 1 .. num'last;
      Memory_Map_Descriptor_Size    : num RANGE 1 .. num'last;
      Memory_Map_Descriptor_Version : num RANGE 0 .. 16#FFFFFFFF#;
   END RECORD
   WITH
      Convention => C_Pass_By_Copy;
   FOR arguments USE RECORD
      Graphics_Mode_Current         AT  0 RANGE 0 ..  31;
      Graphics_Mode_Max             AT  4 RANGE 0 ..  31;
      Framebuffer_Address           AT  8 RANGE 0 ..  63;
      Framebuffer_Size              AT 16 RANGE 0 ..  63;
      Horizontal_Resolution         AT 24 RANGE 0 ..  31;
      Vertical_Resolution           AT 28 RANGE 0 ..  31;
      Pixels_Per_Scanline           AT 32 RANGE 0 ..  31;
      Pixel_Format                  AT 36 RANGE 0 ..  31;
      Pixel_Bitmask                 AT 40 RANGE 0 .. 127;
      Memory_Map_Address            AT 56 RANGE 0 ..  63;
      Memory_Map_Key                AT 64 RANGE 0 ..  63;
      Memory_Map_Size               AT 72 RANGE 0 ..  63;
      Memory_Map_Descriptor_Size    AT 80 RANGE 0 ..  63;
      Memory_Map_Descriptor_Version AT 88 RANGE 0 ..  31;
   END RECORD;

   -- An array of memory descriptors - a memory map.
   TYPE memory_map IS ARRAY(num RANGE <>) OF ALIASED memory_descriptor
   WITH
      Convention     =>   C,
      Component_Size => 384;
END HAVK_Kernel.UEFI;
