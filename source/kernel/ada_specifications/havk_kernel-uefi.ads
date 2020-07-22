-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-uefi.ads                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package contains information about UEFI, which is used to interpret
-- the arguments passed to the kernel by HAVK's bootloader. Make sure that
-- this package specification accurately reflects the structures passed by
-- the bootloader itself.
PACKAGE HAVK_Kernel.UEFI
WITH
   Preelaborate => true
IS
   -- See the UEFI specifications for more about the types of pixel
   -- formats and bitmasks.
   TYPE pixel_formats IS
     (RGB,     -- PixelRedGreenBlueReserved8BitPerColor.
      BGR,     -- PixelBlueGreenRedReserved8BitPerColor.
      bitmask, -- PixelBitMask.
      BLT,     -- PixelBltOnly (no framebuffer access = failure).
      max)     -- PixelFormatMax (should never get this from boot).
   WITH
      Size       => 32,
      Convention =>  C;

   TYPE pixel_bitmasks IS RECORD
      Red      : number RANGE 0 .. 2**32 - 1;
      Green    : number RANGE 0 .. 2**32 - 1;
      Blue     : number RANGE 0 .. 2**32 - 1;
      Reserved : number RANGE 0 .. 2**32 - 1;
   END RECORD
   WITH
      Convention => C;
   FOR pixel_bitmasks USE RECORD
      Red         AT 00 RANGE 0 .. 31;
      Green       AT 04 RANGE 0 .. 31;
      Blue        AT 08 RANGE 0 .. 31;
      Reserved    AT 12 RANGE 0 .. 31;
   END RECORD;

   -- This record contains all the set attributes of a memory region.
   -- Multiple attributes are allowed, they can be OR'd together.
   -- READ: UEFI Specification, Version 2.8 - Page 166.
   TYPE memory_attributes IS RECORD
      Uncacheable      : boolean; -- Memory region is not cacheable.
      Write_Combining  : boolean; -- Write combining capable.
      Write_Through    : boolean; -- Cache hits go through to the memory.
      Write_Back       : boolean; -- Cache hits go back before going to memory.
      Inexportable     : boolean; -- Memory region supports fetch-and-add.
      Write_Protected  : boolean; -- Writing memory is not allowed.
      Read_Protected   : boolean; -- Reading memory is not allowed.
      Not_Executable   : boolean; -- Executing memory is not allowed.
      Persistent       : boolean; -- Special NVRAM region for any firmware etc.
      High_Reliability : boolean; -- Memory is more reliable than normal.
      Read_Only        : boolean; -- Memory region can only be read.
      Specific_Purpose : boolean; -- Memory region is for a specific purpose.
      Crypto_Protected : boolean; -- Protected via CPU cryptographic features.
      Runtime_Service  : boolean; -- UEFI runtime services memory region.
   END RECORD;

   -- Describes what type of memory a region/descriptor is. The current UEFI
   -- specification says to "never" use an enumerated type in a structure,
   -- which is understandable as the size isn't defined too well, yet
   -- they have one directly inside the memory descriptor structure. ???
   -- READ: UEFI Specification, Version 2.8 - Page 21.
   TYPE memory_type IS
     (reserved_data,      -- Unusable memory. It cannot be used by HAVK.
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
   -- is, what its attributes are, and how big it is. The descriptor's size is
   -- static, but in practice, the descriptor size depends on how the UEFI
   -- firmware has padded it. It may be 40 bytes, but it can be 48 bytes
   -- (usually for EDK2/OVMF) or whatever other byte size. The padding at the
   -- end is unfortunately dynamic. Traversing a linear buffer would be easy to
   -- deal with in C with a for loop, but it requires more work in Ada, so
   -- accesses (that skip over the padding) to the descriptors are required.
   -- READ: https://narkive.com/BMqVNNak
   TYPE memory_descriptor IS RECORD
      -- Describes the type of region the memory descriptor is for.
      Memory_Region_Type       : memory_type;
      -- See the representation clause for information about this component.
      Padding_1                : number RANGE 0 .. 2**32 - 1;
      -- The physical address of where the region starts.
      Start_Address_Physical   : address;
      -- The virtual address of where the region starts. This is essentially
      -- useless for the operating system and is truly for UEFI applications.
      Start_Address_Virtual    : address;
      -- Each descriptor has at least one page. Each one is 4 KiB.
      -- The maximum number of pages cannot represent memory over the limit
      -- of 0xFFFFFFFFFFFFF000. If this is zero, then there's a massive error.
      Number_Of_Pages          : number;
      -- Attributes of the memory region that are OR'd together to
      -- create a bitmask. See the "memory_attributes" record for more.
      Memory_Attribute_Bitmask : number;
   END RECORD
   WITH
      Convention        => C,
      Dynamic_Predicate => Start_Address_Physical MOD 4096 = 0 AND THEN
                           Start_Address_Virtual  MOD 4096 = 0 AND THEN
                           Number_Of_Pages IN 1 .. number(address'last / 4096);
   FOR memory_descriptor USE RECORD
      Memory_Region_Type          AT 00 RANGE 0 .. 31;
      -- There's 32 bits of padding right here. This is foolishly unexplained
      -- in the UEFI specification (2.8), but it's for architectural purposes.
      -- You can check it out via debugging the UEFI application with GDB.
      Padding_1                   AT 04 RANGE 0 .. 31;
      Start_Address_Physical      AT 08 RANGE 0 .. 63;
      Start_Address_Virtual       AT 16 RANGE 0 .. 63;
      Number_Of_Pages             AT 24 RANGE 0 .. 63;
      Memory_Attribute_Bitmask    AT 32 RANGE 0 .. 63;
   END RECORD;

   -- The layout of the arguments structure that my bootloader passes to the
   -- kernel.
   -- TODO: Perhaps make a version number for the structure that gets passed
   -- alongside it.
   TYPE arguments IS LIMITED RECORD
      Graphics_Mode_Current         : number  RANGE 0 .. 2**32 - 1;
      Graphics_Mode_Max             : number  RANGE 0 .. 2**32 - 1;
      Framebuffer_Address           : address RANGE 1 .. address'last;
      Framebuffer_Size              : number  RANGE 4 .. number'last;
      Horizontal_Resolution         : number  RANGE 1 .. 2**32 - 1;
      Vertical_Resolution           : number  RANGE 1 .. 2**32 - 1;
      Pixels_Per_Scanline           : number  RANGE 1 .. 2**32 - 1;
      Pixel_Format                  : pixel_formats RANGE RGB .. bitmask;
      Pixel_Bitmask                 : pixel_bitmasks;
      Memory_Map_Address            : address RANGE 1 .. address'last;
      Memory_Map_Key                : number  RANGE 1 .. number'last;
      Memory_Map_Size               : number  RANGE 1 .. number'last;
      Memory_Map_Descriptor_Size    : number  RANGE 1 .. number'last;
      Memory_Map_Descriptor_Version : number  RANGE 0 .. 2**32 - 1;
      RSDP_Address                  : address RANGE 1 .. address'last;
      Physical_Base_Address         : address RANGE 1 .. address'last;
   END RECORD
   WITH
      Dynamic_Predicate => -- A few limits to counter wrap-around situations.
      (
         Pixels_Per_Scanline >= Horizontal_Resolution AND THEN
         Memory_Map_Address < 2**48 - 1               AND THEN -- Will never
         Memory_Map_Size < 4 * GiB                    AND THEN -- go over this.
         Memory_Map_Descriptor_Size * (Memory_Map_Size /
            Memory_Map_Descriptor_Size) = Memory_Map_Size
      ),
      Convention        => C;
   FOR arguments USE RECORD
      Graphics_Mode_Current            AT 000 RANGE 0 .. 031;
      Graphics_Mode_Max                AT 004 RANGE 0 .. 031;
      Framebuffer_Address              AT 008 RANGE 0 .. 063;
      Framebuffer_Size                 AT 016 RANGE 0 .. 063;
      Horizontal_Resolution            AT 024 RANGE 0 .. 031;
      Vertical_Resolution              AT 028 RANGE 0 .. 031;
      Pixels_Per_Scanline              AT 032 RANGE 0 .. 031;
      Pixel_Format                     AT 036 RANGE 0 .. 031;
      Pixel_Bitmask                    AT 040 RANGE 0 .. 127;
      Memory_Map_Address               AT 056 RANGE 0 .. 063;
      Memory_Map_Key                   AT 064 RANGE 0 .. 063;
      Memory_Map_Size                  AT 072 RANGE 0 .. 063;
      Memory_Map_Descriptor_Size       AT 080 RANGE 0 .. 063;
      Memory_Map_Descriptor_Version    AT 088 RANGE 0 .. 031;
      RSDP_Address                     AT 092 RANGE 0 .. 063;
      Physical_Base_Address            AT 100 RANGE 0 .. 063;
   END RECORD;

   -- I use a double pointer to the bootloader arguments structure.
   TYPE access_arguments IS NOT NULL ACCESS arguments;

   -- Points to a memory descriptor in the memory map.
   TYPE access_memory_descriptor IS ACCESS memory_descriptor;

   -- An array of memory descriptors - a memory map. It contains accesses to
   -- the descriptors to account for the dynamic descriptor size.
   Memory_Map : ARRAY(number RANGE 1 .. 512) OF access_memory_descriptor;

   -- This parses and fills in the memory map array. It should be called before
   -- the memory map is utilised.
   PROCEDURE Parse_Memory_Map
   WITH
      Post => (FOR SOME Region OF Memory_Map => Region /= NULL);

   -- Takes in a memory descriptor and then returns a record of booleans
   -- indicating the status of all possible UEFI memory attributes.
   FUNCTION Get_Memory_Attributes
     (Region : NOT NULL ACCESS CONSTANT memory_descriptor)
      RETURN memory_attributes;

   Bootloader_Arguments : CONSTANT access_arguments
   WITH
      Import     => true,
      Convention => Assembler,
      Link_Name  => "global__bootloader_arguments";

END HAVK_Kernel.UEFI;
