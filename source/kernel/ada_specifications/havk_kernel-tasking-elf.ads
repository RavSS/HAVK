-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-elf.ads                                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

-- This package contains logic for loading programs in the Executable and
-- Linkable Format (ELF). It only supports ELF64, as HAVK is solely an x86-64
-- kernel.
-- READ: https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
PACKAGE HAVK_Kernel.Tasking.ELF
IS
   -- This procedure loads all sections of an ELF file that is placed into
   -- memory.
   -- TODO: The validation logic for this as of now is extremely naive and
   -- should not be considered anywhere close to being secure when it comes to
   -- parsing user data. Checking header and entry sanity beyond the basics is
   -- required. I would say that this is the most vulnerable part of the
   -- kernel that user tasks/programs interact with.
   PROCEDURE Load
     (File_Address : IN Memory.canonical_address;
      File_Size    : IN number;
      Task_Name    : IN string;
      Error_Status : OUT error);

PRIVATE
   ELF_Tag : CONSTANT string := "ELF";

   -- Describes which ELF format the file is in (32-bit or 64-bit).
   TYPE identity_class IS NEW byte;
   Protected_Mode : CONSTANT identity_class := 1;
   Long_Mode      : CONSTANT identity_class := 2;

   -- The endianness of the object file. This should always be little endian
   -- for x86-64.
   TYPE identity_endianness IS NEW byte;
   Little_Endian : CONSTANT identity_endianness := 1;
   Big_Endian    : CONSTANT identity_endianness := 2;

   -- This identifies the first several bytes of an ELF file. It belongs to the
   -- ELF header structure.
   TYPE identity IS LIMITED RECORD
      -- The first byte of the file is 0x7F if it's valid.
      Magic_Byte       : number RANGE 0 .. 2**08 - 1 := 0;
      -- For a valid ELF file, this should always be "ELF".
      Magic_Name       : string(1 .. 3) := (OTHERS => NUL);
      -- This should always be 64-bit (two), as HAVK has no reason to support
      -- IA-32 for legacy purposes.
      Class            : identity_class := identity_class'first;
      -- This should always be little endian (one), as we're using x86-64.
      Endianness       : identity_endianness := identity_endianness'first;
      -- This should always be one as of 2020-06-17.
      Version          : number RANGE 0 .. 2**08 - 1 := 0;
      -- This identifies the operating system ABI. For our use-case, it should
      -- just be zero, as that indicates the System V ABI. For e.g. Linux, it
      -- would be three, and for GNU Hurd, it would be 4.
      ABI              : number RANGE 0 .. 2**08 - 1 := 0;
      -- This is mostly ignored, although its interpretation is up to the
      -- kernel itself. Linux sees it as a part of the padding field below it.
      ABI_Version      : number RANGE 0 .. 2**08 - 1 := 0;
      -- This space does nothing for now, so it's just padding.
      Padding          : number RANGE 0 .. 2**56 - 1 := 0;
   END RECORD
   WITH
      Object_Size => (9 * 8) + 55 + 1;
   FOR identity USE RECORD
      Magic_Byte           AT 0 RANGE 0 .. 07;
      Magic_Name           AT 1 RANGE 0 .. 23;
      Class                AT 4 RANGE 0 .. 07;
      Endianness           AT 5 RANGE 0 .. 07;
      Version              AT 6 RANGE 0 .. 07;
      ABI                  AT 7 RANGE 0 .. 07;
      ABI_Version          AT 8 RANGE 0 .. 07;
      Padding              AT 9 RANGE 0 .. 55;
   END RECORD;

   -- Indicates what the ELF file actually is. For most purposes, it should
   -- always be an executable object file.
   TYPE header_file_type IS NEW word;
   No_Object_File                      : CONSTANT header_file_type := 16#0000#;
   Relocatable_Object_File             : CONSTANT header_file_type := 16#0002#;
   Executable_Object_File              : CONSTANT header_file_type := 16#0002#;
   Dynamic_Object_File                 : CONSTANT header_file_type := 16#0003#;
   Core_Object_File                    : CONSTANT header_file_type := 16#0004#;
   OS_Specific_Object_File_Low         : CONSTANT header_file_type := 16#FE00#;
   OS_Specific_Object_File_High        : CONSTANT header_file_type := 16#FEFF#;
   Processor_Specific_Object_File_Low  : CONSTANT header_file_type := 16#FF00#;
   Processor_Specific_Object_File_High : CONSTANT header_file_type := 16#FFFF#;

   TYPE file_header IS LIMITED RECORD
      -- Contains information that can identify the ELF file's purpose etc.
      File_Identity              : identity;
      -- For our purposes, this will always be set to two (also known as
      -- "ET_EXEC" or "dynamic_object_file").
      File_Type                  : header_file_type := No_Object_File;
      -- The text section's instruction set. For x86-64/AMD64/IA-32e, this
      -- should be 0x3E. HAVK is only made for x86-64, so we can ignore
      -- anything that doesn't hold the value 0x3E here.
      Instruction_Set            : number RANGE 0 .. 2**16 - 1 := 0;
      -- Similar to the version field found in the identity structure, this
      -- should be one.
      Version                    : number RANGE 0 .. 2**32 - 1 := 0;
      -- This is the address that the task should jump to upon task startup. It
      -- should match the one we've defined in the tasking package, or else the
      -- executable was not compiled properly (i.e. not to our standards).
      Entry_Address              : address := 0;
      -- The offset of the program header table from the start of the file.
      -- This is where the segments will be loaded from.
      Program_Header_Offset      : address := 0;
      -- The offset of the section header table from the start of the file. It
      -- is not of much interest outside of retrieving the values of symbols
      -- and whatnot.
      Section_Header_Offset      : address := 0;
      -- TODO: An architecture-specific field. This likely has some information
      -- useful for x86-64.
      Flags                      : number RANGE 0 .. 2**32 - 1 := 0;
      -- The size value of this very header. The header itself is 64 bytes for
      -- 64-bit and 32 bytes for 32-bit.
      Header_Size                : number RANGE 0 .. 2**16 - 1 := 0;
      -- The size of an entry inside the program header table.
      Program_Header_Entry_Size  : number RANGE 0 .. 2**16 - 1 := 0;
      -- The amount of entries in the program header table.
      Program_Header_Entry_Count : number RANGE 0 .. 2**16 - 1 := 0;
      -- The size of an entry inside the section header table.
      Section_Header_Entry_Size  : number RANGE 0 .. 2**16 - 1 := 0;
      -- The amount of entries in the section header table.
      Section_Header_Entry_Count : number RANGE 0 .. 2**16 - 1 := 0;
      -- This is the section header entry that contains the section names. It's
      -- a special entry that is in itself a table of strings. Required for
      -- finding symbol names and getting their values.
      Section_Header_Names_Index : number RANGE 0 .. 2**16 - 1 := 0;
   END RECORD
   WITH
      Object_Size => (62 * 8) + 15 + 1;
   FOR file_header USE RECORD
      File_Identity                AT 00 RANGE 0 .. 127;
      File_Type                    AT 16 RANGE 0 .. 015;
      Instruction_Set              AT 18 RANGE 0 .. 015;
      Version                      AT 20 RANGE 0 .. 031;
      Entry_Address                AT 24 RANGE 0 .. 063;
      Program_Header_Offset        AT 32 RANGE 0 .. 063;
      Section_Header_Offset        AT 40 RANGE 0 .. 063;
      Flags                        AT 48 RANGE 0 .. 031;
      Header_Size                  AT 52 RANGE 0 .. 015;
      Program_Header_Entry_Size    AT 54 RANGE 0 .. 015;
      Program_Header_Entry_Count   AT 56 RANGE 0 .. 015;
      Section_Header_Entry_Size    AT 58 RANGE 0 .. 015;
      Section_Header_Entry_Count   AT 60 RANGE 0 .. 015;
      Section_Header_Names_Index   AT 62 RANGE 0 .. 015;
   END RECORD;

   -- Indicates what type of segment the specific program header table entry is
   -- for. In our situation, we only need to focus on the loadable segments and
   -- ignore everything else. I've also included some GNU extensions, which are
   -- very common to encounter when using GCC.
   TYPE segment IS NEW doubleword;
   Null_Segment                          : CONSTANT segment := 16#00000000#;
   Loadable_Segment                      : CONSTANT segment := 16#00000001#;
   Dynamic_Linking_Segment               : CONSTANT segment := 16#00000002#;
   Interpreter_Segment                   : CONSTANT segment := 16#00000003#;
   Auxiliary_Segment                     : CONSTANT segment := 16#00000004#;
   Reserved_Segment                      : CONSTANT segment := 16#00000005#;
   Program_Header_Table_Segment          : CONSTANT segment := 16#00000006#;
   Thread_Local_Storage_Template_Segment : CONSTANT segment := 16#00000007#;
   OS_Specific_Segment_Low               : CONSTANT segment := 16#60000000#;
   GNU_Exception_Information_Segment     : CONSTANT segment := 16#6474E550#;
   GNU_Stack_Segment                     : CONSTANT segment := 16#6474E551#;
   OS_Specific_Segment_High              : CONSTANT segment := 16#6FFFFFFF#;
   Processor_Specific_Low                : CONSTANT segment := 16#70000000#;
   Processor_Specific_High               : CONSTANT segment := 16#7FFFFFFF#;

   -- Indicates the memory permissions for each segment. I've decided to
   -- interpret the flags exactly as ELF defines them. For the sake of
   -- convenience, I've also defined this type as all the possible combinations
   -- instead of the individual flag masks.
   TYPE segment_flag IS NEW doubleword; -- R (Read) W (Write) (X) (Execute)
   No_Access_Segment                : CONSTANT segment_flag := 0; -- ___
   Executable_Segment               : CONSTANT segment_flag := 1; -- __X
   Writeable_Segment                : CONSTANT segment_flag := 2; -- _W_
   Writeable_And_Executable_Segment : CONSTANT segment_flag := 3; -- _WX
   Readable_Segment                 : CONSTANT segment_flag := 4; -- R__
   Readable_And_Executable_Segment  : CONSTANT segment_flag := 5; -- R_X
   Readable_And_Writeable_Segment   : CONSTANT segment_flag := 6; -- RW_
   Full_Access_Segment              : CONSTANT segment_flag := 7; -- RWX
   Unspecified_Access_Segment       : CONSTANT segment_flag := 16#F0000000#;

   -- This structure format makes up the entries in the program header table,
   -- which can be found at the start of the file plus the program header
   -- offset specified in the file header.
   TYPE program_header_entry IS LIMITED RECORD
      -- This indicates what the entry is for. For executable loading purposes,
      -- ignore everything that isn't a loadable segment.
      Segment_Type     : segment := Null_Segment;
      -- Indicates the memory protection for the specified segment.
      Permission_Flag  : segment_flag := No_Access_Segment;
      -- The offset of the segment from the beginning of the ELF file.
      File_Offset      : address := 0;
      -- The virtual address of the segment in memory. This is what needs to be
      -- mapped.
      Virtual_Address  : address := 0;
      -- The physical address. I believe we can ignore this for our situation,
      -- as our executables are unaware of physical memory.
      Physical_Address : address := 0;
      -- The size of the segment in the file itself. This is used for
      -- retrieving the segment from the file itself.
      File_Size        : number := 0;
      -- The size of the segment when put into memory. Use this for mapping and
      -- loading the segment into memory.
      Memory_Size      : number := 0;
      -- This is the boundary that the segment must be placed on. Values zero
      -- and one indicate that there is no alignment. This should be a positive
      -- value that indicates `Virtual_Address = File_Offset MOD Alignment`.
      Alignment        : number := 0;
   END RECORD
   WITH
      Object_Size => (48 * 8) + 63 + 1;
   FOR program_header_entry USE RECORD
      Segment_Type        AT 00 RANGE 0 .. 31;
      Permission_Flag     AT 04 RANGE 0 .. 31;
      File_Offset         AT 08 RANGE 0 .. 63;
      Virtual_Address     AT 16 RANGE 0 .. 63;
      Physical_Address    AT 24 RANGE 0 .. 63;
      File_Size           AT 32 RANGE 0 .. 63;
      Memory_Size         AT 40 RANGE 0 .. 63;
      Alignment           AT 48 RANGE 0 .. 63;
   END RECORD;

   -- Describes what type of section an entry in the section header table is
   -- for. There's more than the ones shown below, so expecting unknown
   -- sections before proceeding is vital.
   TYPE section IS NEW doubleword;
   Null_Section                        : CONSTANT section := 16#00000000#;
   Program_Data_Section                : CONSTANT section := 16#00000001#;
   Symbol_Table_Section                : CONSTANT section := 16#00000002#;
   String_Table_Section                : CONSTANT section := 16#00000003#;
   Relocation_Entries_Addend_Section   : CONSTANT section := 16#00000004#;
   Symbol_hash_table_Section           : CONSTANT section := 16#00000005#;
   Dynamic_linking_Section             : CONSTANT section := 16#00000006#;
   Notes_Section                       : CONSTANT section := 16#00000007#;
   Bss_Section                         : CONSTANT section := 16#00000008#;
   Relocation_Entries_Section          : CONSTANT section := 16#00000009#;
   Reserved_Section                    : CONSTANT section := 16#0000000A#;
   Dynamic_Linker_Symbol_Table_Section : CONSTANT section := 16#0000000B#;
   Constructor_Array_Section           : CONSTANT section := 16#0000000E#;
   Destructor_Array_Section            : CONSTANT section := 16#0000000F#;
   Preconstructor_Array_Section        : CONSTANT section := 16#00000010#;
   Group_Section                       : CONSTANT section := 16#00000011#;
   Indices_Section                     : CONSTANT section := 16#00000012#;
   Number_Of_Section_Types_Section     : CONSTANT section := 16#00000013#;
   OS_Specific_Section_Base            : CONSTANT section := 16#60000000#;

   -- A variety of flags that indicate special characteristics for a section.
   TYPE section_flag IS NEW doubleword;
   Writeable_Section_Flag            : CONSTANT section_flag := 16#00000001#;
   Allocated_Section_Flag            : CONSTANT section_flag := 16#00000002#;
   Executable_Section_Flag           : CONSTANT section_flag := 16#00000004#;
   Mergeable_Section_Flag            : CONSTANT section_flag := 16#00000010#;
   Strings_Section_Flag              : CONSTANT section_flag := 16#00000020#;
   Extra_Information_Section_Flag    : CONSTANT section_flag := 16#00000040#;
   Strict_Order_Section_Flag         : CONSTANT section_flag := 16#00000080#;
   Specially_Handled_Section_Flag    : CONSTANT section_flag := 16#00000100#;
   Group_Member_Section_Flag         : CONSTANT section_flag := 16#00000200#;
   Thread_Local_Storage_Section_Flag : CONSTANT section_flag := 16#00000400#;
   OS_Specific_Section_Flag          : CONSTANT section_flag := 16#0FF00000#;
   Specially_Ordered_Section_Flag    : CONSTANT section_flag := 16#40000000#;
   Potentially_Excluded_Section_Flag : CONSTANT section_flag := 16#80000000#;
   Processor_Specific_Section_Flag   : CONSTANT section_flag := 16#F0000000#;

   -- This makes up the section header table. Each entry describes a section.
   TYPE section_header_entry IS LIMITED RECORD
      -- This index gives the string name of the section when used with the
      -- special string section. The index of that section can be found in the
      -- file header.
      Name_Index           : number RANGE 0 .. 2**32 - 1 := 0;
      -- Gives an attribute of the section. Not sure if they're combinable.
      Section_Type         : section := section'first;
      -- A value that can potentially contain numerous flags.
      Flags                : number := 0;
      -- The virtual address of the section if it's also a loadable section;
      -- otherwise, this is likely zero.
      Virtual_Address      : address := 0;
      -- The offset from the beginning of the file at which the section can be
      -- located.
      File_Offset          : address := 0;
      -- The size of the section in the file. If the section only contains
      -- metadata, then this will be zero.
      File_Size            : number := 0;
      -- An index of another section that indicates the current section is
      -- linked to it.
      Linked_Section_Index : number RANGE 0 .. 2**32 - 1 := 0;
      -- A field for any extra information. This is determined by the section
      -- type.
      Extra_Information    : number RANGE 0 .. 2**32 - 1 := 0;
      -- The alignment of the section. Power of two as usual.
      Alignment            : number := 0;
      -- If the section entry itself contains entries, then this indicates the
      -- size of them.
      Section_Entry_Size   : number := 0;
   END RECORD
   WITH
      Object_Size => (56 * 8) + 63 + 1;
   FOR section_header_entry USE RECORD
      Name_Index              AT 00 RANGE 0 .. 31;
      Section_Type            AT 04 RANGE 0 .. 31;
      Flags                   AT 08 RANGE 0 .. 63;
      Virtual_Address         AT 16 RANGE 0 .. 63;
      File_Offset             AT 24 RANGE 0 .. 63;
      File_Size               AT 32 RANGE 0 .. 63;
      Linked_Section_Index    AT 40 RANGE 0 .. 31;
      Extra_Information       AT 44 RANGE 0 .. 31;
      Alignment               AT 48 RANGE 0 .. 63;
      Section_Entry_Size      AT 56 RANGE 0 .. 63;
   END RECORD;

   -- This checks whether or not the passed ELF file header is sane or not.
   FUNCTION Valid_File_Header
     (Header    : IN file_header;
      File_Size : IN number)
      RETURN boolean;

   -- Similar to `Valid_File_Header()`, but for ELF program headers instead.
   FUNCTION Valid_Program_Header_Entry
     (Header       : IN program_header_entry;
      File_Address : IN Memory.canonical_address;
      File_Size    : IN number)
      RETURN boolean
   WITH
      Pre => address(File_Address) + address(File_Size) NOT IN
                Memory.invalid_address'range;

END HAVK_Kernel.Tasking.ELF;
