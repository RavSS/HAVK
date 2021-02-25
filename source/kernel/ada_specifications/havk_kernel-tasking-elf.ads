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
   TYPE identity_class IS
     (protected_mode,
      long_mode);
   FOR identity_class USE
     (protected_mode => 1,
      long_mode      => 2);

   -- The endianness of the object file. This should always be little endian
   -- for x86-64.
   TYPE identity_endianness IS
     (little_endian,
      big_endian);
   FOR identity_endianness USE
     (little_endian => 1,
      big_endian    => 2);

   -- This identifies the first several bytes of an ELF file. It belongs to the
   -- ELF header structure.
   TYPE identity IS LIMITED RECORD
      -- The first byte of the file is 0x7F if it's valid.
      Magic_Byte       : number RANGE 0 .. 2**08 - 1 := 0;
      -- For a valid ELF file, this should always be "ELF".
      Magic_Name       :       string(1 .. 3) := (OTHERS => character'val(0));
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
   END RECORD;
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
   TYPE header_file_type IS
     (no_object_file,
      relocatable_object_file,
      executable_object_file,
      dynamic_object_file,
      core_object_file,
      low_operating_system_specific_object_file,
      high_operating_system_specific_object_file,
      low_processor_specific_object_file,
      high_processor_specific_object_file);
   FOR header_file_type USE
     (no_object_file                             => 16#0000#,
      relocatable_object_file                    => 16#0001#,
      executable_object_file                     => 16#0002#,
      dynamic_object_file                        => 16#0003#,
      core_object_file                           => 16#0004#,
      low_operating_system_specific_object_file  => 16#FE00#,
      high_operating_system_specific_object_file => 16#FEFF#,
      low_processor_specific_object_file         => 16#FF00#,
      high_processor_specific_object_file        => 16#FFFF#);

   TYPE file_header IS LIMITED RECORD
      -- Contains information that can identify the ELF file's purpose etc.
      File_Identity              : identity;
      -- For our purposes, this will always be set to two (also known as
      -- "ET_EXEC" or "dynamic_object_file").
      File_Type                  : header_file_type := no_object_file;
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
   END RECORD;
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
   TYPE segment IS
     (null_segment,
      loadable_segment,
      dynamic_linking_segment,
      interpreter_segment,
      auxiliary_segment,
      reserved_segment,
      program_header_table_segment,
      thread_local_storage_template_segment,
      low_operating_system_specific_segment,
      GNU_exception_handling_information_segment,
      GNU_stack_segment,
      high_operating_system_specific_segment,
      low_processor_specific_segment,
      high_processor_specific_segment);
   FOR segment USE
     (null_segment                               => 16#00000000#,
      loadable_segment                           => 16#00000001#,
      dynamic_linking_segment                    => 16#00000002#,
      interpreter_segment                        => 16#00000003#,
      auxiliary_segment                          => 16#00000004#,
      reserved_segment                           => 16#00000005#,
      program_header_table_segment               => 16#00000006#,
      thread_local_storage_template_segment      => 16#00000007#,
      low_operating_system_specific_segment      => 16#60000000#,
      GNU_exception_handling_information_segment => 16#6474E550#,
      GNU_stack_segment                          => 16#6474E551#,
      high_operating_system_specific_segment     => 16#6FFFFFFF#,
      low_processor_specific_segment             => 16#70000000#,
      high_processor_specific_segment            => 16#7FFFFFFF#);

   -- Indicates the memory permissions for each segment. I've decided to
   -- interpret the flags exactly as ELF defines them. For the sake of
   -- convenience, I've also defined this type as all the possible combinations
   -- instead of the individual flag masks.
   TYPE segment_flag IS                 -- R (Read) W (Write) (X) (Execute)
     (no_access_segment,                -- ___
      executable_segment,               -- __X
      writeable_segment,                -- _W_
      writeable_and_executable_segment, -- _WX
      readable_segment,                 -- R__
      readable_and_executable_segment,  -- R_X
      readable_and_writeable_segment,   -- RW_
      full_access_segment,              -- RWX
      unspecified_access_segment);      -- ???
   FOR segment_flag USE
     (no_access_segment                => 16#00000000#,
      executable_segment               => 16#00000001#,
      writeable_segment                => 16#00000002#,
      writeable_and_executable_segment => 16#00000003#,
      readable_segment                 => 16#00000004#,
      readable_and_executable_segment  => 16#00000005#,
      readable_and_writeable_segment   => 16#00000006#,
      full_access_segment              => 16#00000007#,
      unspecified_access_segment       => 16#F0000000#);

   -- This structure format makes up the entries in the program header table,
   -- which can be found at the start of the file plus the program header
   -- offset specified in the file header.
   TYPE program_header_entry IS LIMITED RECORD
      -- This indicates what the entry is for. For executable loading purposes,
      -- ignore everything that isn't a loadable segment.
      Segment_Type     : segment := null_segment;
      -- Indicates the memory protection for the specified segment.
      Permission_Flag  : segment_flag := no_access_segment;
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
   END RECORD;
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
   -- for. There's more than the ones shown below, so checking for validity
   -- before attempting a read is vital.
   TYPE section IS
     (null_section,
      program_data_section,
      symbol_table_section,
      string_table_section,
      relocation_entries_addend_section,
      symbol_hash_table_section,
      dynamic_linking_section,
      notes_section,
      bss_section,
      relocation_entries_section,
      reserved_section,
      dynamic_linker_symbol_table_section,
      constructor_array_section,
      destructor_array_section,
      preconstructor_array_section,
      group_section,
      indices_section,
      number_of_section_types_section,
      base_operating_system_specific_sections);
   FOR section USE
     (null_section                            => 16#00000000#,
      program_data_section                    => 16#00000001#,
      symbol_table_section                    => 16#00000002#,
      string_table_section                    => 16#00000003#,
      relocation_entries_addend_section       => 16#00000004#,
      symbol_hash_table_section               => 16#00000005#,
      dynamic_linking_section                 => 16#00000006#,
      notes_section                           => 16#00000007#,
      bss_section                             => 16#00000008#,
      relocation_entries_section              => 16#00000009#,
      reserved_section                        => 16#0000000A#,
      dynamic_linker_symbol_table_section     => 16#0000000B#,
      constructor_array_section               => 16#0000000E#,
      destructor_array_section                => 16#0000000F#,
      preconstructor_array_section            => 16#00000010#,
      group_section                           => 16#00000011#,
      indices_section                         => 16#00000012#,
      number_of_section_types_section         => 16#00000013#,
      base_operating_system_specific_sections => 16#60000000#);

   -- A variety of flags that indicate special characteristics for a section.
   TYPE section_flags IS
     (writable_section_flag,
      allocated_section_flag,
      executable_section_flag,
      mergeable_section_flag,
      strings_section_flag,
      extra_information_section_flag,
      strict_order_section_flag,
      specially_handled_section_flag,
      group_member_section_flag,
      thread_local_storage_section_flag,
      operating_system_specific_section_flag,
      specially_ordered_section_flag,
      potentially_excluded_section_flag,
      processor_specific_section_flag)
   WITH
      Size        => 32,
      Object_Size => number'size;
   FOR section_flags USE
     (writable_section_flag                  => 16#00000001#,
      allocated_section_flag                 => 16#00000002#,
      executable_section_flag                => 16#00000004#,
      mergeable_section_flag                 => 16#00000010#,
      strings_section_flag                   => 16#00000020#,
      extra_information_section_flag         => 16#00000040#,
      strict_order_section_flag              => 16#00000080#,
      specially_handled_section_flag         => 16#00000100#,
      group_member_section_flag              => 16#00000200#,
      thread_local_storage_section_flag      => 16#00000400#,
      operating_system_specific_section_flag => 16#0FF00000#,
      specially_ordered_section_flag         => 16#40000000#,
      potentially_excluded_section_flag      => 16#80000000#,
      processor_specific_section_flag        => 16#F0000000#);

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
   END RECORD;
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
