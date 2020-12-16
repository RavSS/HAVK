-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-paging.ads                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Deallocation;

-- This package has the purpose of managing page translation and mapping.
-- It should be relatively bug free; however, all page map allocations are
-- made on the kernel's heap for now.
PACKAGE HAVK_Kernel.Paging
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
     (MMU_State
      WITH
         External => (Async_Readers, Async_Writers,
                      Effective_Reads, Effective_Writes)),
     (Kernel_Page_Layout_State
      WITH
         External => (Async_Readers, Async_Writers,
                      Effective_Reads, Effective_Writes))
   )
IS
   -- READ: https://wiki.osdev.org/Page_Tables#48-bit_virtual_address_space
   -- See "Figure 5-1. Virtual to Physical Address Translation-Long Mode"
   -- in the AMD64 system programming manual (24593-Rev. 3.32-October 2019),
   -- as it gives an idea on how the page structure should be programmed.
   -- Also see "Figure 5-17. 4-Kbyte Page Translation-Long Mode".

   -- For x86-64 (as of writing), there are three page sizes. The standard is
   -- 4 kibibytes. Huge pages are 2 mebibytes and "giant" (as I call them)
   -- pages are 1-gibibyte, where only the latter may not be supported by the
   -- processor. It can be checked in CPUID's output (bit 26 of EAX).
   -- TODO: Replace the subtype with an enumeration when using Ada 202X.
   TYPE page_frame_variant IS
     (page_size,
      huge_page_size,
      giant_page_size);
   FOR page_frame_variant USE
     (page_size       => 4 * KiB,
      huge_page_size  => 2 * MiB,
      giant_page_size => 1 * GiB); -- Needs CPU support.

   -- This is the main type that is passed around outside of this package. The
   -- fields inside it should not be touched by other packages.
   TYPE page_layout IS LIMITED PRIVATE;

   -- Moves the base 4 KiB aligned address of the page map table to the CR3
   -- register to switch virtual address mappings.
   PROCEDURE Load
     (Layout : IN page_layout);

   -- This goes through the page layout and frees the pointers to other page
   -- structures/tables.
   PROCEDURE Deallocate_Mappings
     (Layout : IN OUT page_layout);

   -- Resolves a virtual address to a physical (frame) address. Returns zero if
   -- it was not mapped to begin with. The size of the frame can be told by the
   -- alignment of it.
   FUNCTION Resolve_Address
     (Layout          : IN page_layout;
      Virtual_Address : IN address)
      RETURN address
   WITH
      Pre => Virtual_Address MOD page_size'enum_rep = 0;

   -- Converts a size in bytes to a certain amount of pages (depending on the
   -- page frame variant).
   FUNCTION Size_To_Pages
     (Size      : IN number;
      Alignment : IN page_frame_variant := page_size)
      RETURN number
   WITH
      Inline => true,
      Post   => Size_To_Pages'result <=
                   number(address'last / Alignment'enum_rep);

   -- Maps a virtual address to a physical address. Handles all page sizes.
   -- While it does align pages for the caller just to be sure, it rounds
   -- them downwards. Note that it supports a very limited amount of pages
   -- to reduce load size. Try using the larger page sizes to avoid oddities.
   -- You should adhere to W^X, but also favour the least possible privileges.
   PRAGMA Warnings(GNATprove, off, "unused initial value",
      Reason => "GNATprove is unaware of MMU page permissions.");
   PROCEDURE Map_Address
     (Layout           : IN OUT page_layout;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Page_Size_Type   : IN page_frame_variant := page_size;
      Present          : IN boolean := true;
      Write_Access     : IN boolean := false;
      User_Access      : IN boolean := false;
      No_Execution     : IN boolean := true)
   WITH -- You can either write or execute. You can also do neither.
      Pre => (IF Write_Access THEN No_Execution);

   -- Shortcut procedure for mapping a virtual address range to a physical
   -- address range. The range is determined by the size, which is then
   -- converted into physical pages. The range is linear.
   -- See the `Map_Address` procedure for more details.
   PROCEDURE Map_Address_Range
     (Layout           : IN OUT page_layout;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size_Type   : IN page_frame_variant := page_size;
      Present          : IN boolean := true;
      Write_Access     : IN boolean := false;
      User_Access      : IN boolean := false;
      No_Execution     : IN boolean := true)
   WITH
      Inline => true, -- See `Map_Address` for the explanation about W^X.
      Pre    => (IF Write_Access THEN No_Execution);

   -- The same as `Map_Address()`, but for the kernel's page layout.
   PROCEDURE Kernel_Map_Address
     (Virtual_Address  : IN address;
      Physical_Address : IN address;
      Page_Size_Type   : IN page_frame_variant := page_size;
      Present          : IN boolean :=  true;
      Write_Access     : IN boolean := false;
      No_Execution     : IN boolean :=  true)
   WITH -- See `Map_Address()` for the explanation about W^X.
      Pre => (IF Write_Access THEN No_Execution);

   -- The same as `Map_Address_Range()`, but for the kernel's page layout.
   PROCEDURE Kernel_Map_Address_Range
     (Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size_Type   : IN page_frame_variant := page_size;
      Present          : IN boolean := true;
      Write_Access     : IN boolean := false;
      No_Execution     : IN boolean := true)
   WITH
      Inline => true, -- See `Map_Address()` for the explanation about W^X.
      Pre    => (IF Write_Access THEN No_Execution);

   -- This loads the kernel page layout and makes sure external functions
   -- (particularly assembly routines) can load the layout properly.
   PROCEDURE Load_Kernel_Page_Layout;

   -- This is for tasking purposes, as it lets the context switching and
   -- scheduling mechanisms to switch tasks correctly. Note that the address
   -- of the level 4 page structure should be page-aligned to begin with, as
   -- I specify the alignment requirement on the array type. If the predicate
   -- check fails and the returned address is not a page-aligned address, then
   -- something is wrong. This does not differentiate from physical to virtual,
   -- so know where the object itself is located in memory before you use this.
   FUNCTION Get_Page_Map_Address
     (Layout : IN page_layout)
      RETURN address
   WITH
      Inline => true,
      Post   => Get_Page_Map_Address'result MOD page_size'enum_rep = 0;

PRIVATE
   Paging_Tag : CONSTANT string := "PAGING";

   -- This record contains the structure for a map entry.
   TYPE map_entry IS LIMITED RECORD
      -- Whether the directory is "active".
      Present      : boolean                     := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean                     := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean                     := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean                     := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean                     := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean                     := false;
      -- There is no "dirty" field. This field is ignored and can be
      -- used for anything.
      Ignored_1    : boolean                     := false;
      -- There is no "huge" or "global" field. This field must be zeroed.
      Zeroed       : number RANGE 0 .. 0         := 0;
      -- Bits that are available for anything.
      Available_1  : number RANGE 0 .. 7         := 0;
      -- A thin pointer to the base address of the directory pointer table
      -- this entry points to. Bits 11 to 0 are zero, as the address
      -- must be 4-KiB aligned regardless of page size. You must do a
      -- right shift of 12.
      Pointer      : number RANGE 0 .. 2**40 - 1 := 0;
      -- Bits that are available for anything.
      Available_2  : number RANGE 0 .. 16#3FF#   := 0;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean                     := false;
   END RECORD;
   FOR map_entry USE RECORD
      Present          AT 0 RANGE 00 .. 00;
      Write_Access     AT 0 RANGE 01 .. 01;
      User_Access      AT 0 RANGE 02 .. 02;
      Writethrough     AT 0 RANGE 03 .. 03;
      Uncacheable      AT 0 RANGE 04 .. 04;
      Accessed         AT 0 RANGE 05 .. 05;
      Ignored_1        AT 0 RANGE 06 .. 06;
      Zeroed           AT 0 RANGE 07 .. 08;
      Available_1      AT 0 RANGE 09 .. 11;
      Pointer          AT 0 RANGE 12 .. 51;
      Available_2      AT 0 RANGE 52 .. 62;
      NX               AT 0 RANGE 63 .. 63;
   END RECORD;

   -- This record contains the structure for a directory pointer table entry.
   TYPE directory_entry IS LIMITED RECORD
      -- Whether the directory is "active".
      Present      : boolean                     := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean                     := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean                     := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean                     := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean                     := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean                     := false;
      -- There is no "dirty" field. This field is ignored and can be
      -- used for anything.
      Ignored_1    : boolean                     := false;
      -- Indicates whether or not the page entry is for a huge or giant page.
      -- If the entry is for a directory pointer table, then it is for a
      -- giant page. If the entry is for a directory table, then it is for a
      -- huge page. If this is true in both cases, then the "Pointer" field
      -- is actually a physical frame. It is expected that the lower bits are
      -- zeroed, as the frame address should be aligned to its respective
      -- alignment. It is recommended not to use the more specialised fields
      -- before this if you are going to use a larger than normal page size.
      Huge         : boolean                     := false;
      -- There is no "global" field. This is another ignored set of bits.
      Ignored_2    : boolean                     := false;
      -- Bits that are available for anything.
      Available_1  : number RANGE 0 .. 7         := 0;
      -- A thin pointer to the base address of the page table this
      -- entry points to. Bits 11 to 0 are zero, as the address
      -- must be 4-KiB aligned regardless of page size. You must do a
      -- right shift of 12.
      Pointer      : number RANGE 0 .. 2**40 - 1 := 0;
      -- Bits that are available for anything.
      Available_2  : number RANGE 0 .. 16#3FF#   := 0;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean                     := false;
   END RECORD;
   FOR directory_entry USE RECORD
      Present          AT 0 RANGE 00 .. 00;
      Write_Access     AT 0 RANGE 01 .. 01;
      User_Access      AT 0 RANGE 02 .. 02;
      Writethrough     AT 0 RANGE 03 .. 03;
      Uncacheable      AT 0 RANGE 04 .. 04;
      Accessed         AT 0 RANGE 05 .. 05;
      Ignored_1        AT 0 RANGE 06 .. 06;
      Huge             AT 0 RANGE 07 .. 07;
      Ignored_2        AT 0 RANGE 08 .. 08;
      Available_1      AT 0 RANGE 09 .. 11;
      Pointer          AT 0 RANGE 12 .. 51;
      Available_2      AT 0 RANGE 52 .. 62;
      NX               AT 0 RANGE 63 .. 63;
   END RECORD;

   -- Page directory pointer entries are the same as page directory entries
   -- when using 4-KiB page sizes.
   TYPE directory_pointer_entry IS NEW directory_entry;

   -- This record contains the structure for a page table entry.
   TYPE page_entry IS LIMITED RECORD
      -- Whether the directory is "active".
      Present      : boolean                     := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean                     := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean                     := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean                     := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean                     := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean                     := false;
      -- True if the page has had physical data written to it. This again
      -- is never set to false by the processor, our code must reset it.
      Dirty        : boolean                     := false;
      -- TODO: This bit (also known as the PAT bit) does something with
      -- the other page attributes ("Writethrough" and "Uncacheable").
      -- Needs to be supported by the CPU. I need to learn more about
      -- this and the named "PAT register".
      Attribute    : boolean                     := false;
      -- Whether the physical page is global. When true, the TLB entry
      -- will not be invalidated if CR3 is changed e.g. `MOV CR*, *`.
      -- I believe the purpose of this is to help performance when some
      -- things in the directory don't change between paging structure
      -- switches for different processes etc. Only changeable for the
      -- lowest paging level, otherwise it's ignored and should be false.
      Global       : boolean                     := false;
      -- Bits that are available for anything.
      Available_1  : number RANGE 0 .. 7         := 0;
      -- Points to the physical frame that is 4-KiB aligned. Must be shifted
      -- to the right by 12, much like the page structure pointers.
      Frame        : number RANGE 0 .. 2**40 - 1 := 0;
      -- Bits that are available for anything.
      Available_2  : number RANGE 0 .. 16#3FF#   := 0;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean                     := false;
   END RECORD;
   FOR page_entry USE RECORD
      Present          AT 0 RANGE 00 .. 00;
      Write_Access     AT 0 RANGE 01 .. 01;
      User_Access      AT 0 RANGE 02 .. 02;
      Writethrough     AT 0 RANGE 03 .. 03;
      Uncacheable      AT 0 RANGE 04 .. 04;
      Accessed         AT 0 RANGE 05 .. 05;
      Dirty            AT 0 RANGE 06 .. 06;
      Attribute        AT 0 RANGE 07 .. 07;
      Global           AT 0 RANGE 08 .. 08;
      Available_1      AT 0 RANGE 09 .. 11;
      Frame            AT 0 RANGE 12 .. 51;
      Available_2      AT 0 RANGE 52 .. 62;
      NX               AT 0 RANGE 63 .. 63;
   END RECORD;

   -- The range of entries in any type of directory. I've started it from
   -- zero, so that way, converting a virtual address to a physical address
   -- requires less work, both technically and mentally.
   SUBTYPE page_mask IS number RANGE 0 .. 2**9 - 1;

   -- Level 4 structure. A page map table with 512 entries.
   TYPE map_table               IS ARRAY(page_mask) OF
      ALIASED map_entry
   WITH
      Component_Size => 64,
      Alignment      => page_size'enum_rep;

   -- Level 3 structure. A page directory pointer table with 512 entries.
   TYPE directory_pointer_table IS ARRAY(page_mask) OF
      ALIASED directory_pointer_entry
   WITH
      Component_Size => 64,
      Alignment      => page_size'enum_rep;

   -- Level 2 structure. A page directory table with 512 entries.
   TYPE directory_table         IS ARRAY(page_mask) OF
      ALIASED directory_entry
   WITH
      Component_Size => 64,
      Alignment      => page_size'enum_rep;

   -- Level 1 structure. A page table with 512 entries.
   TYPE page_table              IS ARRAY(page_mask) OF
      ALIASED page_entry
   WITH
      Component_Size => 64,
      Alignment      => page_size'enum_rep;

   -- Access types that should point towards a table in the kernel's heap.
   -- A pointer table is guaranteed to be allocated, as the map table is not
   -- capable of defining a physical frame of any size alone. The page map can
   -- be statically allocated in the BSS section, not the heap, but for tasks,
   -- it should be allocated on latter.
   TYPE access_directory_pointer_table IS ACCESS directory_pointer_table;
   TYPE access_directory_table         IS ACCESS         directory_table;
   TYPE access_page_table              IS ACCESS              page_table;

   PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
     (object =>        directory_pointer_table,
      name   => access_directory_pointer_table);
   PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
     (object =>                directory_table,
      name   =>         access_directory_table);
   PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
     (object =>                     page_table,
      name   =>              access_page_table);

   -- This turns an address into a pointer to a table. It's useful for
   -- turning a pointer in a table entry into a different lower level table.
   GENERIC
      TYPE        generic_table IS LIMITED PRIVATE;
      TYPE access_generic_table IS ACCESS generic_table;
   FUNCTION To_Pointer
     (Table_Address : IN address)
      RETURN access_generic_table
   WITH
      Import     => true,
      Convention => Intrinsic;

   -- Does the opposite of the above generic function.
   GENERIC
      TYPE        generic_table IS LIMITED PRIVATE;
      TYPE access_generic_table IS ACCESS generic_table;
   FUNCTION To_Address
     (Table_Pointer : IN access_generic_table)
      RETURN address
   WITH
      Import     => true,
      Convention => Intrinsic;

   -- This turns an address into an encoded value, which is then placed into
   -- a page table.
   FUNCTION Encode_Table
     (Value : IN address)
      RETURN number
   WITH
      Inline => true,
      Post   => Encode_Table'result <= 2**40 - 1;

   -- Reverses the process `Encode_Table()` does.
   FUNCTION Decode_Table
     (Value : IN number)
      RETURN address
   WITH
      Inline => true,
      Pre    => Value <= 2**40 - 1,
      Post   => Decode_Table'result MOD page_size'enum_rep = 0;

   -- This sets the value of the CR3 register for the current core.
   PROCEDURE Write_CR3
     (L4_Address : IN address)
   WITH
      Global        => (In_Out => MMU_State),
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__load_page_structure";

   -- This represents the full page layout. Can be freely expanded if needed.
   -- Only the address of the L4 (PML4/page map) must be accessible, all other
   -- record fields will not impact the paging mechanism. Intel is apparently
   -- going to add the ability for a 5th level page structure (L5), so making
   -- this into a discriminant record has been kept open.
   TYPE page_layout IS LIMITED RECORD
      -- The actual page structure. A value in the CR3 register will point to
      -- it. Other packages do not need to see what's inside it and memory
      -- management is handled in a very manual manner.
      L4 : ALIASED map_table;
   END RECORD
   WITH
      Pack => true;

   -- The default paging layout for the entire kernel. This will be referenced
   -- externally in assembler, as it should be the only page layout which maps
   -- every section of the kernel itself, and that visibility is sometimes
   -- needed. This is allocated when first required.
   Kernel_Page_Layout : page_layout
   WITH
      Part_Of => Kernel_Page_Layout_State;

END HAVK_Kernel.Paging;
