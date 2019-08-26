-- This package has the purpose of managing page translation.
-- TODO: Currently only supports a page size of 2 MiB. Needs a complete
-- refactor so directory entry records can represent any page size layout.

PACKAGE HAVK_Kernel.Paging
IS
   -- READ: https://wiki.osdev.org/Page_Tables#48-bit_virtual_address_space
   -- Also see "Figure 5-1. Virtual to Physical Address Translation-Long Mode"
   -- in the AMD64 system programming manual. Also see "Figure 5-22. 2-Mbyte
   -- Page Translation-Long Mode" as I'm going to mainly use 2 MiB pages.
   -- 2 MiB page setup uses a 3 level paging structure.
   -- Page Map -> Page Directories -> Page Directory (-> Page).
   -- Remember that the page size is also the page alignment e.g. a 2 MiB
   -- physical page is aligned on a 2 MiB address boundary. That means the
   -- bits from 20 to 0 (least significant) are completely zeroed.

   -- This record is the structure for a 2 MiB page directory entry, and it
   -- does not point to a page table, instead it points to a physical page.
   -- Refer to "Figure 5-25. 2-Mbyte PDE-Long Mode" in the AMD64 system
   -- programming manual.
   TYPE directory_entry_huge IS RECORD
      -- Whether the directory is "active".
      Present          : boolean := false;
      -- When true, both reading and writing is allowed.
      Write_Access     : boolean := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access      : boolean := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough     : boolean := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable      : boolean := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed         : boolean := false;
      -- True if the page has had physical data written to it. This again is
      -- never set to false by the processor, our code must reset it.
      Dirty            : boolean := false;
      -- When true, the page size increases from 4 KiB to 2 MiB (when PAE
      -- is also enabled, which it always should be, else it's 4 MiB).
      -- NOTE: As of now, I'm just going to use 2 MiB pages. 4 KiB seems
      -- too great of a granularity for me, so this is always set to true.
      Huge             : boolean RANGE true .. true := true;
      -- Whether the physical page is global. When true, the TLB entry will
      -- not be invalidated if CR3 is changed e.g. `MOV CR*, *`. I believe
      -- the purpose of this is to help performance when some things in the
      -- directory don't change between paging structure switches for
      -- different processes etc. Only changeable for the lowest paging level,
      -- otherwise it is ignored and should be false.
      Global           : boolean := false;
      -- This next bit can be used for whatever HAVK wants to use it for.
      -- The processor does not care about it or interpret it.
      Ignored_1        : boolean := false;
      -- TODO: This bit (also known as the PAT bit) does something with the
      -- other page attributes ("Writethrough" and "Uncacheable"). Needs to be
      -- supported by the CPU. I need to learn more about this and the
      -- named "PAT register".
      Attribute        : boolean := false;
      -- This must be set to zero due to the first 20 bits of the physical
      -- page being empty.
      Zeroed           : num RANGE 0 .. 0 := 0;
      -- Finally, we have the physical page. The full range is limited to
      -- how the CPU itself works. Bits 20 to 0 are zeroed due to the address
      -- needing to be 2 MiB aligned, obviously, so you must `SHR()` the
      -- physical address by 20 shifts because of that reason.
      -- Non-sliced physical addresses are valid to 0xFFFFFFFE00000.
      Physical_Address : num RANGE 0 .. 16#7FFFFFFF# := 0;
      -- Another "available" field. This can be whatever I want it to be,
      -- but I don't know what to put in here for the time being.
      Ignored_2        : boolean := false;
      -- The NX (non-executable) bit. Self-explanatory.
      NX               : boolean := false;
   END RECORD;
   FOR directory_entry_huge USE RECORD
      Present          AT 0 RANGE  0 ..  0;
      Write_Access     AT 0 RANGE  1 ..  1;
      User_Access      AT 0 RANGE  2 ..  2;
      Writethrough     AT 0 RANGE  3 ..  3;
      Uncacheable      AT 0 RANGE  4 ..  4;
      Accessed         AT 0 RANGE  5 ..  5;
      Dirty            AT 0 RANGE  6 ..  6;
      Huge             AT 0 RANGE  7 ..  7;
      Global           AT 0 RANGE  8 ..  8;
      Ignored_1        AT 0 RANGE  9 .. 11;
      Attribute        AT 0 RANGE 12 .. 12;
      Zeroed           AT 0 RANGE 13 .. 20;
      Physical_Address AT 0 RANGE 21 .. 51;
      Ignored_2        AT 0 RANGE 52 .. 62;
      NX               AT 0 RANGE 63 .. 63;
   END RECORD;

   -- An entry record for the page directory pointer. For information on the
   -- fields, consult the "directory_entry_huge" record.
   TYPE directory_pointer_entry IS RECORD
      Present           : boolean := false;
      Write_Access      : boolean := false;
      User_Access       : boolean := false;
      Writethrough      : boolean := false;
      Uncacheable       : boolean := false;
      Accessed          : boolean := false;
      -- There is no "dirty" field. This field is ignored and can be used for
      -- anything.
      Ignored_1         : boolean := false;
      -- If this is true for the directory pointer table entry, then instead
      -- of pointing to a page directory, it points to a 1 GiB physical page.
      -- I doubt we will ever be needing that.
      Huge              : boolean RANGE false .. false := false;
      -- There is no "global" field. This is another free set of bits.
      Ignored_2         : boolean := false;
      -- A thin pointer to the base address of the directory table this entry
      -- points to. Bits 11 to 0 are zero, as the directory address must be
      -- 4 KiB aligned regardless of huge pages. You must do the right shift.
      -- Non-sliced addresses are valid to 0xFFFFFFFFFF000.
      Directory_Address : num RANGE 0 .. 16#FFFFFFFFFF# := 0;
      Ignored_3         : boolean := false;
      NX                : boolean := false;
   END RECORD;
   FOR directory_pointer_entry USE RECORD
      Present           AT 0 RANGE  0 ..  0;
      Write_Access      AT 0 RANGE  1 ..  1;
      User_Access       AT 0 RANGE  2 ..  2;
      Writethrough      AT 0 RANGE  3 ..  3;
      Uncacheable       AT 0 RANGE  4 ..  4;
      Accessed          AT 0 RANGE  5 ..  5;
      Ignored_1         AT 0 RANGE  6 ..  6;
      Huge              AT 0 RANGE  7 ..  7;
      Ignored_2         AT 0 RANGE  8 .. 11; -- IGN (1) + (3) AVL.
      Directory_Address AT 0 RANGE 12 .. 51;
      Ignored_3         AT 0 RANGE 52 .. 62;
      NX                AT 0 RANGE 63 .. 63;
   END RECORD;

   -- An entry record for the page directory map. For information on the
   -- fields, consult the "directory_entry_huge" record.
   TYPE directory_map_entry IS RECORD
      Present           : boolean := false;
      Write_Access      : boolean := false;
      User_Access       : boolean := false;
      Writethrough      : boolean := false;
      Uncacheable       : boolean := false;
      Accessed          : boolean := false;
      -- There is no "dirty" field. This field is ignored and can be used for
      -- anything.
      Ignored_1         : boolean := false;
      -- There is no "huge"  or "global" field. This field must be zeroed.
      Zeroed            : num RANGE 0 .. 0 := 0;
      -- A thin pointer to the base address of the directory pointer table this
      -- entry points to. Like the other tables, it must be 4 KiB aligned.
      -- It's your job to right shift the address by 11 bits.
      -- Non-sliced addresses are valid to 0xFFFFFFFFFF000.
      Directory_Pointer : num RANGE 0 .. 16#FFFFFFFFFF# := 0;
      Ignored_3         : boolean := false;
      NX                : boolean := false;
   END RECORD;
   FOR directory_map_entry USE RECORD
      Present           AT 0 RANGE  0 ..  0;
      Write_Access      AT 0 RANGE  1 ..  1;
      User_Access       AT 0 RANGE  2 ..  2;
      Writethrough      AT 0 RANGE  3 ..  3;
      Uncacheable       AT 0 RANGE  4 ..  4;
      Accessed          AT 0 RANGE  5 ..  5;
      Ignored_1         AT 0 RANGE  6 ..  6;
      Zeroed            AT 0 RANGE  7 .. 11; -- Two MBZ (2) + (3) AVL.
      Directory_Pointer AT 0 RANGE 12 .. 51;
      Ignored_3         AT 0 RANGE 52 .. 62;
      NX                AT 0 RANGE 63 .. 63;
   END RECORD;

   -- The range of entries in any type of directory. I've started it from
   -- zero, so that way, converting a virtual address to a physical address
   -- requires less work, both technically and mentally.
   SUBTYPE directory_entry_range  IS num RANGE 0 .. 511;

   -- Level 1 structure. A page directory table that contains entries for
   -- 2 MiB physical pages.
   TYPE  directory_table_huge     IS ARRAY(directory_entry_range)
      OF directory_entry_huge    -- References a physical page.
   WITH
      Component_Size => 64,
      Alignment      => 4096; -- Every table must be 4 KiB aligned.

   -- Level 2 structure. A page directory pointer table that contains entries
   -- which point to directory tables.
   TYPE  directory_pointer_table  IS ARRAY(directory_entry_range)
      OF directory_pointer_entry -- References a directory table.
   WITH
      Component_Size => 64,
      Alignment      => 4096;

   -- Level 3 structure. A page directory map table that contains entries
   -- which point to directory pointer tables.
   TYPE  directory_map_table      IS ARRAY(directory_entry_range)
      OF directory_map_entry     -- References a directory pointer table.
   WITH
      Component_Size => 64,
      Alignment      => 4096;

   -- TODO: I have no damn idea what the actual structure of the paging
   -- layout is, every resource is unclear. It's either one of the
   -- following structures for 2 MiB pages if we ignore 4 KiB page size:
   ----------------------------------------------------------------------------
   -- A: 1 "PtrTable" that contains 512 entries. Same for "Table".
   --                                Map
   --                                 |
   --                              PtrTable
   --                                 |
   --                               Table
   ----------------------------------------------------------------------------
   -- B: 512 "PtrTables" that each contain 512 entries. Same for "Tables".
   --                                Map
   --                                 |
   --          ... ___PtrTable________|________PtrTable___ ...
   --                    |                        |
   --        ... Table___|___Table ...... Table___|___Table ...
   ----------------------------------------------------------------------------
   -- I am going to confusingly assume B is correct, because it makes more
   -- sense as changing an entry in the lowest level will be reflected among
   -- all other table entries in structure A. It's a tree-like structure?

   -- Contains 512 directory tables which each have 512 entries.
   TYPE  directory_tables_huge    IS ARRAY(directory_entry_range)
      OF directory_table_huge;

   -- Contains 512 directory pointer tables which each have 512 entries.
   TYPE  directory_pointer_tables IS ARRAY(directory_entry_range)
      OF directory_pointer_table;

   -- The 3 level paging structure layout when using a page size of 2 MiB.
   -- The lowest level contains the physical page, but it's built-in as part
   -- of the directory table when using the huge page size (2 MiB).
   TYPE page_layout_huge IS TAGGED RECORD
      Level_1  : directory_tables_huge;
      Level_2  : directory_pointer_tables;
      Level_3  : directory_map_table;
   END RECORD;

   -- Takes in an address and aligns it to a 2 MiB boundary. Note that it
   -- rounds down, not up, if no second argument is provided.
   FUNCTION Align_Huge(
      Address  : IN num;
      Round_Up : IN boolean := false)
   RETURN num
   WITH
      Inline => true,
      Post   => Align_Huge'result MOD 16#200000# = 0;

   -- Moves the base 4 KiB aligned address of the directory map table
   -- to the CR3 register to switch virtual address mappings.
   PROCEDURE Load(
      Object   : IN page_layout_huge)
   WITH
      Inline => true;

   -- Converts a size in bytes to an amount of 2 MiB physical page frames.
   FUNCTION Size_To_Pages_Huge(
      Size     : IN num)
   RETURN num
   WITH
      Inline        => true,
      Pre           => Size > 0,
      -- Always return at least one page for obvious reasons.
      Post          => Size_To_Pages_Huge'result > 0;

   -- Maps a virtual address to a physical address, huge entry.
   -- Requires all addresses passed to it as 2 MiB aligned.
   PROCEDURE Map_Address(
      Object           : IN OUT page_layout_huge;
      Virtual_Address  : IN num;
      Physical_Address : IN num)
   WITH
      Pre => Virtual_Address = Align_Huge(Virtual_Address) AND THEN
         Physical_Address = Align_Huge(Physical_Address);

   -- Shortcut procedure for identity mapping a virtual address range to a
   -- physical address range. The range is determined by the size, which
   -- is then converted into 2 MiB physical pages. Requires all addresses
   -- passed to it as 2 MiB aligned.
   PROCEDURE Map_Linear_Address_Range(
      Object           : IN OUT page_layout_huge;
      Virtual_Address  : IN num;
      Physical_Address : IN num;
      Size             : IN num)
   WITH
      Inline => true;

   -- TODO: Can I avoid these "Symbol_*" variables without using an access
   -- type? I'd like to not use those due to potential future SPARK rewrites.
   Symbol_Kernel_Base  : CONSTANT System.Address
   WITH
      Import        => true,
      External_Name => "kernel_base";

   Symbol_Kernel_End   : CONSTANT System.Address
   WITH
      Import        => true,
      External_Name => "kernel_end";

   Kernel_Base : CONSTANT num := Address_To_num(Symbol_Kernel_Base'address);
   Kernel_End  : CONSTANT num := Address_To_num(Symbol_Kernel_End'address);
   Kernel_Size : CONSTANT num := Kernel_End - Kernel_Base;
END HAVK_Kernel.Paging;