-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-paging.ads                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package has the purpose of managing page translation.
-- TODO: Please check the record/structure of "page_layout". I am unsure of
-- how many tables I need for each level. All I truly know is that there's only
-- a single page map (PML4) due to x86-64 only practically supporting a 48-bit
-- virtual address space instead of its theoretical max of 64-bit, as a single
-- page map covers 256 TiB (48 bits).
PACKAGE HAVK_Kernel.Paging
IS
   -- READ: https://wiki.osdev.org/Page_Tables#48-bit_virtual_address_space
   -- See "Figure 5-1. Virtual to Physical Address Translation-Long Mode"
   -- in the AMD64 system programming manual (24593-Rev. 3.32-October 2019),
   -- as it gives an idea on how the page structure should be programmed.
   -- Also see "Figure 5-17. 4-Kbyte Page Translation-Long Mode".

   -- For x86-64 (as of writing), there are three page sizes. The standard is
   -- 4 kibibytes. Huge pages are 2 mebibytes and "giant" (as I call them)
   -- pages are 1 gibibyte, where only the latter may not be supported by the
   -- processor. It can be checked in CPUID's output (bit 26 of EAX).
   SUBTYPE page_frame_variant IS number RANGE  4 * KiB .. 1 * GiB;
   Page       : CONSTANT page_frame_variant := 4 * KiB;
   Huge_Page  : CONSTANT page_frame_variant := 2 * MiB;
   Giant_Page : CONSTANT page_frame_variant := 1 * GiB; -- Needs CPU support.

   -- This record contains the structure for a map entry.
   TYPE map_entry IS RECORD
      -- Whether the directory is "active".
      Present      : boolean                          := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean                          := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean                          := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean                          := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean                          := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean                          := false;
      -- There is no "dirty" field. This field is ignored and can be
      -- used for anything.
      Ignored_1    : boolean                          := false;
      -- There is no "huge" or "global" field. This field must be zeroed.
      Zeroed       : number RANGE 0 .. 00000000000000 := 0;
      -- Bits that are available for anything.
      Available_1  : number RANGE 0 .. 00000000000007 := 0;
      -- A thin pointer to the base address of the directory pointer table
      -- this entry points to. Bits 11 to 0 are zero, as the address
      -- must be 4-KiB aligned regardless of page size. You must do a
      -- right shift of 12.
      Pointer      : number RANGE 0 .. 16#FFFFFFFFFF# := 0;
      -- Bits that are available for anything.
      Available_2  : number RANGE 0 .. 16#00000003FF# := 0;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean                          := false;
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
   TYPE directory_entry IS RECORD
      -- Whether the directory is "active".
      Present      : boolean                          := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean                          := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean                          := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean                          := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean                          := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean                          := false;
      -- There is no "dirty" field. This field is ignored and can be
      -- used for anything.
      Ignored_1    : boolean                          := false;
      -- Indicates whether or not the page entry is for a huge or giant page.
      -- If the entry is for a directory pointer table, then it is for a
      -- giant page. If the entry is for a directory table, then it is for a
      -- huge page. If this is true in both cases, then the "Pointer" field
      -- is actually a physical frame. It is expected that the lower bits are
      -- zeroed, as the frame address should be aligned to its respective
      -- alignment. It is recommended not to use the more specialised fields
      -- before this if you are going to use a larger than normal page size.
      Huge         : boolean                          := false;
      -- There is no "global" field. This is another ignored set of bits.
      Ignored_2    : boolean                          := false;
      -- Bits that are available for anything.
      Available_1  : number RANGE 0 .. 00000000000007 := 0;
      -- A thin pointer to the base address of the page table this
      -- entry points to. Bits 11 to 0 are zero, as the address
      -- must be 4-KiB aligned regardless of page size. You must do a
      -- right shift of 12.
      Pointer      : number RANGE 0 .. 16#FFFFFFFFFF# := 0;
      -- Bits that are available for anything.
      Available_2  : number RANGE 0 .. 16#00000003FF# := 0;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean                          := false;
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
   TYPE page_entry IS RECORD
      -- Whether the directory is "active".
      Present      : boolean                          := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean                          := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean                          := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean                          := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean                          := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean                          := false;
      -- True if the page has had physical data written to it. This again
      -- is never set to false by the processor, our code must reset it.
      Dirty        : boolean                          := false;
      -- TODO: This bit (also known as the PAT bit) does something with
      -- the other page attributes ("Writethrough" and "Uncacheable").
      -- Needs to be supported by the CPU. I need to learn more about
      -- this and the named "PAT register".
      Attribute    : boolean                          := false;
      -- Whether the physical page is global. When true, the TLB entry
      -- will not be invalidated if CR3 is changed e.g. `MOV CR*, *`.
      -- I believe the purpose of this is to help performance when some
      -- things in the directory don't change between paging structure
      -- switches for different processes etc. Only changeable for the
      -- lowest paging level, otherwise it's ignored and should be false.
      Global       : boolean                          := false;
      -- Bits that are available for anything.
      Available_1  : number RANGE 0 .. 00000000000007 := 0;
      -- Points to the physical frame that is 4-KiB aligned. Must be shifted
      -- to the right by 12, much like the page structure pointers.
      Frame        : number RANGE 0 .. 16#FFFFFFFFFF# := 0;
      -- Bits that are available for anything.
      Available_2  : number RANGE 0 .. 16#00000003FF# := 0;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean                          := false;
   END RECORD
   WITH
      Volatile => true;
   FOR page_entry USE RECORD
      Present       AT 0 RANGE 00 .. 00;
      Write_Access  AT 0 RANGE 01 .. 01;
      User_Access   AT 0 RANGE 02 .. 02;
      Writethrough  AT 0 RANGE 03 .. 03;
      Uncacheable   AT 0 RANGE 04 .. 04;
      Accessed      AT 0 RANGE 05 .. 05;
      Dirty         AT 0 RANGE 06 .. 06;
      Attribute     AT 0 RANGE 07 .. 07;
      Global        AT 0 RANGE 08 .. 08;
      Available_1   AT 0 RANGE 09 .. 11;
      Frame         AT 0 RANGE 12 .. 51;
      Available_2   AT 0 RANGE 52 .. 62;
      NX            AT 0 RANGE 63 .. 63;
   END RECORD;

   -- The range of entries in any type of directory. I've started it from
   -- zero, so that way, converting a virtual address to a physical address
   -- requires less work, both technically and mentally.
   SUBTYPE page_mask IS number RANGE 0 .. 511;

   -- Level 4 structure. A page map table with 512 entries.
   TYPE map_table               IS ARRAY(page_mask) OF
      ALIASED map_entry
   WITH
      Component_Size => 64,
      Alignment      => Page;

   -- Level 3 structure. A page directory pointer table with 512 entries.
   TYPE directory_pointer_table IS ARRAY(page_mask) OF
      ALIASED directory_pointer_entry
   WITH
      Component_Size => 64,
      Alignment      => Page;

   -- Level 2 structure. A page directory table with 512 entries.
   TYPE directory_table         IS ARRAY(page_mask) OF
      ALIASED directory_entry
   WITH
      Component_Size => 64,
      Alignment      => Page;

   -- Level 1 structure. A page table with 512 entries.
   TYPE page_table              IS ARRAY(page_mask) OF
      ALIASED page_entry
   WITH
      Component_Size => 64,
      Alignment      => Page;

   -- Access types that should point towards a table in the kernel's heap.
   -- A pointer table is guaranteed to be allocated, as the map table is not
   -- capable of defining a physical frame of any size alone. The page map can
   -- be statically allocated in the BSS section, not the heap.
   TYPE access_directory_pointer_table IS ACCESS directory_pointer_table;
   TYPE access_directory_table         IS ACCESS         directory_table;
   TYPE access_page_table              IS ACCESS              page_table;

   -- The default page layout class. Can be freely expanded if needed.
   -- Only the address of the L4 (PML4/page map) must be accessible, all other
   -- record fields will not impact the paging mechanism. Don't use the address
   -- of the record as a substitute for the page map's address, as the hidden
   -- "_tag" field is likely before it.
   TYPE page_layout IS TAGGED RECORD
      L4        : ALIASED map_table;
   END RECORD;

   -- In some cases, we want the object on the heap instead for e.g. a
   -- processes virtual space. This lets us put it inside another record
   -- without heavily expanding that parent record itself.
   TYPE access_page_layout IS ACCESS page_layout;

   -- Moves the base 4-KiB aligned address of the directory map table
   -- to the CR3 register to switch virtual address mappings.
   PROCEDURE Load
     (Object    : IN page_layout);

   -- Converts a size in bytes to an amount of certain physical page frames.
   -- Purposefully does not take in zero bytes or else a condition fails.
   FUNCTION Size_To_Pages
     (Size      : IN number;
      Alignment : IN page_frame_variant := Page)
      RETURN number
   WITH
      Inline => true,
      Pre    => Alignment =       Page OR ELSE
                Alignment =  Huge_Page OR ELSE
                Alignment = Giant_Page;

   -- Maps a virtual address to a physical address. Handles all page sizes.
   -- While it does align pages for the caller just to be sure, it rounds
   -- them downwards. Note that it supports a very limited amount of pages
   -- to reduce load size. Try using the larger page sizes to avoid oddities.
   -- You should adhere to W^X, but also favour the least possible privileges.
   PRAGMA Warnings(GNATprove, off, "unused initial value",
         Reason => "GNATprove is unaware of MMU page permissions.");
   PROCEDURE Map_Address
     (Object           : IN OUT page_layout;
      Virtual_Address  : IN number;
      Physical_Address : IN number;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
   WITH -- You can either write or execute. You can also do neither.
      Pre'class => (IF Write_Access THEN No_Execution) AND THEN
                   (Page_Size =       Page              OR ELSE
                    Page_Size =  Huge_Page              OR ELSE
                    Page_Size = Giant_Page);

   -- Shortcut procedure for mapping a virtual address range to a physical
   -- address range. The range is determined by the size, which is then
   -- converted into physical pages. The range is linear.
   -- See the `Map_Address` procedure for more details.
   PROCEDURE Map_Address_Range
     (Object           : IN OUT page_layout;
      Virtual_Address  : IN number;
      Physical_Address : IN number;
      Size             : IN number;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
   WITH
      Inline    => true, -- See `Map_Address` for the explanation about W^X.
      Pre'class => (IF Write_Access THEN No_Execution) AND THEN
                   (Page_Size =       Page              OR ELSE
                    Page_Size =  Huge_Page              OR ELSE
                    Page_Size = Giant_Page);

   PROCEDURE Page_Fault_Handler
     (Error_Code : IN number)
   WITH
      No_Return => true, -- Temporary handling of the page fault.
      Inline    => true,
      Pre       => Error_Code <= 16#FFFFFFFF#; -- Error codes are 32-bits.

END HAVK_Kernel.Paging;
