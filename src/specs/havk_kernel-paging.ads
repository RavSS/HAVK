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
   TYPE page_frame_variant IS(
      page,        -- 4 KiB.
      huge_page,   -- 2 MiB.
      giant_page); -- 1 GiB. Requires explicit CPU support.
   FOR page_frame_variant USE(
      page       =>     16#1000#,
      huge_page  =>   16#200000#,
      giant_page => 16#40000000#);

   -- This record contains the structure for a map entry.
   TYPE map_entry IS RECORD
      -- Whether the directory is "active".
      Present      : boolean := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean := false;
      -- There is no "dirty" field. This field is ignored and can be
      -- used for anything.
      Ignored_1    : boolean := false;
      -- There is no "huge" or "global" field. This field must be zeroed.
      Zeroed       : num RANGE 0 .. 0 := 0;
      -- Bits that are available for anything.
      Available_1  : num RANGE 0 .. 7 := 0;
      -- A thin pointer to the base address of the directory pointer table
      -- this entry points to. Bits 11 to 0 are zero, as the address
      -- must be 4-KiB aligned regardless of page size. You must do a
      -- right shift of 12.
      Pointer      : num RANGE 0 .. 16#FFFFFFFFFF# := 0;
      -- Bits that are available for anything.
      Available_2  : num RANGE 0 .. 1023;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean := false;
   END RECORD;
   FOR map_entry         USE RECORD
      Present       AT 0 RANGE  0 ..  0;
      Write_Access  AT 0 RANGE  1 ..  1;
      User_Access   AT 0 RANGE  2 ..  2;
      Writethrough  AT 0 RANGE  3 ..  3;
      Uncacheable   AT 0 RANGE  4 ..  4;
      Accessed      AT 0 RANGE  5 ..  5;
      Ignored_1     AT 0 RANGE  6 ..  6;
      Zeroed        AT 0 RANGE  7 ..  8;
      Available_1   AT 0 RANGE  9 .. 11;
      Pointer       AT 0 RANGE 12 .. 51;
      Available_2   AT 0 RANGE 52 .. 62;
      NX            AT 0 RANGE 63 .. 63;
   END RECORD;

   -- This record contains the structure for a directory pointer table entry.
   TYPE directory_entry IS RECORD
      -- Whether the directory is "active".
      Present      : boolean := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean := false;
      -- There is no "dirty" field. This field is ignored and can be
      -- used for anything.
      Ignored_1    : boolean := false;
      -- Indicates whether or not the page entry is for a huge or giant page.
      -- If the entry is for a directory pointer table, then it is for a
      -- giant page. If the entry is for a directory table, then it is for a
      -- huge page. If this is true in both cases, then the "Pointer" field
      -- is actually a physical frame. It is expected that the lower bits are
      -- zeroed, as the frame address should be aligned to its respective
      -- alignment. It is recommended not to use the more specialised fields
      -- before this if you are going to use a larger than normal page size.
      Huge         : boolean := false;
      -- There is no "global" field. This is another ignored set of bits.
      Ignored_2    : boolean := false;
      -- Bits that are available for anything.
      Available_1  : num RANGE 0 .. 7 := 0;
      -- A thin pointer to the base address of the page table this
      -- entry points to. Bits 11 to 0 are zero, as the address
      -- must be 4-KiB aligned regardless of page size. You must do a
      -- right shift of 12.
      Pointer      : num RANGE 0 .. 16#FFFFFFFFFF# := 0;
      -- Bits that are available for anything.
      Available_2  : num RANGE 0 .. 1023;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean := false;
   END RECORD;
   FOR directory_entry   USE RECORD
      Present       AT 0 RANGE  0 ..  0;
      Write_Access  AT 0 RANGE  1 ..  1;
      User_Access   AT 0 RANGE  2 ..  2;
      Writethrough  AT 0 RANGE  3 ..  3;
      Uncacheable   AT 0 RANGE  4 ..  4;
      Accessed      AT 0 RANGE  5 ..  5;
      Ignored_1     AT 0 RANGE  6 ..  6;
      Huge          AT 0 RANGE  7 ..  7;
      Ignored_2     AT 0 RANGE  8 ..  8;
      Available_1   AT 0 RANGE  9 .. 11;
      Pointer       AT 0 RANGE 12 .. 51;
      Available_2   AT 0 RANGE 52 .. 62;
      NX            AT 0 RANGE 63 .. 63;
   END RECORD;

   -- Page directory pointer entries are the same as page directory entries
   -- when using 4-KiB page sizes.
   TYPE directory_pointer_entry IS NEW directory_entry;

   -- This record contains the structure for a page table entry.
   TYPE page_entry IS RECORD
      -- Whether the directory is "active".
      Present      : boolean := false;
      -- When true, both reading and writing is allowed.
      Write_Access : boolean := false;
      -- If this is set, ring 3 can access the page. Otherwise, only
      -- rings 0/1/2 can access it.
      User_Access  : boolean := false;
      -- If true, then this sets the caching policy to writethrough instead
      -- of writeback.
      Writethrough : boolean := false;
      -- True if the page entry this directory entry points to isn't cacheable.
      Uncacheable  : boolean := false;
      -- Whether the entry has been accessed. This is never reset to false by
      -- the CPU, it's your role to track it.
      Accessed     : boolean := false;
      -- True if the page has had physical data written to it. This again
      -- is never set to false by the processor, our code must reset it.
      Dirty        : boolean := false;
      -- TODO: This bit (also known as the PAT bit) does something with
      -- the other page attributes ("Writethrough" and "Uncacheable").
      -- Needs to be supported by the CPU. I need to learn more about
      -- this and the named "PAT register".
      Attribute    : boolean := false;
      -- Whether the physical page is global. When true, the TLB entry
      -- will not be invalidated if CR3 is changed e.g. `MOV CR*, *`.
      -- I believe the purpose of this is to help performance when some
      -- things in the directory don't change between paging structure
      -- switches for different processes etc. Only changeable for the
      -- lowest paging level, otherwise it's ignored and should be false.
      Global       : boolean := false;
      -- Bits that are available for anything.
      Available_1  : num RANGE 0 .. 7;
      -- Points to the physical frame that is 4-KiB aligned. Must be shifted
      -- to the right by 12, much like the page structure pointers.
      Frame        : num RANGE 0 .. 16#FFFFFFFFFF# := 0;
      -- Bits that are available for anything.
      Available_2  : num RANGE 0 .. 1023;
      -- The NX (non-executable) bit. Self-explanatory.
      NX           : boolean := false;
   END RECORD;
   FOR page_entry        USE RECORD
      Present       AT 0 RANGE  0 ..  0;
      Write_Access  AT 0 RANGE  1 ..  1;
      User_Access   AT 0 RANGE  2 ..  2;
      Writethrough  AT 0 RANGE  3 ..  3;
      Uncacheable   AT 0 RANGE  4 ..  4;
      Accessed      AT 0 RANGE  5 ..  5;
      Dirty         AT 0 RANGE  6 ..  6;
      Attribute     AT 0 RANGE  7 ..  7;
      Global        AT 0 RANGE  8 ..  8;
      Available_1   AT 0 RANGE  9 .. 11;
      Frame         AT 0 RANGE 12 .. 51;
      Available_2   AT 0 RANGE 52 .. 62;
      NX            AT 0 RANGE 63 .. 63;
   END RECORD;

   -- The range of entries in any type of directory. I've started it from
   -- zero, so that way, converting a virtual address to a physical address
   -- requires less work, both technically and mentally.
   SUBTYPE page_mask IS num RANGE 0 .. 511;

   -- The page structures below are of limited sizes. They cannot represent
   -- every possible virtual address, so use them sparingly.

   -- Level 4 structure. 1 page map table.
   TYPE map_table
      IS ARRAY(page_mask)            OF map_entry
   WITH
      Component_Size => 64,
      Alignment      => 16#1000#;

   -- Level 3 structure. 512 page directory pointer tables.
   TYPE pointer_tables
      IS ARRAY(page_mask, page_mask) OF directory_pointer_entry
   WITH
      Component_Size => 64,
      Alignment      => 16#1000#;

   -- Level 2 structure. 512 page directory tables.
   TYPE directory_tables
      IS ARRAY(page_mask, page_mask) OF directory_entry
   WITH
      Component_Size => 64,
      Alignment      => 16#1000#;

   -- Level 1 structure. 512 page tables.
   TYPE page_tables
      IS ARRAY(page_mask, page_mask) OF page_entry
   WITH
      Component_Size => 64,
      Alignment      => 16#1000#;

   -- The default page layout class. Can be freely expanded if needed.
   TYPE page_layout IS TAGGED RECORD
      L4        : map_table;
      L3        : pointer_tables;
      L2        : directory_tables;
      L1        : page_tables;
   END RECORD
   WITH
      Pack   => false; -- Respect the alignments.

   -- Takes in an address and aligns it to a 4 KiB boundary by default.
   -- Note that it rounds down, not up, if no second argument is provided.
   FUNCTION Align(
      Address   : IN num;
      Alignment : IN page_frame_variant :=  page;
      Round_Up  : IN boolean            := false)
   RETURN num
   WITH
      Inline => true,
      Post   => Align'result MOD Alignment'enum_rep = 0;

   -- Moves the base 4-KiB aligned address of the directory map table
   -- to the CR3 register to switch virtual address mappings.
   PROCEDURE Load(
      Object    : IN page_layout);

   -- Converts a size in bytes to an amount of certain physical page frames.
   -- Purposefully does not take in zero bytes or else a condition fails.
   FUNCTION Size_To_Pages(
      Size      : IN num;
      Alignment : IN page_frame_variant := page)
   RETURN num
   WITH
      Inline => true,
      Pre    => Size > 0,
      -- Always return at least one page for obvious reasons.
      Post   => Size_To_Pages'result > 0;

   -- Maps a virtual address to a physical address, huge entry.
   -- Requires all addresses passed to it as 2-MiB aligned.
   -- Note that it supports a very limited amount of pages to reduce ELF size.
   -- Try using the larger page sizes to avoid oddities.
   PROCEDURE Map_Address(
      Object           : IN OUT page_layout;
      Virtual_Address  : IN num;
      Physical_Address : IN num;
      Page_Size        : IN page_frame_variant :=  page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      NX               : IN boolean            :=  true)
   WITH
      Pre    => Virtual_Address  = Align( Virtual_Address, Page_Size) AND THEN
                Physical_Address = Align(Physical_Address, Page_Size);

   -- Shortcut procedure for identity mapping a virtual address range to a
   -- physical address range. The range is determined by the size, which
   -- is then converted into 2-MiB physical pages. Requires all addresses
   -- passed to it as 2-MiB aligned. The range is linear. See the
   -- `Map_Address` procedure for more details.
   PROCEDURE Map_Address_Range(
      Object           : IN OUT page_layout;
      Virtual_Address  : IN num;
      Physical_Address : IN num;
      Size             : IN num;
      Page_Size        : IN page_frame_variant :=  page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      NX               : IN boolean            :=  true)
   WITH
      Pre    => Virtual_Address  = Align( Virtual_Address, Page_Size) AND THEN
                Physical_Address = Align(Physical_Address, Page_Size),
      Inline => true;

   PROCEDURE Page_Fault_Handler(
      Error_Code       : IN num)
   WITH
      Pre    => Error_Code <= 16#FFFF_FFFF#, -- Error codes are 32-bits.
      Inline => true;

   -- TODO: Can I avoid these "Symbol_*" variables?
   Symbol_Kernel_Base          : CONSTANT System.Address
   WITH
      Import        => true,
      External_Name => "kernel_base";

   Symbol_Kernel_End           : CONSTANT System.Address
   WITH
      Import        => true,
      External_Name => "kernel_end";

   Symbol_Kernel_Virtual_Base  : CONSTANT System.Address
   WITH
      Import        => true,
      External_Name => "kernel_virtual_base";

   Symbol_Kernel_Physical_Base : CONSTANT System.Address
   WITH
      Import        => true,
      External_Name => "kernel_physical_base";

   Kernel_Base                 : CONSTANT num :=
      Address_To_num(Symbol_Kernel_Base'address);
   Kernel_End                  : CONSTANT num :=
      Address_To_num(Symbol_Kernel_End'address);
   Kernel_Virtual_Base         : CONSTANT num :=
      Address_To_num(Symbol_Kernel_Virtual_Base'address);
   Kernel_Physical_Base        : CONSTANT num :=
      Address_To_num(Symbol_Kernel_Physical_Base'address);
   Kernel_Size                 : CONSTANT num :=
      Kernel_End - Kernel_Base;
END HAVK_Kernel.Paging;
