-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-paging.adb                                 --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

WITH
   System.Machine_Code,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Debug,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Memory;
USE
   System.Machine_Code,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Memory;

PACKAGE BODY HAVK_Kernel.Paging
IS
   PROCEDURE Map_Address(
      Object             : IN OUT page_layout;
      Virtual_Address    : IN num;
      Physical_Address   : IN num;
      Page_Size          : IN page_frame_variant :=  Page;
      Present            : IN boolean            :=  true;
      Write_Access       : IN boolean            := false;
      User_Access        : IN boolean            := false;
      No_Execution       : IN boolean            :=  true)
   IS
      -- TODO: Maybe split this up into separate functions and/or procedures.
      -- That might be useful for the virtual-address-to-offset parts.

      -- Make sure to see "Figure 5-17. 4-Kbyte Page Translation-Long Mode"
      -- in the AMD64 system programming manual before you touch anything.

      -- This turns an address into a pointer to a table. It's useful for
      -- turning a pointer in a table entry into a different lower level table.
      GENERIC
         TYPE generic_table        IS PRIVATE;
         TYPE access_generic_table IS ACCESS generic_table;
      FUNCTION To_Pointer(
         Address : IN num)
      RETURN access_generic_table
      WITH
         Import     => true,
         Convention => Intrinsic;

      -- Does the opposite of the above generic function.
      GENERIC
         TYPE generic_table        IS PRIVATE;
         TYPE access_generic_table IS ACCESS generic_table;
      FUNCTION To_Address(
         Pointer : IN access_generic_table)
      RETURN num
      WITH
         Import     => true,
         Convention => Intrinsic;

      FUNCTION Table_Access  IS NEW To_Pointer(
                generic_table =>        directory_pointer_table,
         access_generic_table => access_directory_pointer_table);

      FUNCTION Table_Pointer IS NEW To_Address(
                generic_table =>        directory_pointer_table,
         access_generic_table => access_directory_pointer_table);

      FUNCTION Table_Access  IS NEW To_Pointer(
                generic_table =>        directory_table,
         access_generic_table => access_directory_table);

      FUNCTION Table_Pointer IS NEW To_Address(
                generic_table =>        directory_table,
         access_generic_table => access_directory_table);

      FUNCTION Table_Access  IS NEW To_Pointer(
                generic_table =>        page_table,
         access_generic_table => access_page_table);

      FUNCTION Table_Pointer IS NEW To_Address(
                generic_table =>        page_table,
         access_generic_table => access_page_table);

      FUNCTION Encode_Table(
         Value        : IN num;
         Virtual_Base : IN num := Kernel_Virtual_Base)
      RETURN num IS
         (SHR(Value - Virtual_Base, 12) AND 16#FFFFFFFFFF#)
      WITH
         Inline => true,
         Post   => Encode_Table'result <= 16#FFFFFFFFFF#;

      FUNCTION Decode_Table(
         Value        : IN num;
         Virtual_Base : IN num := Kernel_Virtual_Base)
      RETURN num IS
         (SHL(Value, 12) + Virtual_Base)
      WITH
         Inline => true;

      -- The aligned (virtual) address to be mapped.
      Address   : CONSTANT num       := Align(Virtual_Address, Page_Size);

      -- Bits 47 to 39. Directory map index.
      L4_Offset : CONSTANT page_mask := SHR(Address, 39)
         AND page_mask'last;

      -- Bits 38 to 30. Directory pointer table index.
      L3_Offset : CONSTANT page_mask := SHR(Address, 30)
         AND page_mask'last;

      -- Bits 29 to 21. Directory table index.
      L2_Offset : CONSTANT page_mask := SHR(Address, 21)
         AND page_mask'last;

      -- Bits 20 to 12. Page table index.
      L1_Offset : CONSTANT page_mask := SHR(Address, 12)
         AND page_mask'last;

      -- Points to a page directory pointer table on the heap. The directory
      -- pointer table will never truly null, as the page map alone cannot
      -- define physical frames of any size.
      L3 : access_directory_pointer_table;

      -- Points to a page directory table on the heap.
      L2 : access_directory_table;

      -- Points to a page table on the heap.
      L1 : access_page_table;

      -- Points to a physical frame, regardless of page frame size.
      L0 : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         Encode_Table(Align(Physical_Address, Page_Size), Virtual_Base => 0);
   BEGIN
      -- TODO: Account for CPU vulnerabilities by removing the pointer or frame
      -- address when a page structure is marked as not present. Set to null.

      IF Object.L4(L4_Offset).Pointer = 0 THEN
         L3 := NEW directory_pointer_table;
      ELSE
         L3 := Table_Access(Decode_Table(Object.L4(L4_Offset).Pointer));
      END IF;

      IF L3 = NULL THEN
         Exceptions.Tears_In_Rain("Heap has been exhausted; can't create a " &
            "page directory pointer table for a page mapping.",
            Debug.File, Debug.Line);
      END IF;

      IF Page_Size /= Giant_Page THEN
         IF L3(L3_Offset).Pointer = 0 OR ELSE L3(L3_Offset).Huge THEN
            L2 := NEW directory_table;
         ELSE
            L2 := Table_Access(Decode_Table(L3(L3_Offset).Pointer));
         END IF;
      END IF;

      IF Page_Size /= Giant_Page AND THEN L2 = NULL THEN
         Exceptions.Tears_In_Rain("Heap has been exhausted; can't create a " &
            "page directory table for a 2-MiB or 4-KiB page mapping.",
            Debug.File, Debug.Line);
      END IF;

      IF Page_Size = Page THEN
         IF L2(L2_Offset).Pointer = 0 OR ELSE L2(L2_Offset).Huge THEN
            L1 := NEW page_table;
         ELSE
            L1 := Table_Access(Decode_Table(L2(L2_Offset).Pointer));
         END IF;
      END IF;

      IF Page_Size = Page AND THEN L1 = NULL THEN
         Exceptions.Tears_In_Rain("Heap has been exhausted; can't create a " &
            "page table for a 4-KiB page mapping.",
            Debug.File, Debug.Line);
      END IF;

      Object.L4(L4_Offset).Pointer      := Encode_Table(Table_Pointer(L3));
      Object.L4(L4_Offset).Present      := true;
      Object.L4(L4_Offset).Write_Access := true;
      Object.L4(L4_Offset).User_Access  := true;
      Object.L4(L4_Offset).NX           := false;

      -- Handle the mapping in giant pages and return.
      IF Page_Size = Giant_Page THEN
         -- Bits 29 to 0 are zero for physical page frame addresses when the
         -- page size is 1 GiB. Just pretend that the "Huge" record field
         -- is named "Giant" instead.
         L3(L3_Offset).Pointer          := L0;
         L3(L3_Offset).Present          := Present;
         L3(L3_Offset).Huge             := true;
         L3(L3_Offset).Write_Access     := Write_Access;
         L3(L3_Offset).User_Access      := User_Access;
         L3(L3_Offset).NX               := No_Execution;
         RETURN;
      END IF;

      L3(L3_Offset).Pointer             := Encode_Table(Table_Pointer(L2));
      L3(L3_Offset).Present             := true;
      L3(L3_Offset).Huge                := false;
      L3(L3_Offset).Write_Access        := true;
      L3(L3_Offset).User_Access         := true;
      L3(L3_Offset).NX                  := false;

      -- Handle the mapping in huge pages and return.
      IF Page_Size = Huge_Page THEN
         -- Bits 20 to 0 are zero for physical page frame addresses when the
         -- page size is 2 MiB.
         L2(L2_Offset).Pointer          := L0;
         L2(L2_Offset).Present          := Present;
         L2(L2_Offset).Huge             := true;
         L2(L2_Offset).Write_Access     := Write_Access;
         L2(L2_Offset).User_Access      := User_Access;
         L2(L2_Offset).NX               := No_Execution;
         RETURN;
      END IF;

      L2(L2_Offset).Pointer             := Encode_Table(Table_Pointer(L1));
      L2(L2_Offset).Present             := true;
      L2(L2_Offset).Huge                := false;
      L2(L2_Offset).Write_Access        := true;
      L2(L2_Offset).User_Access         := true;
      L2(L2_Offset).NX                  := false;

      -- Handle the mapping in standard pages.
      L1(L1_Offset).Frame               := L0;
      L1(L1_Offset).Present             := Present;
      L1(L1_Offset).Write_Access        := Write_Access;
      L1(L1_Offset).User_Access         := User_Access;
      L1(L1_Offset).NX                  := No_Execution;
   END Map_Address;

   PROCEDURE Map_Address_Range(
      Object             : IN OUT page_layout;
      Virtual_Address    : IN num;
      Physical_Address   : IN num;
      Size               : IN num;
      Page_Size          : IN page_frame_variant :=  Page;
      Present            : IN boolean            :=  true;
      Write_Access       : IN boolean            := false;
      User_Access        : IN boolean            := false;
      No_Execution       : IN boolean            :=  true)
   IS
      -- Start from zero first so we can efficiently map the base addresses.
      Pages              : CONSTANT num := Size_To_Pages(Size, Page_Size) - 1;
   BEGIN
      FOR P IN 0 .. Pages LOOP
         Object.Map_Address(
            Virtual_Address   + Page_Size * P,
            Physical_Address  + Page_Size * P,
            Page_Size        =>     Page_Size,
            Present          =>       Present,
            Write_Access     =>  Write_Access,
            User_Access      =>   User_Access,
            No_Execution     =>  No_Execution);
      END LOOP;
   END Map_Address_Range;

   -- Took this function's equation from the UEFI macro `EFI_SIZE_TO_PAGES()`.
   -- Not sure if I have adapted it properly for the larger page sizes.
   FUNCTION Size_To_Pages(
      Size             : IN num;
      Alignment        : IN page_frame_variant := Page)
   RETURN num            IS
      Extra_Page       : CONSTANT num := Size AND Alignment;
      Pages            : num;
   BEGIN
      -- Read the manuals to see which lower bits need to be zeroed.
      CASE Alignment IS
         WHEN       Page => Pages := SHR(Size, 12);
         WHEN  Huge_Page => Pages := SHR(Size, 21);
         WHEN Giant_Page => Pages := SHR(Size, 30);
         WHEN     OTHERS => Pages := SHR(Size, 12); -- Precondition failure.
      END CASE;

      IF Extra_Page /= 0 THEN
         RETURN Pages + 1;
      ELSE
         -- If the size is even a single byte, then it still needs one page.
         RETURN (IF Pages /= 0 THEN Pages ELSE 1);
      END IF;
   END Size_To_Pages;

   -- Avoid inlining this, or else it makes it harder to pick up on weird
   -- stack smash failure calls.
   PROCEDURE Load(
      Object           : IN page_layout)
   WITH
      SPARK_Mode => off -- Assembly is used to load the page structure.
   IS
      Structure        : CONSTANT System.Address := Object.L4'address -
         System'To_Address(Kernel_Virtual_Base);
   BEGIN
      Asm(
         "MOV CR3, %0;",
         Inputs   => System.Address'asm_input("r", Structure),
         Clobber  => "memory",
         Volatile => true);
   END Load;

   PROCEDURE Page_Fault_Handler(
      Error_Code       : IN num)
   WITH
      SPARK_Mode => off -- Assembly is used to get the page fault address.
   IS
      Fault_Address : System.Address;

      -- The below conditionals describe why the page fault was raised.
      -- READ: https://wiki.osdev.org/Exceptions#Page_Fault

      Present_Field    : CONSTANT string :=
      (
         IF BT(Error_Code, 0) THEN
            "Page-protection violation, "
         ELSE
            "Page not present, "
      );

      Write_Field      : CONSTANT string :=
      (
         IF BT(Error_Code, 1) THEN
            "occurred during write."
         ELSE
            "occurred during read."
      );

      -- TODO: Add more to describe the page fault. I've only covered 2 fields.
   BEGIN
      -- The fault address is always in the CR2 register, which we presume is
      -- loaded already, as this should be called from ISR 14's handler.
      Asm(
         "MOV %0, CR2;",
         Outputs  => System.Address'asm_output("=r", Fault_Address),
         Volatile => true);

      Log(
         "ISR 14: Page fault triggered - Error code:" & Error_Code'img &
         " - Fault address: 0x" & System.Address_Image(Fault_Address)  &
         " - " & Present_Field & Write_Field, warning);

      Tears_In_Rain("Unexpected page fault as of this stage in development",
         Debug.File, Debug.Line);
   END Page_Fault_Handler;
END HAVK_Kernel.Paging;
