-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-paging.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Memory;
USE
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Memory;

PACKAGE BODY HAVK_Kernel.Paging
IS
   PROCEDURE Map_Address
     (Object           : IN OUT page_layout;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
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
      FUNCTION To_Pointer
        (Address : IN number)
         RETURN access_generic_table
      WITH
         Import     => true,
         Convention => Intrinsic;

      -- Does the opposite of the above generic function.
      GENERIC
         TYPE generic_table        IS PRIVATE;
         TYPE access_generic_table IS ACCESS generic_table;
      FUNCTION To_Address
        (Pointer : IN access_generic_table)
         RETURN number
      WITH
         Import     => true,
         Convention => Intrinsic;

      FUNCTION Table_Access  IS NEW To_Pointer
               (generic_table =>        directory_pointer_table,
         access_generic_table => access_directory_pointer_table);

      FUNCTION Table_Pointer IS NEW To_Address
               (generic_table =>        directory_pointer_table,
         access_generic_table => access_directory_pointer_table);

      FUNCTION Table_Access  IS NEW To_Pointer
               (generic_table =>        directory_table,
         access_generic_table => access_directory_table);

      FUNCTION Table_Pointer IS NEW To_Address
               (generic_table =>        directory_table,
         access_generic_table => access_directory_table);

      FUNCTION Table_Access  IS NEW To_Pointer
               (generic_table =>        page_table,
         access_generic_table => access_page_table);

      FUNCTION Table_Pointer IS NEW To_Address
               (generic_table =>        page_table,
         access_generic_table => access_page_table);

      FUNCTION Encode_Table
        (Value        : IN number;
         Virtual_Base : IN address := Kernel_Virtual_Base)
         RETURN number
      IS
        (Shift_Right(Value - number(Virtual_Base), 12) AND 2**40 - 1)
      WITH
         Inline => true,
         Post   => Encode_Table'result <= 2**40 - 1;

      FUNCTION Decode_Table
        (Value        : IN number;
         Virtual_Base : IN address := Kernel_Virtual_Base)
         RETURN number
      IS
        (Shift_Left(Value, 12) + number(Virtual_Base))
      WITH
         Inline => true;

      -- The aligned (virtual) address to be mapped.
      Mapping   : CONSTANT number := Align(number(Virtual_Address), Page_Size);

      -- Bits 47 to 39. Directory map index.
      L4_Offset : CONSTANT page_mask := Shift_Right(Mapping, 39)
         AND page_mask'last;

      -- Bits 38 to 30. Directory pointer table index.
      L3_Offset : CONSTANT page_mask := Shift_Right(Mapping, 30)
         AND page_mask'last;

      -- Bits 29 to 21. Directory table index.
      L2_Offset : CONSTANT page_mask := Shift_Right(Mapping, 21)
         AND page_mask'last;

      -- Bits 20 to 12. Page table index.
      L1_Offset : CONSTANT page_mask := Shift_Right(Mapping, 12)
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
      L0 : CONSTANT number RANGE 0 .. 16#FFFFFFFFFF# :=
         Encode_Table(Align(number(Physical_Address), Page_Size),
                         Virtual_Base => address(0));
   BEGIN
      -- TODO: Find a way to model the CPU doing a page-walk (if that is even
      -- possible without library level static page structures).
      -- For now, these just silence the warnings.
      PRAGMA Warnings(GNATprove, off, "unused assignment",
         Reason => "GNATprove is unaware of CPU page walks.");
      PRAGMA Warnings(GNATprove, off, "statement has no effect",
         Reason => "GNATprove is unaware of CPU page walks.");

      -- TODO: Account for CPU vulnerabilities by removing the pointer or frame
      -- address when a page structure is marked as not present. Set to null.
      -- This shouldn't be necessary for code in ring 0 unless something has
      -- gone truly wrong, but still.

      IF
         Object.L4(L4_Offset).Pointer = 0
      THEN
         L3 := NEW directory_pointer_table;
      ELSE
         L3 := Table_Access(Decode_Table(Object.L4(L4_Offset).Pointer));
      END IF;

      IF
         L3 = NULL
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Heap has been exhausted; can't create a " &
            "page directory pointer table for a page mapping.";
         PRAGMA Annotate(GNATprove, False_Positive,
            "exception might be raised",
            "The pointer arithmetic is fine. Heap issues crash externally.");
      END IF;

      IF
         Page_Size /= Giant_Page
      THEN
         IF
            L3(L3_Offset).Pointer = 0 OR ELSE L3(L3_Offset).Huge
         THEN
            L2 := NEW directory_table;
         ELSE
            L2 := Table_Access(Decode_Table(L3(L3_Offset).Pointer));
         END IF;
      END IF;

      IF
         Page_Size /= Giant_Page AND THEN L2 = NULL
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Heap has been exhausted; can't create a " &
            "page directory table for a 2-MiB or 4-KiB page mapping.";
         PRAGMA Annotate(GNATprove, False_Positive,
            "exception might be raised",
            "The pointer arithmetic is fine. Heap issues crash externally.");
      END IF;

      IF
         Page_Size = Page
      THEN
         IF
            L2(L2_Offset).Pointer = 0 OR ELSE L2(L2_Offset).Huge
         THEN
            L1 := NEW page_table;
         ELSE
            L1 := Table_Access(Decode_Table(L2(L2_Offset).Pointer));
         END IF;
      END IF;

      IF
         Page_Size = Page AND THEN L1 = NULL
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Heap has been exhausted; can't create a " &
            "page table for a 4-KiB page mapping.";
         PRAGMA Annotate(GNATprove, False_Positive,
            "exception might be raised",
            "The pointer arithmetic is fine. Heap issues crash externally.");
      END IF;

      Object.L4(L4_Offset).Pointer      := Encode_Table(Table_Pointer(L3));
      Object.L4(L4_Offset).Present      := true;
      Object.L4(L4_Offset).Write_Access := true;
      Object.L4(L4_Offset).User_Access  := true;
      Object.L4(L4_Offset).NX           := false;

      IF -- Handle the mapping in giant pages and return.
         Page_Size = Giant_Page
      THEN
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

      IF -- Handle the mapping in huge pages and return.
         Page_Size = Huge_Page
      THEN
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
      Object           : IN OUT page_layout;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
   IS
      -- Start from zero first so we can efficiently map the base addresses.
      Pages            : CONSTANT number := Size_To_Pages(Size, Page_Size) - 1;
   BEGIN
      FOR
         P IN 0 .. Pages
      LOOP
         Object.Map_Address(
            Virtual_Address   + address(Page_Size * P),
            Physical_Address  + address(Page_Size * P),
            Page_Size        =>     Page_Size,
            Present          =>       Present,
            Write_Access     =>  Write_Access,
            User_Access      =>   User_Access,
            No_Execution     =>  No_Execution);
      END LOOP;
   END Map_Address_Range;

   -- Took this function's equation from the UEFI macro `EFI_SIZE_TO_PAGES()`.
   FUNCTION Size_To_Pages
     (Size         : IN number;
      Alignment    : IN page_frame_variant := Page)
      RETURN number
   IS
      Aligned_Size : CONSTANT number       :=
        Memory.Align(Size, Alignment, Round_Up => true);
   BEGIN
      CASE -- Read the manuals to see which lower bits need to be zeroed.
         Alignment
      IS
         WHEN       Page =>
            RETURN Shift_Right(Aligned_Size, 12);
         WHEN  Huge_Page =>
            RETURN Shift_Right(Aligned_Size, 21);
         WHEN Giant_Page =>
            RETURN Shift_Right(Aligned_Size, 30);
         WHEN OTHERS     =>
            RAISE Panic
            WITH
               Source_Location & " - Unsupported page frame size.";
      END CASE;
   END Size_To_Pages;

   PROCEDURE Load
     (Object : IN page_layout)
   WITH
      SPARK_Mode => off -- Address attributes are used.
   IS
      PROCEDURE Write_CR3
        (L4_Address : IN address)
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__load_page_structure";
   BEGIN
      Write_CR3(Object.L4'address - Kernel_Virtual_Base);
   END Load;

   PROCEDURE Page_Fault_Handler
     (Error_Code    : IN number)
   IS
      FUNCTION Read_CR2
         RETURN address
      WITH
         Global            => NULL,
         Volatile_Function => true,
         Import            => true,
         Convention        => Assembler,
         External_Name     => "assembly__get_page_fault_address";

      -- The fault address is always in the CR2 register, which we presume is
      -- loaded already, as this should be called from ISR 14's handler.
      Fault_Address : CONSTANT address := Read_CR2;

      -- The below conditionals describe why the page fault was raised.
      -- READ: https://wiki.osdev.org/Exceptions#Page_Fault

      Present_Field : CONSTANT string :=
      (
         IF
            Bit_Test(Error_Code, 0)
         THEN
            "Page-protection violation, "
         ELSE
            "Page not present, "
      );

      Write_Field   : CONSTANT string :=
      (
         IF
            Bit_Test(Error_Code, 1)
         THEN
            "occurred during write."
         ELSE
            "occurred during read."
      );

      -- TODO: Add more to describe the page fault. I've only covered 2 fields.
   BEGIN
      Log("ISR 14: Page fault triggered - Error code:"     &
         number'image(Error_Code) & " - Fault address: 0x" &
         Hex_Image(Fault_Address) & " - " & Present_Field  &
         Write_Field, warning);

      RAISE Panic
      WITH
         Source_Location &
         " - Unexpected page fault as of this stage in development.";
      PRAGMA Annotate(GNATprove, Intentional,
         "exception might be raised",
         "We don't handle page faults as of now. We will later.");
   END Page_Fault_Handler;
END HAVK_Kernel.Paging;
