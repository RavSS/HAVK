WITH
   System.Machine_Code,
   System.Address_Image,
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
   FUNCTION Get_Level_Pointer(
      Object   : IN page_layout;
      Level    : IN num;
      Offset_1 : IN page_mask;
      Offset_2 : IN page_mask)
   RETURN num IS
   (  -- Bits 11 to 0 are zero for page structure level addresses regardless
      -- of page size. The rest of the value should have its least
      -- significant size zeroed according to a precondition contract.
      IF    Level = 1 THEN
         SHR(Object.L1(Offset_1, Offset_2)'address - Kernel_Virtual_Base, 12)
            AND 16#FFFFFFFFFF#
      ELSIF Level = 2 THEN
         SHR(Object.L2(Offset_1, Offset_2)'address - Kernel_Virtual_Base, 12)
            AND 16#FFFFFFFFFF#
      ELSIF Level = 3 THEN
         SHR(Object.L3(Offset_1, Offset_2)'address - Kernel_Virtual_Base, 12)
            AND 16#FFFFFFFFFF#
      ELSE 0 -- Pointer to null address on precondition failure.
   )
   WITH
      SPARK_Mode => off; -- Address attributes used in pointer calculations.

   PROCEDURE Map_Address(
      Object             : IN OUT page_layout;
      Virtual_Address    : IN num;
      Physical_Address   : IN num;
      Page_Size          : IN page_frame_variant :=  Page;
      Cascade_Privileges : IN boolean            := false;
      Cascade_Presence   : IN boolean            :=  true;
      Present            : IN boolean            :=  true;
      Write_Access       : IN boolean            := false;
      User_Access        : IN boolean            := false;
      No_Execution       : IN boolean            :=  true)
   IS
      -- TODO: Maybe split this up into separate functions and/or procedures.
      -- That might be useful for the virtual-address-to-offset parts.

      -- Make sure to see "Figure 5-17. 4-Kbyte Page Translation-Long Mode"
      -- in the AMD64 system programming manual before you touch anything.

      -- The aligned (virtual) address to be mapped.
      Address            : CONSTANT num := Align(Virtual_Address, Page_Size);

      -- Bits 47 to 39. Directory map index.
      L4_Offset          : CONSTANT page_mask := SHR(Address, 39)
         AND page_mask'last;

      -- Bits 38 to 30. Directory pointer table index.
      L3_Offset          : CONSTANT page_mask := SHR(Address, 30)
         AND page_mask'last;

      -- Bits 29 to 21. Directory table index.
      L2_Offset          : CONSTANT page_mask := SHR(Address, 21)
         AND page_mask'last;

      -- Bits 20 to 12. Page table index.
      L1_Offset          : CONSTANT page_mask := SHR(Address, 12)
         AND page_mask'last;

      -- Points to a page directory pointer table.
      L3_Pointer         : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         Object.Get_Level_Pointer(3, L4_Offset, L3_Offset);

      -- Points to a page directory table.
      L2_Pointer         : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         Object.Get_Level_Pointer(2, L3_Offset, L2_Offset);

      -- Points to a page table.
      L1_Pointer         : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         Object.Get_Level_Pointer(1, L2_Offset, L1_Offset);

      -- Points to a physical frame, regardless of page frame size.
      L0_Frame           : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         SHR(Align(Physical_Address, Page_Size), 12) AND 16#FFFFFFFFFF#;
   BEGIN
      -- TODO: Account for CPU vulnerabilities by removing the pointer or frame
      -- address when a page structure is marked as not present. Set to null.

      -- The page map won't be restricted, but the levels below it will be, as
      -- their permissions are more easily manipulatable via passing different
      -- page sizes. Ring 3 access is still disabled everywhere until change.
      Object.L4(L4_Offset).Pointer                       := L3_Pointer;
      Object.L4(L4_Offset).Present                       := true;
      Object.L4(L4_Offset).Write_Access                  := true;
      Object.L4(L4_Offset).User_Access                   := false;
      Object.L4(L4_Offset).NX                            := false;

      -- Handle the mapping in giant pages.
      IF Page_Size /= Giant_Page THEN
         Object.L3(L4_Offset, L3_Offset).Pointer         := L2_Pointer;
         Object.L3(L4_Offset, L3_Offset).Huge            := false;

         IF Cascade_Privileges THEN
            Object.L3(L4_Offset, L3_Offset).Write_Access := Write_Access;
            Object.L3(L4_Offset, L3_Offset).User_Access  := User_Access;
            Object.L3(L4_Offset, L3_Offset).NX           := No_Execution;
         END IF;

         IF Cascade_Presence THEN
            Object.L3(L4_Offset, L3_Offset).Present      := Present;
         END IF;
      ELSE
         -- Bits 29 to 0 are zero for physical page frame addresses when the
         -- page size is 1 GiB. Just pretend that the "Huge" record field
         -- is named "Giant" instead.
         Object.L3(L4_Offset, L3_Offset).Pointer         := L0_Frame;
         Object.L3(L4_Offset, L3_Offset).Present         := Present;
         Object.L3(L4_Offset, L3_Offset).Huge            := true;
         Object.L3(L4_Offset, L3_Offset).Write_Access    := Write_Access;
         Object.L3(L4_Offset, L3_Offset).User_Access     := User_Access;
         Object.L3(L4_Offset, L3_Offset).NX              := No_Execution;
         RETURN;
      END IF;

      -- Handle the mapping in huge pages.
      IF Page_Size /= Huge_Page THEN
         Object.L2(L3_Offset, L2_Offset).Pointer         := L1_Pointer;
         Object.L2(L3_Offset, L2_Offset).Huge            := false;

         IF Cascade_Privileges THEN
            Object.L2(L3_Offset, L2_Offset).Write_Access := Write_Access;
            Object.L2(L3_Offset, L2_Offset).User_Access  := User_Access;
            Object.L2(L3_Offset, L2_Offset).NX           := No_Execution;
         END IF;

         IF Cascade_Presence THEN
            Object.L2(L3_Offset, L2_Offset).Present      := Present;
         END IF;
      ELSE
         -- Bits 20 to 0 are zero for physical page frame addresses when the
         -- page size is 2 MiB.
         Object.L2(L3_Offset, L2_Offset).Pointer         := L0_Frame;
         Object.L2(L3_Offset, L2_Offset).Present         := Present;
         Object.L2(L3_Offset, L2_Offset).Huge            := true;
         Object.L2(L3_Offset, L2_Offset).Write_Access    := Write_Access;
         Object.L2(L3_Offset, L2_Offset).User_Access     := User_Access;
         Object.L2(L3_Offset, L2_Offset).NX              := No_Execution;
         RETURN;
      END IF;

      -- Handle the mapping in standard pages.
      Object.L1(L2_Offset, L1_Offset).Frame              := L0_Frame;
      Object.L1(L2_Offset, L1_Offset).Present            := Present;
      Object.L1(L2_Offset, L1_Offset).Write_Access       := Write_Access;
      Object.L1(L2_Offset, L1_Offset).User_Access        := User_Access;
      Object.L1(L2_Offset, L1_Offset).NX                 := No_Execution;
   END Map_Address;

   PROCEDURE Map_Address_Range(
      Object             : IN OUT page_layout;
      Virtual_Address    : IN num;
      Physical_Address   : IN num;
      Size               : IN num;
      Page_Size          : IN page_frame_variant :=  Page;
      Cascade_Privileges : IN boolean            := false;
      Cascade_Presence   : IN boolean            :=  true;
      Present            : IN boolean            :=  true;
      Write_Access       : IN boolean            := false;
      User_Access        : IN boolean            := false;
      No_Execution       : IN boolean            :=  true)
   IS
      -- Start from zero first so we can efficiently map the base addresses.
      Pages            : CONSTANT num := Size_To_Pages(Size, Page_Size) - 1;
   BEGIN
      FOR P IN 0 .. Pages LOOP
         Object.Map_Address(
            Virtual_Address     +      Page_Size * P,
            Physical_Address    +      Page_Size * P,
            Page_Size          =>          Page_Size,
            Cascade_Privileges => Cascade_Privileges,
            Cascade_Presence   =>   Cascade_Presence,
            Present            =>            Present,
            Write_Access       =>       Write_Access,
            User_Access        =>        User_Access,
            No_Execution       =>       No_Execution);
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
