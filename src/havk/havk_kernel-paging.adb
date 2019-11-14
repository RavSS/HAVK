WITH
   System.Machine_Code,
   System.Address_Image,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Debug,
   HAVK_Kernel.Exceptions;
USE
   System.Machine_Code,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Exceptions;

PACKAGE BODY HAVK_Kernel.Paging
IS
   FUNCTION Align(
      Address   : IN num;
      Alignment : IN page_frame_variant :=  page;
      Round_Up  : IN boolean            := false)
   RETURN num     IS
      Remnant   : CONSTANT num := Address MOD Alignment'enum_rep;
   BEGIN
      IF Remnant /= 0 THEN
         RETURN (IF Round_Up THEN Address + Remnant ELSE Address - Remnant);
      ELSE
         RETURN Address;
      END IF;
   END Align;

   PROCEDURE Map_Address(
      Object           : IN OUT page_layout;
      Virtual_Address  : IN num;
      Physical_Address : IN num;
      Page_Size        : IN page_frame_variant :=  page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      NX               : IN boolean            :=  true)
   IS
      -- TODO: Maybe split this up into separate functions and/or procedures.
      -- That might be useful for the virtual-address-to-offset parts.

      -- Make sure to see "Figure 5-17. 4-Kbyte Page Translation-Long Mode"
      -- in the AMD64 system programming manual before you touch anything.

      -- Bits 47 to 39. Directory map index.
      L4_Offset        : CONSTANT page_mask := SHR(Virtual_Address, 39)
         AND page_mask'last;

      -- Bits 38 to 30. Directory pointer table index.
      L3_Offset        : CONSTANT page_mask := SHR(Virtual_Address, 30)
         AND page_mask'last;

      -- Bits 29 to 21. Directory table index.
      L2_Offset        : CONSTANT page_mask := SHR(Virtual_Address, 21)
         AND page_mask'last;

      -- Bits 20 to 12. Page table index.
      L1_Offset        : CONSTANT page_mask := SHR(Virtual_Address, 12)
         AND page_mask'last;

      -- Bits 11 to 0 are presumed zero for directory pointer table addresses.
      L3_Pointer       : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         SHR(num(Object.L3(L4_Offset, L3_Offset)'address) -
            Kernel_Virtual_Base, 12)
         AND 16#FFFFFFFFFF#;

      -- Bits 11 to 0 are presumed zero for directory table addresses.
      L2_Pointer       : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         SHR(num(Object.L2(L3_Offset, L2_Offset)'address) -
            Kernel_Virtual_Base, 12)
         AND 16#FFFFFFFFFF#;

      -- Bits 11 to 0 are presumed zero for page table addresses.
      L1_Pointer       : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         SHR(num(Object.L1(L2_Offset, L1_Offset)'address) -
            Kernel_Virtual_Base, 12)
         AND 16#FFFFFFFFFF#;

      -- Bits 11 to 0 are presumed zero for physical page frame addresses
      -- when the page size is the standard 4 KiB.
      L0_Frame         : CONSTANT num RANGE 0 .. 16#FFFFFFFFFF# :=
         SHR(Physical_Address,   12)
         AND 16#FFFFFFFFFF#;
   BEGIN
      Object.L4(L4_Offset).Pointer                 := L3_Pointer;
      Object.L4(L4_Offset).Present                 := Present;
      Object.L4(L4_Offset).Write_Access            := Write_Access;
      Object.L4(L4_Offset).User_Access             := User_Access;
      Object.L4(L4_Offset).NX                      := NX;

      Object.L3(L4_Offset, L3_Offset).Present      := Present;
      Object.L3(L4_Offset, L3_Offset).Write_Access := Write_Access;
      Object.L3(L4_Offset, L3_Offset).User_Access  := User_Access;
      Object.L3(L4_Offset, L3_Offset).NX           := NX;

      IF Page_Size /= giant_page THEN
         Object.L3(L4_Offset, L3_Offset).Pointer   := L2_Pointer;
      ELSE
         -- Bits 20 to 0 are presumed zero for physical page frame addresses
         -- when the page size is 2 MiB.
         Object.L3(L4_Offset, L3_Offset).Pointer   := L0_Frame;
         -- Just pretend that this record field is named "Giant" instead.
         Object.L3(L4_Offset, L3_Offset).Huge      := true;
         RETURN;
      END IF;

      Object.L2(L3_Offset, L2_Offset).Present      := Present;
      Object.L2(L3_Offset, L2_Offset).Write_Access := Write_Access;
      Object.L2(L3_Offset, L2_Offset).User_Access  := User_Access;
      Object.L2(L3_Offset, L2_Offset).NX           := NX;

      IF Page_Size /= huge_page THEN
         Object.L2(L3_Offset, L2_Offset).Pointer   := L1_Pointer;
      ELSE
         -- Bits 29 to 0 are presumed zero for physical page frame addresses
         -- when the page size is 1 GiB.
         Object.L2(L3_Offset, L2_Offset).Pointer   := L0_Frame;
         Object.L2(L3_Offset, L2_Offset).Huge      := true;
         RETURN;
      END IF;

      Object.L1(L2_Offset, L1_Offset).Frame        := L0_Frame;
      Object.L1(L2_Offset, L1_Offset).Present      := Present;
      Object.L1(L2_Offset, L1_Offset).Write_Access := Write_Access;
      Object.L1(L2_Offset, L1_Offset).User_Access  := User_Access;
      Object.L1(L2_Offset, L1_Offset).NX           := NX;
   END Map_Address;

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
   IS
      -- Start from zero first so we can efficiently map the base addresses.
      Pages            : CONSTANT num := Size_To_Pages(Size, Page_Size) - 1;
   BEGIN
      FOR P IN 0 .. Pages LOOP
         Object.Map_Address(
            Virtual_Address  + Page_Size'enum_rep * P,
            Physical_Address + Page_Size'enum_rep * P,
            Page_Size    =>    Page_Size,
            Present      =>      Present,
            Write_Access => Write_Access,
            User_Access  =>  User_Access,
            NX           =>          NX);
      END LOOP;
   END Map_Address_Range;

   -- Took this function's equation from the UEFI macro `EFI_SIZE_TO_PAGES()`.
   -- Not sure if I have adapted it properly for the larger page sizes.
   FUNCTION Size_To_Pages(
      Size          : IN num;
      Alignment     : IN page_frame_variant := page)
   RETURN num         IS
      Extra_Page    : CONSTANT num := Size AND Alignment'enum_rep;
      Pages         : num;
   BEGIN
      -- Read the manuals to see which lower bits need to be zeroed.
      CASE Alignment IS
         WHEN       page => Pages := SHR(Size, 12);
         WHEN  huge_page => Pages := SHR(Size, 21);
         WHEN giant_page => Pages := SHR(Size, 30);
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
      Object        : IN page_layout)
   IS
      Structure     : CONSTANT System.Address := Object.L4'address -
         System'To_Address(Kernel_Virtual_Base);
   BEGIN
      Asm(
         "MOV CR3, %0;",
         Inputs   => System.Address'asm_input("r", Structure),
         Clobber  => "memory",
         Volatile => true);
   END Load;

   PROCEDURE Page_Fault_Handler(
      Error_Code    : IN num)
   IS
      Fault_Address : System.Address;

      -- The below conditionals describe why the page fault was raised.
      -- READ: https://wiki.osdev.org/Exceptions#Page_Fault

      Present_Field : CONSTANT string :=
      (
         IF BT(Error_Code, 0) THEN
            "Page-protection violation, "
         ELSE
            "Page not present, "
      );

      Write_Field   : CONSTANT string :=
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

      PRAGMA Debug(Debug_Message(
         "ISR 14: Page fault triggered - Error code:" & Error_Code'img &
         " - Fault address: 0x" & System.Address_Image(Fault_Address)  &
         " - " & Present_Field & Write_Field));

      PRAGMA Debug(Tears_In_Rain(
         "Unexpected page fault as of this stage in development - " &
         Debug.File, Debug.Line));
   END Page_Fault_Handler;
END HAVK_Kernel.Paging;
