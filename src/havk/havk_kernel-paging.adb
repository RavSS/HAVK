WITH
   System.Machine_Code,
   System.Address_Image,
   HAVK_Kernel.Intrinsics;
USE
   System.Machine_Code,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Paging IS
   FUNCTION Align_Huge(
      Address   : IN num;
      Round_Up  : IN boolean := false)
   RETURN num     IS
      Remnant   : CONSTANT num := Address MOD 16#200000#; -- 2 MiB.
   BEGIN
      IF Remnant /= 0 THEN
         RETURN (IF Round_Up THEN Address + Remnant ELSE Address - Remnant);
      ELSE
         RETURN Address;
      END IF;
   END Align_Huge;

   -- TODO: This most certainly is not done correctly, I can feel it.
   PROCEDURE Map_Address(
      Object                : IN OUT page_layout_huge;
      Virtual_Address       : IN num;
      Physical_Address      : IN num;
      Present               : IN boolean :=  true;
      Write_Access          : IN boolean := false;
      User_Access           : IN boolean := false;
      NX                    : IN boolean :=  true)
   IS
      -- TODO: Maybe split this up into separate functions and/or procedures.
      -- That might be useful for the virtual-address-to-offset parts.

      -- Make sure to see "Figure 5-22. 2-Mbyte Page Translation-Long Mode"
      -- in the AMD64 system programming manual before you touch anything.

      -- Bits 21 to 29. Directory table index.
      Level_1_Offset        : CONSTANT directory_entry_range :=
         SHR(Virtual_Address, 21)  AND directory_entry_range'last;
      -- Bits 30 to 38. Directory pointer table index.
      Level_2_Offset        : CONSTANT directory_entry_range :=
         SHR(Virtual_Address, 30)  AND directory_entry_range'last;
      -- Bits 39 to 47. Directory map table index.
      Level_3_Offset        : CONSTANT directory_entry_range :=
         SHR(Virtual_Address, 39)  AND directory_entry_range'last;

      -- Bits 0 to 20 are presumed zero for 2 MiB physical pages.
      Level_1_Address_Shift : CONSTANT num := 21;
      -- Bits 0 to 11 are presumed zero for directory table addresses.
      Level_2_Address_Shift : CONSTANT num := 12;
      -- Bits 0 to 11 are presumed zero for directory pointer table addresses.
      Level_3_Address_Shift : CONSTANT num := 12;
   BEGIN
      -- By default, mark the entries as present.
      Object.Level_3
         (Level_3_Offset).Present                      := Present;
      Object.Level_2
         (Level_3_Offset)(Level_2_Offset).Present      := Present;
      Object.Level_1
         (Level_2_Offset)(Level_1_Offset).Present      := Present;

      -- By default, don't give the new mappings write access.
      Object.Level_3
         (Level_3_Offset).Write_Access                 := Write_Access;
      Object.Level_2
         (Level_3_Offset)(Level_2_Offset).Write_Access := Write_Access;
      Object.Level_1
         (Level_2_Offset)(Level_1_Offset).Write_Access := Write_Access;

      -- By default, only let ring 0 access the memory.
      Object.Level_3
         (Level_3_Offset).User_Access                  := User_Access;
      Object.Level_2
         (Level_3_Offset)(Level_2_Offset).User_Access  := User_Access;
      Object.Level_1
         (Level_2_Offset)(Level_1_Offset).User_Access  := User_Access;

      -- By default, don't let the memory at the addresses be executable.
      Object.Level_3
         (Level_3_Offset).NX                           := NX;
      Object.Level_2
         (Level_3_Offset)(Level_2_Offset).NX           := NX;
      Object.Level_1
         (Level_2_Offset)(Level_1_Offset).NX           := NX;

      -- Calculate the directory pointer table's base address.
      Object.Level_3(Level_3_Offset).Directory_Pointer :=
         SHR(Address_To_num(
            Object.Level_2(Level_3_Offset)(Level_2_Offset)'address),
         Level_3_Address_Shift);

      -- Calculate the directory table's base address.
      Object.Level_2(Level_3_Offset)(Level_2_Offset).Directory_Address :=
         SHR(Address_To_num(
            Object.Level_1(Level_2_Offset)(Level_1_Offset)'address),
         Level_2_Address_Shift);

      -- Calculate the physical page's base address.
      Object.Level_1(Level_2_Offset)(Level_1_Offset).Physical_Address  :=
         SHR(Physical_Address, Level_1_Address_Shift);

      -- PRAGMA Debug(Debug_Message("Page address mapping offsets 3 2 1:" &
         -- Level_3_Offset'img & Level_2_Offset'img & Level_1_Offset'img));
   END Map_Address;

   PROCEDURE Map_Address_Range(
      Object            : IN OUT page_layout_huge;
      Virtual_Address   : IN num;
      Physical_Address  : IN num;
      Size              : IN num;
      Present           : IN boolean :=  true;
      Write_Access      : IN boolean := false;
      User_Access       : IN boolean := false;
      NX                : IN boolean :=  true)
   IS
      Pages             : CONSTANT num := Size_To_Pages_Huge(Size);
   BEGIN
      FOR P IN 0 .. Pages LOOP
         Object.Map_Address(
            Virtual_Address  + 16#200000# * P,
            Physical_Address + 16#200000# * P,
            Present         => Present,
            Write_Access    => Write_Access,
            User_Access     => User_Access,
            NX              => NX);
      END LOOP;
   END Map_Address_Range;

   -- Took this function's equation from the UEFI macro `EFI_SIZE_TO_PAGES()`.
   FUNCTION Size_To_Pages_Huge(
      Size        : IN num)
   RETURN num       IS
      -- Remember that bits 0 to 20 are zero for 2 MiB pages.
      Huge_Pages  : CONSTANT num := SHR(Size, 21);
      Extra_Page  : CONSTANT num := Size AND directory_entry_range'last;
   BEGIN
      IF Extra_Page /= 0 THEN
         RETURN Huge_Pages + 1;
      ELSE
         -- If the size is even a single byte, then it still needs one page.
         RETURN (IF Huge_Pages /= 0 THEN Huge_Pages ELSE 1);
      END IF;
   END Size_To_Pages_Huge;

   PROCEDURE Load(
      Object      : IN page_layout_huge)
   IS
   BEGIN
      Asm(
         "MOV CR3, %0;",
         Inputs   => System.Address'asm_input("r", Object.Level_3'address),
         Volatile => true);
   END Load;

   PROCEDURE Page_Fault_Handler(
      Error_Code       : IN num)
   IS
      Fault_Address    : System.Address;

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
            "occured during write."
         ELSE
            "occured during read."
      );

      -- TODO: Add more to describe the page fault. I've only covered 2 fields.
   BEGIN
      -- The fault address is always in the CR2 register, which we presume is
      -- loaded already, as this should be called from ISR 14's handler.
      Asm(
         "MOV %0, CR2;",
         Outputs  => System.Address'asm_output("+r", Fault_Address),
         Volatile => true);

      PRAGMA Debug(Debug_Message(
         "ISR 14: Page fault triggered - Error code:" & Error_Code'img &
         " - Fault address: 0x" & System.Address_Image(Fault_Address)  &
         " - " & Present_Field & Write_Field));
   END Page_Fault_Handler;
END HAVK_Kernel.Paging;