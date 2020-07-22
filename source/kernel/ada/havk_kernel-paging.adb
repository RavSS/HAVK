-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-paging.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Memory;

PACKAGE BODY HAVK_Kernel.Paging
WITH
   Refined_State => (MMU_State                => NULL,
                     Kernel_Page_Layout_State => Kernel_Page_Layout)
IS
   FUNCTION Encode_Table
     (Value : IN address)
      RETURN number
   IS
     (number(Shift_Right(Value, 12) AND 2**40 - 1));

   FUNCTION Decode_Table
     (Value : IN number)
      RETURN address
   IS
     (address(Shift_Left(Value, 12)));

   FUNCTION Table_Access  IS NEW To_Pointer
            (generic_table =>        directory_pointer_table,
      access_generic_table => access_directory_pointer_table);

   FUNCTION Table_Address IS NEW To_Address
            (generic_table =>        directory_pointer_table,
      access_generic_table => access_directory_pointer_table);

   FUNCTION Table_Access  IS NEW To_Pointer
            (generic_table =>        directory_table,
      access_generic_table => access_directory_table);

   FUNCTION Table_Address IS NEW To_Address
            (generic_table =>        directory_table,
      access_generic_table => access_directory_table);

   FUNCTION Table_Access  IS NEW To_Pointer
            (generic_table =>        page_table,
      access_generic_table => access_page_table);

   FUNCTION Table_Address IS NEW To_Address
            (generic_table =>        page_table,
      access_generic_table => access_page_table);

   PROCEDURE Map_Address
     (Layout           : IN OUT page_layout;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
   IS
      -- Make sure to see "Figure 5-17. 4-Kbyte Page Translation-Long Mode"
      -- in the AMD64 system programming manual before you touch anything.

      -- Bits 47 to 39. Directory map index.
      L4_Offset : CONSTANT page_mask :=
         number(Shift_Right(Virtual_Address, 39)) AND page_mask'last;

      -- Bits 38 to 30. Directory pointer table index.
      L3_Offset : CONSTANT page_mask :=
         number(Shift_Right(Virtual_Address, 30)) AND page_mask'last;

      -- Bits 29 to 21. Directory table index.
      L2_Offset : CONSTANT page_mask :=
         number(Shift_Right(Virtual_Address, 21)) AND page_mask'last;

      -- Bits 20 to 12. Page table index.
      L1_Offset : CONSTANT page_mask :=
         number(Shift_Right(Virtual_Address, 12)) AND page_mask'last;

      -- Points to a page directory pointer table on the heap. The directory
      -- pointer table will never truly null, as the page map alone cannot
      -- define physical frames of any size.
      L3 : access_directory_pointer_table
      WITH
         Annotate => (GNATprove, Intentional,
                      "memory leak might occur at end of scope",
                      "This is freed manually outside of this scope.");

      -- Points to a page directory table on the heap.
      L2 : access_directory_table
      WITH
         Annotate => (GNATprove, Intentional,
                      "memory leak might occur at end of scope",
                      "This is freed manually outside of this scope.");

      -- Points to a page table on the heap.
      L1 : access_page_table
      WITH
         Annotate => (GNATprove, Intentional,
                      "memory leak might occur at end of scope",
                      "This is freed manually outside of this scope.");

      -- Points to a physical frame, regardless of page frame size.
      L0 : CONSTANT number RANGE 0 .. 2**40 - 1 :=
         Encode_Table(address(Memory.Align(number(Physical_Address),
            Page_Size)));
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
         Layout.L4(L4_Offset).Pointer = 0
      THEN
         L3 := NEW directory_pointer_table;
      ELSE
         L3 := Table_Access(Decode_Table(Layout.L4(L4_Offset).Pointer));
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

      Layout.L4(L4_Offset).Pointer      := Encode_Table(Table_Address(L3));
      Layout.L4(L4_Offset).Present      := true;
      Layout.L4(L4_Offset).Write_Access := true;
      Layout.L4(L4_Offset).User_Access  := true;
      Layout.L4(L4_Offset).NX           := false;

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

      L3(L3_Offset).Pointer             := Encode_Table(Table_Address(L2));
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

      L2(L2_Offset).Pointer             := Encode_Table(Table_Address(L1));
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

   PROCEDURE Map_Address_Range
     (Layout           : IN OUT page_layout;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      User_Access      : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
   IS
      Pages : CONSTANT number := Size_To_Pages(Size, Page_Size);
   BEGIN
      IF -- There will be a wrap-around problem if we're mapping zero pages.
         Pages = 0
      THEN
         RETURN;
      END IF;

      -- Start from zero first so we can efficiently map the base addresses.
      FOR
         Page_Index IN 0 .. Pages - 1
      LOOP
         Map_Address
           (Layout,
            Virtual_Address  + address(Page_Size * Page_Index),
            Physical_Address + address(Page_Size * Page_Index),
            Page_Size       =>     Page_Size,
            Present         =>       Present,
            Write_Access    =>  Write_Access,
            User_Access     =>   User_Access,
            No_Execution    =>  No_Execution);
      END LOOP;
   END Map_Address_Range;

   PROCEDURE Kernel_Map_Address
     (Virtual_Address  : IN address;
      Physical_Address : IN address;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
   IS
   BEGIN
      Map_Address
        (Layout           => Kernel_Page_Layout,
         Virtual_Address  =>    Virtual_Address,
         Physical_Address =>   Physical_Address,
         Page_Size        =>          Page_Size,
         Present          =>            Present,
         Write_Access     =>       Write_Access,
         User_Access      =>              false,
         No_Execution     =>       No_Execution);
   END Kernel_Map_Address;

   PROCEDURE Kernel_Map_Address_Range
     (Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size        : IN page_frame_variant :=  Page;
      Present          : IN boolean            :=  true;
      Write_Access     : IN boolean            := false;
      No_Execution     : IN boolean            :=  true)
   IS
   BEGIN
      Map_Address_Range
        (Layout           => Kernel_Page_Layout,
         Virtual_Address  =>    Virtual_Address,
         Physical_Address =>   Physical_Address,
         Size             =>               Size,
         Page_Size        =>          Page_Size,
         Present          =>            Present,
         Write_Access     =>       Write_Access,
         User_Access      =>              false,
         No_Execution     =>       No_Execution);
   END Kernel_Map_Address_Range;

   PROCEDURE Deallocate_Mappings -- TODO: Maybe split this up.
     (Layout : IN OUT page_layout)
   WITH
      Refined_Post => (FOR ALL L4_Entry OF Layout.L4 =>
                          NOT L4_Entry.Present  AND THEN
                          NOT L4_Entry.Accessed AND THEN
                          L4_Entry.Pointer = 0)
   IS
      -- The L4 is part of the main record. It must be freed separately. The L1
      -- points to physical frames i.e. RAM, so it should not be iterated over.
      Current_L3 : access_directory_pointer_table;
      Current_L2 : access_directory_table
      WITH
         Annotate => (GNATprove, False_Positive, "memory leak *",
                      "This is not possible, as it's decoded and freed.");
      Current_L1 : access_page_table
      WITH
         Annotate => (GNATprove, False_Positive, "memory leak *",
                      "This is not possible, as it's decoded and freed.");
   BEGIN
      FOR
         L4_Entry OF Layout.L4
      LOOP
         Current_L3 := Table_Access(Decode_Table(L4_Entry.Pointer));
         PRAGMA Annotate(GNATprove, False_Positive, "memory leak *",
            "This is not possible, as it's decoded and freed.");

         IF -- If there's a directory pointer table.
            Current_L3 /= NULL
         THEN
            FOR
               L3_Entry OF Current_L3.ALL
            LOOP
               Current_L2 := Table_Access(Decode_Table(L3_Entry.Pointer));
               PRAGMA Annotate(GNATprove, False_Positive, "memory leak *",
                  "This is not possible, as it's decoded and freed.");

               IF -- If there's a directory table.
                  Current_L2 /= NULL
               THEN
                  FOR
                     L2_Entry OF Current_L2.ALL
                  LOOP
                     Current_L1 :=
                        Table_Access(Decode_Table(L2_Entry.Pointer));
                     PRAGMA Annotate(GNATprove, False_Positive,
                        "memory leak might occur",
                        "This is not possible, as it's decoded and freed.");

                     IF -- If there's a page table.
                        Current_L1 /= NULL
                     THEN
                        -- Free the page table.
                        Free(Current_L1);
                     END IF;
                  END LOOP;

                  -- Free the directory table.
                  Free(Current_L2);
               END IF;
            END LOOP;

            -- Free the directory pointer table.
            Free(Current_L3);
         END IF;
      END LOOP;

      WHILE -- The check below is just for `gnatprove` and the "Refined_Post".
        (FOR SOME L4_Entry OF Layout.L4 =>
            L4_Entry.Present  OR ELSE
            L4_Entry.Accessed OR ELSE
            L4_Entry.Pointer /= 0)
      LOOP
         FOR -- Clear out all the PML4 entries.
            L4_Entry OF Layout.L4
         LOOP
            -- The other fields will be reset by the mapping procedure.
            L4_Entry.Present  := false;
            L4_Entry.Accessed := false;
            L4_Entry.Pointer  := 0;
         END LOOP;
      END LOOP;
   END Deallocate_Mappings;

   FUNCTION Size_To_Pages
     (Size         : IN number;
      Alignment    : IN page_frame_variant := Page)
      RETURN number
   IS
      Aligned_Size : CONSTANT number :=
         Memory.Align(Size, Alignment, Round_Up => true);
   BEGIN
      -- Read the manuals to see which lower bits need to be zeroed.
      -- It should be rather obvious from the alignment itself.
      RETURN Shift_Right(Aligned_Size, (IF Alignment = Page THEN 12
         ELSIF Alignment = Huge_Page THEN 21 ELSE 30));
   END Size_To_Pages;

   PROCEDURE Load
     (Layout : IN page_layout)
   WITH
      SPARK_Mode => off -- Address attributes are used to load the layout.
   IS
   BEGIN
      Write_CR3(Layout.L4'address);
   END Load;

   PROCEDURE Load_Kernel_Page_Layout
   WITH
      -- Address attributes are used to load the layout and alias the page map
      -- object so they don't need to keep calling this procedure.
      SPARK_Mode => off
   IS
   BEGIN
      Memory.Kernel_Page_Map :=
         Memory.Kernel_Virtual_To_Physical(Kernel_Page_Layout.L4'address);
      Write_CR3(Memory.Kernel_Page_Map);
   END Load_Kernel_Page_Layout;

   FUNCTION Get_Page_Map_Address
     (Layout : IN page_layout)
      RETURN address
   IS
     (Layout.L4'address)
   WITH
      SPARK_Mode => off; -- This is for low-level usage.

END HAVK_Kernel.Paging;
