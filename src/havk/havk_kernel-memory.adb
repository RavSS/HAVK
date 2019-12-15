-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-memory.adb                                 --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI;

PACKAGE BODY HAVK_Kernel.Memory
IS
   -- The formula was taken from here.
   -- https://en.wikipedia.org/wiki/Data_structure_alignment#Computing_padding
   FUNCTION Align(
      Value     : IN num;
      Alignment : IN num;
      Round_Up  : IN boolean := false)
   RETURN num     IS
   (
      IF NOT Round_Up THEN
         Value - ((Alignment + (Value MOD Alignment)) MOD Alignment)
      ELSE
         Value + ((Alignment - (Value MOD Alignment)) MOD Alignment)
   );

   PROCEDURE Prepare_Heap(
      Page_Structure : IN OUT page_layout)
   IS
      USE
         HAVK_Kernel.UEFI;

      Map            : CONSTANT memory_map := Get_Memory_Map;
      Attributes     : memory_attributes;
   BEGIN
      FOR I IN Map'range LOOP
         Attributes := Get_Memory_Attributes(Map(I));

         IF -- Check if the region is useable.
            Map(I).Memory_Region_Type = conventional_data AND
            THEN NOT Attributes.Read_Only                 AND
            THEN NOT Attributes.Write_Protected           AND
            THEN NOT Attributes.Read_Protected
         THEN
            FOR P IN num RANGE 0 .. Map(I).Number_Of_Pages LOOP
               IF -- Check if the region overlaps or has the heap space.
                  Map(I).Start_Address_Physical + Page * P
                  IN kernel_heap_physical'range
               THEN
                  EXIT WHEN Kernel_Heap_Left + Page > Kernel_Heap_Size;

                  Page_Structure.Map_Address(
                     (Map(I).Start_Address_Physical + Kernel_Virtual_Base) +
                        Page * P,
                     Map(I).Start_Address_Physical + Page * P,
                     Write_Access => true);

                  Kernel_Heap_Left := Kernel_Heap_Left + Page;
               END IF;
            END LOOP;

            -- Count the conventional memory only. Ignore reserved memory.
            System_Memory_Limit := System_Memory_Limit +
               Page * Map(I).Number_Of_Pages;
         END IF;
      END LOOP;

      Log("Usable system memory size is" &
         num'image(System_Memory_Limit / MiB) & " MiB.", nominal);
      Log("Kernel heap size is" &
         num'image(Kernel_Heap_Left    / MiB) & " MiB.", nominal);
   END Prepare_Heap;

   FUNCTION Allocate(
      Size : IN num)
   RETURN pointer
   WITH
      SPARK_Mode => off -- See this function's specification for the reason.
   IS
      -- This intrinsic is normally found within the RTS package called
      -- "System.Address_To_Access_Conversions" with a few differences,
      -- but I've put it here to save space and time, as it obviously
      -- does not require a body.
      FUNCTION To_Pointer(Allocated_Address : num)
      RETURN pointer
      WITH
         Import     => true,
         Inline     => true,
         Convention => Intrinsic;

      -- Allocated addresses begin on an 8-byte aligned boundary. This
      -- should fit the most common alignment for e.g. `num`.
      Alignment    : CONSTANT num := 8;

      Allocation   : CONSTANT num := Align(kernel_heap_virtual'last -
         Kernel_Heap_Left, Alignment, Round_Up => true);

      Aligned_Size : CONSTANT num := Align(Size, Alignment, Round_Up => true);
   BEGIN
      -- See if the allocation's range actually fits into the heap's range.
      IF Allocation + Aligned_Size NOT IN kernel_heap_virtual'range THEN
         Log("The kernel's heap has been exhausted.", fatal);
         RAISE Storage_Error;
      END IF;

      -- "Mark" the space as used and return a pointer to the space's start.
      Kernel_Heap_Left := Kernel_Heap_Left - Aligned_Size;
      RETURN To_Pointer(Allocation);
   END Allocate;
END HAVK_Kernel.Memory;
