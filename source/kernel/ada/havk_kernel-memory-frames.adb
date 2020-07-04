-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory-frames.adb                          --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI;

PACKAGE BODY HAVK_Kernel.Memory.Frames
WITH
   Refined_State => (Frame_Allocator_State => Physical_Frames)
IS
   FUNCTION Frame_Is_Reserved
     (Frame_Index : IN number)
      RETURN boolean
   IS
      USE TYPE
         UEFI.memory_type;

      -- Returns true if there's a number of attributes that are not suitable
      -- for the memory manager.
      -- TODO: Expand this and the encompassing function for requesting
      -- specific attributes on a frame.
      FUNCTION Undesirable_Attributes
        (Region : NOT NULL ACCESS CONSTANT UEFI.memory_descriptor)
         RETURN boolean
      WITH
         Inline => true;

      FUNCTION Undesirable_Attributes
        (Region : NOT NULL ACCESS CONSTANT UEFI.memory_descriptor)
         RETURN boolean
      IS
         Attributes : CONSTANT UEFI.memory_attributes :=
            UEFI.Get_Memory_Attributes(Region);
      BEGIN
         RETURN
         (
            Attributes.Read_Only        OR ELSE
            Attributes.Read_Protected   OR ELSE
            Attributes.Write_Protected  OR ELSE
            Attributes.Specific_Purpose OR ELSE
            Attributes.Persistent
         );
      END Undesirable_Attributes;

      Frame_Base_Address : CONSTANT page_address :=
         page_address(Frame_Index * Paging.Page);
      Map                : CONSTANT UEFI.memory_map := UEFI.Get_Memory_Map
      WITH
         Annotate => (GNATprove, False_Positive,
                      "memory leak might occur at end of scope",
                      "No allocation is occurring.");
   BEGIN
      FOR
         Region OF Map
      LOOP -- Check if the page address is in a reserved system memory region.
         IF
            Frame_Base_Address IN Region.Start_Address_Physical ..
               Region.Start_Address_Physical +
               address(Region.Number_Of_Pages * Paging.Page)
         THEN
               RETURN (Region.Memory_Region_Type /= UEFI.conventional_data
                  OR ELSE Undesirable_Attributes(Region));
         END IF;
      END LOOP;

      RETURN true;
   END Frame_Is_Reserved;

   PROCEDURE Allocate
     (Frame_Base_Address : OUT page_address;
      Frame_Owner        : IN number;
      Frame_Count        : IN number := 1)
   WITH
      Refined_Post => (IF Frame_Base_Address /= Null_Frame_Address THEN
                          number(Frame_Base_Address)
                             IN Paging.Page * Physical_Frames'first ..
                                Paging.Page * Physical_Frames'last
                             AND THEN
                          number(Frame_Base_Address +
                             page_address(Paging.Page * (Frame_Count - 1)))
                                IN Paging.Page * Physical_Frames'first ..
                                   Paging.Page * Physical_Frames'last)
   IS
      End_Frame : number;
   BEGIN
      FOR -- TODO: This is extremely inefficient and stupid slow. Fix it later.
         Base_Frame IN Physical_Frames'range
      LOOP
         End_Frame := Base_Frame + (Frame_Count - 1);

         IF -- TODO: This looks up every single frame in the memory map.
           (FOR ALL Frame_Index IN Base_Frame .. End_Frame =>
               Frame_Index IN Physical_Frames'range  AND THEN
               NOT Physical_Frames(Frame_Index).Used AND THEN
               NOT Frame_Is_Reserved(Frame_Index))
         THEN
            FOR
               Frame_Index IN Base_Frame .. End_Frame
            LOOP
               Physical_Frames(Frame_Index).Owner := Frame_Owner;
               Physical_Frames(Frame_Index).Used  := true;
            END LOOP;

            Frame_Base_Address := page_address(Base_Frame * Paging.Page);
            RETURN;
         END IF;
      END LOOP;

      -- We have run out of valid free frames or we can't provide the number of
      -- page frames in a consecutive manner.
      Frame_Base_Address := Null_Frame_Address;
   END Allocate;

   PROCEDURE Deallocate
     (Frame_Base_Address : IN page_address;
      Frame_Count        : IN number := 1)
   IS
      Base_Frame : CONSTANT number := number(Frame_Base_Address) / Paging.Page;
      End_Frame  : CONSTANT number := number(Frame_Base_Address) +
        (Base_Frame + (Paging.Page * (Frame_Count - 1)) / Paging.Page);
   BEGIN
      IF
         Base_Frame IN Physical_Frames'range AND THEN
         End_Frame  IN Physical_Frames'range
      THEN
         FOR
            Physical_Frame IN Base_Frame .. End_Frame
         LOOP
            Physical_Frames(Physical_Frame) := (OTHERS => <>);
         END LOOP;
      END IF;
   END Deallocate;

   PROCEDURE Deallocate_All_Owner_Frames
     (Frame_Owner : IN number)
   IS
   BEGIN
      FOR -- TODO: This is very lazy and checks the whole array. Fix it later.
         Physical_Frame OF Physical_Frames
      LOOP
         IF
            Physical_Frame.Owner = Frame_Owner
         THEN
            Physical_Frame := (OTHERS => <>);
         END IF;
      END LOOP;
   END Deallocate_All_Owner_Frames;

   PROCEDURE Prepare_Kernel_Heap
   IS
   BEGIN
      IF -- Only initialise the heap once. Ignore all other attempts.
         Kernel_Heap_Base /= 0 OR ELSE
         Kernel_Heap_End  /= 0
      THEN
         RETURN;
      END IF;

      Allocate(Kernel_Heap_Base, 0, -- The below needs to be at least one page.
         Frame_Count => Paging.Size_To_Pages(Kernel_Heap_Size) OR 1);
      Kernel_Heap_End := Kernel_Heap_Base + page_address(Kernel_Heap_Size);

      IF
         Kernel_Heap_Base = Null_Frame_Address OR ELSE
         Kernel_Heap_End  = Null_Frame_Address
      THEN
         RAISE Panic
         WITH
            Source_Location & " - Not enough consecutive RAM for kernel heap.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "We can't continue if we can't provide a heap for the kernel.");
      END IF;
   END Prepare_Kernel_Heap;

END HAVK_Kernel.Memory.Frames;
