-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory-frames.adb                          --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

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
         page_address(Frame_Index * Paging.page_size'enum_rep);
      Missing_Frame      : boolean := false;
   BEGIN
      FOR
         Region OF UEFI.Memory_Map
      LOOP -- Check if the page address is in a reserved system memory region.
         IF
            Region /= NULL AND THEN
            Frame_Base_Address IN Region.Start_Address_Physical ..
               Region.Start_Address_Physical +
               address(Region.Number_Of_Pages * Paging.page_size'enum_rep)
         THEN
            -- Memory descriptors inside the memory map can and will often
            -- share memory ranges. So, we just check if the frame is reserved
            -- in any single region inside the memory map; otherwise, odd
            -- dangerous things will happen.
            IF
               Region.Memory_Region_Type /= UEFI.conventional_data OR ELSE
               Undesirable_Attributes(Region)
            THEN
               RETURN true;
            END IF;

            Missing_Frame := false;
         END IF;
      END LOOP;

      RETURN Missing_Frame;
   END Frame_Is_Reserved;

   PROCEDURE Allocate
     (Frame_Base_Address : OUT page_address;
      Frame_Owner        : IN number;
      Frame_Count        : IN frame_limit := 1)
   WITH
      Refined_Post => Frame_Base_Address = Null_Frame_Address          OR ELSE
                     (Frame_Base_Address IN valid_frame_address'range AND THEN
                      Frame_Base_Address + page_address
                        (Paging.page_size'enum_rep * (Frame_Count - 1)) IN
                            valid_frame_address'range)
   IS
      Base_Frame, New_Base : number RANGE Physical_Frames'range :=
         Physical_Frames'first;
      Valid_Range          : boolean;
   BEGIN
      Frame_Check : WHILE
         Base_Frame + (Frame_Count - 1) IN Physical_Frames'range
      LOOP
         Valid_Range := true;

         FOR -- Check whether the range is marked as used first.
            Frame_Index IN Base_Frame .. Base_Frame + (Frame_Count - 1)
         LOOP
            IF
               Physical_Frames(Frame_Index).Used
            THEN
               EXIT Frame_Check WHEN
                  Frame_Index + 1 NOT IN Physical_Frames'range;

               New_Base := Frame_Index + 1;
               Valid_Range := false;
            END IF;
         END LOOP;

         -- Check the reservedness last, as it should be more expensive to
         -- perform a region check than just seeing if a bit is set or not.
         IF
            Valid_Range
         THEN
            FOR
               Frame_Index IN Base_Frame .. Base_Frame + (Frame_Count - 1)
            LOOP
               IF
                  Frame_Is_Reserved(Frame_Index)
               THEN
                  EXIT Frame_Check WHEN
                     Frame_Index + 1 NOT IN Physical_Frames'range;

                  New_Base := Frame_Index + 1;
                  Valid_Range := false;
               END IF;
            END LOOP;

            IF
               Valid_Range
            THEN
               FOR
                  Frame_Index IN Base_Frame .. Base_Frame + (Frame_Count - 1)
               LOOP
                  Physical_Frames(Frame_Index) :=
                    (Owner => Frame_Owner,
                     Used  => true);
               END LOOP;

               Frame_Base_Address :=
                  page_address(Base_Frame * Paging.page_size'enum_rep);
               RETURN;
            END IF;

            Base_Frame := New_Base;
         ELSE
            Base_Frame := New_Base;
         END IF;
      END LOOP Frame_Check;

      -- We have run out of valid free frames or we can't provide the number of
      -- page frames in a consecutive manner.
      Frame_Base_Address := Null_Frame_Address;
   END Allocate;

   PROCEDURE Deallocate
     (Frame_Base_Address : IN page_address;
      Frame_Count        : IN frame_limit := 1)
   IS
      Base_Frame : CONSTANT number :=
         number(Frame_Base_Address / Paging.page_size'enum_rep);
      End_Frame  : CONSTANT number := number(Frame_Base_Address +
         address(Base_Frame + (Paging.page_size'enum_rep * (Frame_Count - 1)) /
            Paging.page_size'enum_rep));
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
      Heap_Frames : number;
   BEGIN
      IF -- Only initialise the heap once. Ignore all other attempts.
         Kernel_Heap_Base /= 0 OR ELSE
         Kernel_Heap_End  /= 0
      THEN
         RETURN;
      END IF;

      UEFI.Parse_Memory_Map; -- Fill in the memory map accordingly.

      Heap_Frames := Paging.Size_To_Pages(Kernel_Heap_Size);
      IF -- Need at least one frame for the kernel's heap.
         Heap_Frames < 1
      THEN
         Heap_Frames := 1;
      END IF;

      Allocate(Kernel_Heap_Base, 0, Frame_Count => Heap_Frames);
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
