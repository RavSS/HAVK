-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory-manager.adb                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Memory.Manager
WITH
   Refined_State => (Frame_Allocator_State => Physical_Memory,
                     Kernel_Heap_State     => Kernel_Heap)
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

      Frame_Base_Address : CONSTANT frame_address :=
         frame_address(Frame_Index * Paging.Page);
      Map                : CONSTANT UEFI.memory_map := UEFI.Get_Memory_Map
      WITH
         Annotate => (GNATprove, False_Positive,
                      "memory leak might occur at end of scope",
                      "No allocation is occurring.");
   BEGIN
      FOR
         Region OF Map
      LOOP
         IF -- Check if the page address is in a reserved system memory region.
           (Region.Memory_Region_Type /= UEFI.conventional_data OR ELSE
            Undesirable_Attributes(Region))                    AND THEN
            Frame_Base_Address IN Region.Start_Address_Physical ..
               Region.Start_Address_Physical +
               address(Region.Number_Of_Pages * Paging.Page)
         THEN
            RETURN true;
         END IF;
      END LOOP;

      RETURN false;
   END Frame_Is_Reserved;

   PROCEDURE Get_Frames
     (Frames_Base_Address : OUT frame_address;
      Frames_Amount       : IN number := 1)
   IS
      End_Physical_Frame  : number;
   BEGIN
      FOR
         Physical_Frame IN Physical_Memory'range
      LOOP
         End_Physical_Frame := Physical_Frame + (Frames_Amount - 1);

         IF
           (FOR ALL Frame_Index IN Physical_Frame .. End_Physical_Frame =>
               Frame_Index IN Physical_Memory'range  AND THEN
               NOT Physical_Memory(Frame_Index).Used AND THEN
               NOT Frame_Is_Reserved(Frame_Index))
         THEN
            FOR
               Frame_Index IN Physical_Frame .. End_Physical_Frame
            LOOP
               Physical_Memory(Frame_Index).Used := true;
            END LOOP;

            Frames_Base_Address := frame_address(Physical_Frame * Paging.Page);
            RETURN;
         END IF;
      END LOOP;

      -- We have run out of valid free frames.
      Frames_Base_Address := Null_Frame_Address;
   END Get_Frames;

   PROCEDURE Free_Frames
     (Frames_Base_Address : IN frame_address;
      Frames_Amount       : IN number := 1)
   IS
      Base_Frame : CONSTANT number := number(Frames_Base_Address) /
         Paging.Page;
      End_Frame  : CONSTANT number := number(Frames_Base_Address) +
        (Base_Frame + (Paging.Page * (Frames_Amount - 1)) / Paging.Page);
   BEGIN
      IF
         Base_Frame IN Physical_Memory'range AND THEN
         End_Frame  IN Physical_Memory'range
      THEN
         FOR
            Physical_Frame IN Base_Frame .. End_Frame
         LOOP
            Physical_Memory(Physical_Frame).Used := false;
         END LOOP;
      END IF;
   END Free_Frames;

   PROCEDURE Allocator
     (Allocation_Space : IN OUT space;
      Allocation       : OUT generic_pointer;
      Size             : IN number;
      Alignment        : IN number := 0)
   IS
      FUNCTION To_Pointer
        (Allocation_Address : IN address)
         RETURN generic_pointer
      WITH
         Import     => true,
         Convention => Intrinsic;

      -- In order for deallocation to occur without the caller needing to do
      -- calculations on the allocation, I'll store the actual allocated
      -- address precisely before the aligned address we are to return.
      Aligned_Size            : CONSTANT number :=
      (
         IF
            Alignment /= 0
         THEN
            Size + (Alignment + (address'size / 8))
         ELSE
            Size
      );

      True_Allocation_Address : address;
      Allocation_Address      : address;
   BEGIN
      IF
         NOT Allocation_Space.Frames_Set
      THEN
         Allocation := To_Pointer(Null_Allocation_Address);
         RETURN;
      END IF;

      Allocate(Allocation_Space, True_Allocation_Address,
        (IF Aligned_Size > Allocation_Alignment THEN
            Aligned_Size ELSE Allocation_Alignment));

      IF -- Return the actual address if we don't need special alignment.
         Alignment = 0
      THEN
         Allocation := To_Pointer(True_Allocation_Address);
         RETURN;
      END IF;

      -- Align it upwards and save enough room for another pointer.
      Allocation_Address := address(Align(number(True_Allocation_Address) +
        (address'size / 8), Alignment, Round_Up => true));
      Allocation := To_Pointer(Allocation_Address);

      PRAGMA Warnings(GNATprove, off, "unused assignment",
         Reason => "GNATprove is unaware that I store it behind the address.");
      DECLARE -- Just before the returned address, place the real address.
         Free_Address : address
         WITH
            Import  => true,
            Address => True_Allocation_Address;
      BEGIN
         Free_Address := True_Allocation_Address;
      END;
   END Allocator;

   PROCEDURE Deallocator
     (Deallocation_Space : IN OUT space;
      Deallocation       : IN OUT generic_pointer;
      Aligned            : IN boolean := false)
   IS
      FUNCTION To_Address
        (Deallocation_Address : IN generic_pointer)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;

      FUNCTION To_Pointer
        (Deallocated_Address  : IN address)
         RETURN generic_pointer
      WITH
         Import     => true,
         Convention => Intrinsic;

      -- For more on how this is calculated (if the address was aligned after
      -- allocation), check the `Allocator()` generic procedure.
      True_Deallocation_Address : CONSTANT address
      WITH
         Import  => true,
         Address => To_Address(Deallocation) - (address'size / 8);

      -- Only use the appropriate one.
      Deallocation_Address : address := (IF Aligned THEN
         True_Deallocation_Address ELSE To_Address(Deallocation));
   BEGIN
      IF
         Deallocation_Space.Frames_Set AND THEN
         To_Address(Deallocation) /= Null_Allocation_Address
      THEN
         Deallocate(Deallocation_Space, Deallocation_Address);
         Deallocation := To_Pointer(Deallocation_Address);
         PRAGMA Annotate(GNATprove, False_Positive, "memory leak might occur",
            "It's either not a valid address to begin with or it turns null.");
      END IF;
   END Deallocator;

   PROCEDURE Allocate
     (Allocation_Space : IN OUT space;
      Allocation       : OUT address;
      Allocation_Size  : IN number)
   IS
      FUNCTION To_Pointer
        (New_Block : IN address)
         RETURN access_block
      WITH
         Import     => true,
         Convention => Intrinsic,
         Pre        => New_Block /= 0,
         Post       => To_Pointer'result /= NULL;

      Current_Limit : address := Allocation_Space.Base_Limit;
      Valid_Block   : boolean;
      Current_Block : access_block
      WITH
         Annotate => (GNATprove, False_Positive,
                      "memory leak might occur at end of scope",
                      "Handled through manual memory access.");
      New_Block     : access_block
      WITH
         Annotate => (GNATprove, False_Positive,
                      "memory leak might occur at end of scope",
                      "Handled through manual memory access.");
   BEGIN
      IF -- Specially handle the first block.
         Allocation_Space.Base_Block = NULL
      THEN
         Allocation_Space.Base_Block     := To_Pointer(Current_Limit);
         Allocation_Space.Base_Block.ALL :=
           (Next_Block => NULL,
            Last_Block => NULL,
            Magic      => Magic,
            Used       => true,
            Size       => Allocation_Size,
            Data       => void'first);
         PRAGMA Annotate(GNATprove, Intentional, "memory leak might occur",
            "This is the allocation itself that is later manually freed.");
         Allocation := Block_Data(Allocation_Space.Base_Block);
         RETURN;
      ELSE
         Current_Block := Allocation_Space.Base_Block;
      END IF;

      LOOP
         PRAGMA Loop_Invariant(Current_Block /= NULL);

         IF -- Check if it's free.
            NOT Current_Block.Used AND THEN
            Current_Block.Size <= Allocation_Size
         THEN -- Return the free block.
            -- TODO: Merging or halving free blocks might be optimal here.
            Current_Block.Used := true;
            Allocation := Block_Data(Current_Block);
            RETURN;
         END IF;

         -- Skip the current block, as it's used.
         Current_Limit := Block_Data(Current_Block); -- Volatile function.
         Current_Limit := Current_Limit + (address(Current_Block.Size) + 1);

         IF
            Current_Limit NOT IN
               Allocation_Space.Base_Limit .. Allocation_Space.End_Limit
         THEN
            Allocation := Null_Allocation_Address; -- Exhausted memory space.
            RETURN;
         END IF;

         EXIT WHEN Current_Block.Next_Block = NULL;

         Valid_Block := Block_Is_Valid
           (Allocation_Space.Base_Limit,
            Allocation_Space.End_Limit,
            Current_Block.Next_Block);

         IF -- Check for corruption of the next block.
            Valid_Block
         THEN -- It's valid, so we traverse forward.
            Current_Block := Current_Block.Next_Block;
            PRAGMA Annotate(GNATprove, False_Positive,
               "memory leak might occur", "Leave the previous block alone.");
         ELSE
            RAISE Panic
            WITH
               Source_Location & " - A memory manager block has been damaged.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Block damage is external or we returned a bad data address.");
         END IF;
      END LOOP;

      -- TODO: I have absolutely no idea how to do this without aliasing, which
      -- is obviously not allowed in SPARK. What I've done below violates the
      -- ownership rules for moves. I need to point the current block towards
      -- the new block and the new block to the current (now previous) block
      -- for the next/last block fields. The below intrinsic function is
      -- basically an implicit assumption that the aliasing is acceptable.
      Current_Block.Next_Block := To_Pointer(Current_Limit);
      New_Block                := To_Pointer(Current_Limit);

      New_Block.ALL :=
        (Next_Block => NULL,
         Last_Block => Current_Block, -- TODO: See the above.
         Magic      => Magic,
         Used       => true,
         Size       => Allocation_Size,
         Data       => void'first);
      PRAGMA Annotate(GNATprove, False_Positive, "memory leak might occur",
         "We're manually creating a new block in a checked area.");

      Allocation    := Block_Data(New_Block);
      Current_Block := New_Block.Last_Block;

      WHILE -- Move the original base block back by reversing to the start.
         Current_Block /= NULL AND THEN
         Current_Block.Last_Block /= NULL
      LOOP
         -- Check for corruption again, but in reverse for the last blocks.
         Valid_Block := Block_Is_Valid
           (Allocation_Space.Base_Limit,
            Allocation_Space.End_Limit,
            Current_Block.Last_Block);

         IF
            Valid_Block
         THEN
            Current_Block := Current_Block.Last_Block;
            PRAGMA Annotate(GNATprove, False_Positive,
               "memory leak might occur", "We're just going backwards.");
         ELSE
            RAISE Panic
            WITH
               Source_Location & " - A memory manager block has been damaged.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Block damage is external or we returned a bad data address.");
         END IF;
      END LOOP;

      Allocation_Space.Base_Block := Current_Block;
   END Allocate;

   PROCEDURE Deallocate
     (Deallocation_Space : IN OUT space;
      Deallocation       : IN OUT address)
   IS
      Current_Block      : access_block := Deallocation_Space.Base_Block
      WITH
         Annotate => (GNATprove, False_Positive,
                      "memory leak might occur at end of scope",
                      "We're only traversing and marking a block as unused.");
      Current_Block_Data : address;
      Valid_Block        : boolean;
   BEGIN
      IF
         Current_Block = NULL
      THEN
         RETURN;
      END IF;

      LOOP
         PRAGMA Loop_Invariant(Current_Block /= NULL);
         Current_Block_Data := Block_Data(Current_Block);

         IF
            Current_Block.Used AND THEN
            Current_Block_Data = Deallocation
         THEN
            PRAGMA Warnings(GNATprove, off, "unused assignment",
               Reason => "It's used by the `Allocate()` procedure instead.");
            Current_Block.Used := false;
            Deallocation       := Null_Allocation_Address;
            RETURN;
         END IF;

         EXIT WHEN Current_Block.Next_Block = NULL;

         Valid_Block := Block_Is_Valid
           (Deallocation_Space.Base_Limit,
            Deallocation_Space.End_Limit,
            Current_Block.Next_Block);

         IF
            Valid_Block
         THEN
            Current_Block := Current_Block.Next_Block;
            PRAGMA Annotate(GNATprove, False_Positive,
               "memory leak might occur", "Leave the previous block alone.");
         ELSE
            RAISE Panic
            WITH
               Source_Location & " - A memory manager block has been damaged.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Block damage is external or we returned a bad data address.");
         END IF;
      END LOOP;

      -- TODO: This isn't necessary, but `gnatprove` and the ownership policy
      -- demand it. Not quite sure how to avoid doing so. I don't want to raise
      -- a panic here, as that is not necessary. If I didn't have to do this,
      -- then a singly linked list would be possible, and I would have to avoid
      -- that aliasing in the `Allocate()` procedure up above.
      WHILE -- Move the original base block back by reversing to the start.
         Current_Block /= NULL AND THEN
         Current_Block.Last_Block /= NULL
      LOOP
         Valid_Block := Block_Is_Valid
           (Deallocation_Space.Base_Limit,
            Deallocation_Space.End_Limit,
            Current_Block.Last_Block);

         IF
            Valid_Block
         THEN
            Current_Block := Current_Block.Last_Block;
            PRAGMA Annotate(GNATprove, False_Positive,
               "memory leak might occur", "We're just going backwards.");
         ELSE
            RAISE Panic
            WITH
               Source_Location & " - A memory manager block has been damaged.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Block damage is external or we returned a bad data address.");
         END IF;
      END LOOP;

      Deallocation_Space.Base_Block := Current_Block;
   END Deallocate;

   FUNCTION Block_Data
     (Current_Block : NOT NULL ACCESS CONSTANT block)
      RETURN address
   IS
     (address(Align(number(Current_Block.Data'address),
         Allocation_Alignment, Round_Up => true)))
   WITH
      SPARK_Mode => off; -- This is a short wrapper for the address attribute.

   FUNCTION Block_Is_Valid
     (Base_Limit    : IN address;
      End_Limit     : IN address;
      Current_Block : NOT NULL ACCESS CONSTANT block)
      RETURN boolean
   IS
      FUNCTION To_Address
        (Check_Block  : ACCESS CONSTANT block)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;

      Base_Data_Limit : CONSTANT address := Block_Data(Current_Block);
      End_Data_Limit  : CONSTANT address :=
         Base_Data_Limit + address(Current_Block.Size);
   BEGIN
      RETURN
      (
         -- Check if the block itself is in the memory space's limits.
         To_Address(Current_Block) IN Base_Limit .. End_Limit AND THEN
         -- Check if the magic number has not been corrupted.
         Current_Block.Magic = Magic AND THEN
         -- Check if the last and next blocks are not in the block's data.
         -- They can be null (address zero), but the limits cannot be zero.
         To_Address(Current_Block.Last_Block) NOT IN
            Base_Data_Limit .. End_Data_Limit AND THEN
         To_Address(Current_Block.Next_Block) NOT IN
            Base_Data_Limit .. End_Data_Limit
      );
   END Block_Is_Valid;

   FUNCTION New_Space
      RETURN space
   IS
     (OTHERS => <>);

   FUNCTION Get_Base
     (Current_Space : IN space)
      RETURN address
   IS
     (Current_Space.Base_Limit);

   PROCEDURE Initialise_Space
     (New_Space   : IN OUT space;
      Size        : IN number;
      Error_Check : OUT error)
   WITH
      Refined_Global => (In_Out => Physical_Memory,
                         Input  => UEFI.Bootloader_Arguments),
      Refined_Post   => Error_Check IN no_error | memory_error | size_error
                           AND THEN
                       (IF Error_Check IN no_error | memory_error THEN
                           New_Space.Frames_Set)
   IS
      Base_Frame_Address : frame_address;
   BEGIN
      IF
         New_Space.Frames_Set -- Don't redo the initialisation.
      THEN
         Error_Check := memory_error;
         RETURN;
      END IF;

      Get_Frames(Base_Frame_Address, Size / Paging.Page);

      IF
         Base_Frame_Address = 0 OR ELSE
         Base_Frame_Address = Null_Frame_Address
      THEN
         Error_Check := size_error;
         RETURN;
      END IF;

      New_Space :=
        (Base_Block => NULL,
         Base_Limit => Base_Frame_Address,
         End_Limit  => Base_Frame_Address + address(Size),
         Frames_Set => true);
      PRAGMA Annotate(GNATprove, False_Positive, "memory leak might occur",
         "We cannot destruct heap spaces yet. Freeing happens elsewhere.");

      Error_Check := no_error;
   END Initialise_Space;

   FUNCTION Kernel_Allocate
     (Size : IN number)
      RETURN pointer
   WITH
      SPARK_Mode => off -- See the specification for why. This is not pure.
   IS
      FUNCTION To_Pointer
        (Pointer_Address : IN address)
         RETURN pointer
      WITH
         Import     => true,
         Convention => Intrinsic;

      PROCEDURE Kernel_Allocator IS NEW Allocator
        (generic_data => void, generic_pointer => pointer);
   BEGIN
      RETURN
         Allocation : pointer
      DO
         Kernel_Allocator(Kernel_Heap, Allocation, Size);

         IF -- Check if the kernel heap is full or not.
            Allocation = To_Pointer(Null_Allocation_Address)
         THEN
            RAISE Panic
            WITH
               Source_Location & " - The kernel heap is exhausted.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "We must be careful and use dynamic memory sparsely.");
         END IF;
      END RETURN;
   END Kernel_Allocate;

   PROCEDURE Kernel_Deallocate
     (Free_Address : IN OUT pointer)
   WITH
      Refined_Global => (In_Out => Kernel_Heap, Input => Memory_State)
   IS
      -- I could technically just call `Deallocate()` directly and change the
      -- passed argument to this procedure into an address instead of an access
      -- type, but this is more consistent.
      PROCEDURE Kernel_Deallocator IS NEW Deallocator
        (generic_data => void, generic_pointer => pointer);
   BEGIN
      Kernel_Deallocator(Kernel_Heap, Free_Address, Aligned => false);
   END Kernel_Deallocate;

   PROCEDURE Prepare_Kernel_Heap
   WITH
      Refined_Global => (In_Out => (Physical_Memory, Kernel_Heap,
                                    Kernel_Heap_Base),
                         Input  => UEFI.Bootloader_Arguments),
      Refined_Post   => Kernel_Heap.Frames_Set
   IS
      Error_Check : error;
   BEGIN
      IF -- Prevent multiple preparations of the heap.
         NOT Kernel_Heap.Frames_Set
      THEN
         Initialise_Space(Kernel_Heap, Kernel_Heap_Size, Error_Check);

         IF
            Error_Check = no_error
         THEN
            Kernel_Heap_Base := Kernel_Heap.Base_Limit;
         ELSE
            RAISE Panic
            WITH
               Source_Location & " - Failed to initialise the kernel's heap.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "The kernel heap size is either too large or not enough RAM.");
         END IF;
      END IF;
   END Prepare_Kernel_Heap;

END HAVK_Kernel.Memory.Manager;
