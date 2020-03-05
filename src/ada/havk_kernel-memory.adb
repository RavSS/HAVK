-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-memory.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Paging;

PACKAGE BODY HAVK_Kernel.Memory
WITH
   Refined_State => (Memory_State => NULL)
IS
   FUNCTION Align
     (Value     : IN number;
      Alignment : IN number;
      Round_Up  : IN boolean := false)
      RETURN number
   IS
   (
      -- Formula was taken from here under "Computing padding" (2020-02-23).
      -- READ: https://en.wikipedia.org/wiki/Data_structure_alignment
      IF
         NOT Round_Up
      THEN
         Value - (Alignment + (Value AND Alignment - 1) AND Alignment - 1)
      ELSE
         Value + (Alignment - (Value AND Alignment - 1) AND Alignment - 1)
   );

   FUNCTION Allocate_System_Stack
     (Size : IN number)
      RETURN address
   IS
      TYPE kernel_stack IS ARRAY(number RANGE 1 .. Size)
         OF address RANGE 0 .. 2**8 - 1
      WITH
         Component_Size => 008, -- Each element is a single byte.
         Alignment      => 128; -- 16-byte alignment required for the ABI.

      TYPE access_kernel_stack IS NOT NULL ACCESS kernel_stack;

      FUNCTION To_Pointer
        (Stack : IN access_kernel_stack)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;

      -- Creates the new stack for handling interrupts in ring 0.
      New_Stack_End  : CONSTANT access_kernel_stack :=
         NEW kernel_stack'(OTHERS => 0);

      -- Remember that x86 stacks grow downwards.
      New_Stack_Base : CONSTANT address :=
         To_Pointer(New_Stack_End) + address(kernel_stack'last);
   BEGIN
      -- While the alignment is guaranteed and can be assumed, it's probably
      -- safer to just check for alignment anyway instead of using the pragma.
      RETURN address(Align(number(New_Stack_Base), 16));
   END Allocate_System_Stack;

   FUNCTION System_Limit
      RETURN number
   IS
      USE
         HAVK_Kernel.UEFI;

      Map        : CONSTANT memory_map := Get_Memory_Map;
      Attributes : memory_attributes;
   BEGIN
      RETURN
         Limit   : number := 0
      DO
         FOR
            Region OF Map
         LOOP
            Attributes := Get_Memory_Attributes(Region);

            IF -- Check if the region is useable.
               Region.Memory_Region_Type = conventional_data AND THEN
               NOT Attributes.Read_Only                      AND THEN
               NOT Attributes.Write_Protected                AND THEN
               NOT Attributes.Read_Protected
            THEN
               -- Count the conventional memory only. Ignore reserved memory.
               Limit := Limit + Paging.Page * Region.Number_Of_Pages;
            END IF;
         END LOOP;
      END RETURN;
   END System_Limit;

   FUNCTION Kernel_Virtual_To_Physical
     (Kernel_Virtual_Address : IN address)
      RETURN address
   IS
      Bootloader : CONSTANT UEFI.arguments := UEFI.Get_Arguments;
      Offset     : CONSTANT address        :=
         Kernel_Virtual_Base - Bootloader.Physical_Base_Address;
   BEGIN
      RETURN Kernel_Virtual_Address - Offset;
   END Kernel_Virtual_To_Physical;

END HAVK_Kernel.Memory;
