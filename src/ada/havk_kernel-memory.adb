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
IS
   -- The formula was taken from here.
   -- https://en.wikipedia.org/wiki/Data_structure_alignment#Computing_padding
   FUNCTION Align
     (Value     : IN number;
      Alignment : IN number;
      Round_Up  : IN boolean := false)
      RETURN number
   IS
   (
      IF
         NOT Round_Up
      THEN
         Value - ((Alignment + (Value MOD Alignment)) MOD Alignment)
      ELSE
         Value + ((Alignment - (Value MOD Alignment)) MOD Alignment)
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
         Convention => Intrinsic,
         Post       => To_Pointer'result IN
                          Kernel_Heap_Base .. Kernel_Heap_End;

      -- Creates the new stack for handling interrupts in ring 0.
      New_Stack_End  : CONSTANT access_kernel_stack :=
         NEW kernel_stack'(OTHERS => 0);

      -- Remember that x86 stacks grow downwards.
      New_Stack_Base : CONSTANT address :=
         To_Pointer(New_Stack_End) + address(kernel_stack'last);
   BEGIN
      IF
         New_Stack_Base IN Kernel_Heap_Base .. Kernel_Heap_End
      THEN
         RETURN New_Stack_Base;
      ELSE
         RAISE Panic
         WITH
            "Ran out of heap memory for system stack allocation.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "HAVK should keep sane limits to prevent this from occuring.");
      END IF;
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

   FUNCTION Kernel_Heap_Base
      RETURN address
   IS
      Kernel_Heap_Base_Address : CONSTANT address
         RANGE Kernel_Virtual_Base .. address'last
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "__kernel_heap_base_address";
   BEGIN
      RETURN Kernel_Heap_Base_Address;
   END Kernel_Heap_Base;

   FUNCTION Kernel_Heap_End
      RETURN address
   IS
      Kernel_Heap_End_Address : CONSTANT address
         RANGE Kernel_Heap_Base + address'size / 8 .. address'last
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "__kernel_heap_end_address";
   BEGIN
      RETURN Kernel_Heap_End_Address;
   END Kernel_Heap_End;

END HAVK_Kernel.Memory;
