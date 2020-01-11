-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-memory.ads                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package contains logic for memory operations.
PACKAGE HAVK_Kernel.Memory
IS
   -- Returns the total usable memory limit that is not reserved for hardware
   -- or any other firmware purposes.
   FUNCTION System_Limit
      RETURN number;

   -- Takes in a value and aligns it to the power-of-2 alignment specified.
   -- Note that it rounds down, not up, if no third argument is provided.
   -- TODO: Might want to add a post-condition contract that checks for
   -- alignment, but I can't seem to prove `Align'result MOD Alignment = 0`.
   -- The examples brought up by `gnatprove` aren't valid (?), as it for some
   -- reason gives an example where "Alignment" is zero in the post-condition.
   -- Regardless of that, does it have to do with wrap-around arithmetic?
   FUNCTION Align
     (Value     : IN number;
      Alignment : IN number;
      Round_Up  : IN boolean := false)
      RETURN number
   WITH
      Inline => true,
      Pre    => Alignment /= 0 AND THEN (Alignment AND -Alignment) = Alignment;

   -- Allocates a valid stack on the heap that can go inside the RSP register
   -- and then returns a pointer to the top of it. The size starts from one.
   -- TODO: The issue with the `Align()` post-condition occurs with this
   -- as well. Could be a bug with `gnatprove`?
   FUNCTION Allocate_System_Stack
     (Size : IN number)
     RETURN address
   WITH
      Pre  => Size >= 16 AND THEN Size MOD 16 = 0,
      Post => Allocate_System_Stack'result >= Address_Value(Kernel_Heap_Base)
              AND THEN
              Allocate_System_Stack'result <= Address_Value(Kernel_Heap_End);

   -- What follows below are useful symbol pointer values. Note that some of
   -- them have ranges on them to make `gnatprove` have some notion of what
   -- values it can realistically expect without needing assumptions.

   Kernel_Base           : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_base_address";

   Kernel_End            : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_end_address";

   Kernel_Virtual_Base   : CONSTANT number
      RANGE 16#FFFFFFFF80000000# .. 16#FFFFFFFF80000000#
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_virtual_base_address";

   Kernel_Physical_Base  : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_physical_base_address";

   Kernel_Text_Base      : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_text_base_address";

   Kernel_Text_End       : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_text_end_address";

   Kernel_RO_Data_Base   : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_rodata_base_address";

   Kernel_RO_Data_End    : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_rodata_end_address";

   Kernel_Data_Base      : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_data_base_address";

   Kernel_Data_End       : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_data_end_address";

   Kernel_BSS_Base       : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_bss_base_address";

   Kernel_BSS_End        : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_bss_end_address";

   Kernel_Heap_Base      : CONSTANT number
      RANGE Kernel_Virtual_Base .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_heap_base_address";

   Kernel_Heap_End       : CONSTANT number
      RANGE Kernel_Heap_Base + address'size / 8 .. number'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_heap_end_address";

   Kernel_Heap_Top       : CONSTANT number
      RANGE Kernel_Heap_Base .. Kernel_Heap_End
   WITH
      Import        => true,
      Convention    => Ada,
      External_Name => "__kernel_heap_top";

   Kernel_Size           : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_size_address";

   Kernel_Text_Size      : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_text_size_address";

   Kernel_RO_Data_Size   : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_rodata_size_address";

   Kernel_Data_Size      : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_data_size_address";

   Kernel_BSS_Size       : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_bss_size_address";

   Kernel_Heap_Size      : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_heap_size_address";

END HAVK_Kernel.Memory;
