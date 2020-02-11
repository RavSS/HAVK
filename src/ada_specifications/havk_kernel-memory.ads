-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-memory.ads                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package contains logic for memory operations.
PACKAGE HAVK_Kernel.Memory
IS
   PRAGMA Preelaborate;

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
      Post => Allocate_System_Stack'result >= Kernel_Heap_Base AND THEN
              Allocate_System_Stack'result <= Kernel_Heap_End;

   -- What follows below are useful symbol pointer values. Note that some of
   -- them have ranges on them to make `gnatprove` have some notion of what
   -- values it can realistically expect without needing assumptions.

   -- TODO: Ada (the standard, not GNAT) seems to not understand how to define
   -- true compile-time constants that are imported. For some reason, it won't
   -- let you mark a package as pre-elaboration if you have an imported (but
   -- constant) variable and another package relies upon it. It calls them
   -- "non-static constants". It's something I would expect of a volatile
   -- constant which can externally change, but not of these below variables.
   -- A basic workaround is to just make a function return it instead of having
   -- it as a variable in here. It would be nice if a new aspect or something
   -- along it was introduced to alleviate this behaviour.

   Kernel_Base           : CONSTANT address
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_base_address";

   Kernel_End            : CONSTANT address
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_end_address";

   Kernel_Virtual_Base   : CONSTANT address
      RANGE 16#FFFFFFFF80000000# .. 16#FFFFFFFF80000000#
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_virtual_base_address";

   Kernel_Physical_Base  : CONSTANT address
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_physical_base_address";

   Kernel_Text_Base      : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_text_base_address";

   Kernel_Text_End       : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_text_end_address";

   Kernel_RO_Data_Base   : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_rodata_base_address";

   Kernel_RO_Data_End    : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_rodata_end_address";

   Kernel_Data_Base      : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_data_base_address";

   Kernel_Data_End       : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_data_end_address";

   Kernel_BSS_Base       : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_bss_base_address";

   Kernel_BSS_End        : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "__kernel_bss_end_address";

   FUNCTION Kernel_Heap_Base
      RETURN address
   WITH
      Inline => true;

   FUNCTION Kernel_Heap_End
      RETURN address
   WITH
      Inline => true;

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
