-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-memory.ads                                 --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

-- This package contains logic for memory operations.
PACKAGE HAVK_Kernel.Memory
IS
   -- Returns the total usable memory limit that is not reserved for hardware
   -- or any other firmware purposes.
   FUNCTION System_Limit
   RETURN num;

   -- Takes in a value and aligns it to the power-of-2 alignment specified.
   -- Note that it rounds down, not up, if no third argument is provided.
   -- TODO: Might want to add a post-condition contract that checks for
   -- alignment, but I can't seem to prove `Align'result MOD Alignment = 0`.
   -- The examples brought up by `gnatprove` aren't valid (?), as it for some
   -- reason gives an example where "Alignment" is zero in the post-condition.
   -- Regardless of that, does it have to do with wrap-around arithmetic?
   FUNCTION Align(
      Value     : IN num;
      Alignment : IN num;
      Round_Up  : IN boolean := false)
   RETURN num
   WITH
      Inline => true,
      Pre    => Alignment /= 0 AND THEN (Alignment AND -Alignment) = Alignment;

   Kernel_Base           : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_base_address";

   Kernel_End            : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_end_address";

   Kernel_Virtual_Base   : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_virtual_base_address";

   Kernel_Physical_Base  : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_physical_base_address";

   Kernel_Text_Base      : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_text_base_address";

   Kernel_Text_End       : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_text_end_address";

   Kernel_RO_Data_Base   : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_rodata_base_address";

   Kernel_RO_Data_End    : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_rodata_end_address";

   Kernel_Data_Base      : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_data_base_address";

   Kernel_Data_End       : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_data_end_address";

   Kernel_BSS_Base       : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_bss_base_address";

   Kernel_BSS_End        : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_bss_end_address";

   Kernel_Heap_Base      : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_heap_base_address";

   Kernel_Heap_End       : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_heap_end_address";

   Kernel_Size           : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_size_address";

   Kernel_Text_Size      : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_text_size_address";

   Kernel_RO_Data_Size   : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_rodata_size_address";

   Kernel_Data_Size      : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_data_size_address";

   Kernel_BSS_Size       : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_bss_size_address";

   Kernel_Heap_Size      : CONSTANT num
   WITH
      Import        => true,
      Convention    => NASM,
      External_Name => "__kernel_heap_size_address";

END HAVK_Kernel.Memory;
