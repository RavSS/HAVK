-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory.ads                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Paging;

-- This package contains logic for memory operations and everything else to do
-- with memory, like the kernel's memory layout.
-- TODO: Create more address subtypes with dynamic predicates for virtual
-- and physical memory.
PACKAGE HAVK_Kernel.Memory
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
      Memory_State
      WITH
         External => (Async_Readers, Async_Writers, Effective_Writes)
   )
IS
   -- Returns the total usable memory limit that is not reserved for hardware
   -- or any other firmware purposes.
   FUNCTION System_Limit
      RETURN number;

   -- Takes in a value and aligns it to the power-of-2 alignment specified.
   -- Note that it rounds down, not up, if no third argument is provided.
   FUNCTION Align
     (Value     : IN number;
      Alignment : IN number;
      Round_Up  : IN boolean := false)
      RETURN number
   WITH
      Inline => true,
      Pre    => Alignment >= 2 AND THEN (Alignment AND (Alignment - 1)) = 0,
      Post   => Align'result MOD Alignment = 0;

   -- Converts a virtual address inside kernel space (higher-half virtual
   -- memory) to a physical one. This is needed as the physical base address of
   -- the kernel is calculated during my bootloader's ELF loading sequence.
   FUNCTION Kernel_Virtual_To_Physical
     (Kernel_Virtual_Address : IN address)
      RETURN address
   WITH
      Inline => true;

   -- This is for a 48-bit address space, which is the current size for x86 in
   -- 64-bit mode. This will surely change in the future with Intel's new
   -- 5-level paging that's already present in newer processors.
   SUBTYPE canonical_address IS address
   WITH
      Dynamic_Predicate => canonical_address IN address'first ..
                              2**47 - 1 | -(2**47) .. address'last;

   -- The opposite of a canonical address.
   SUBTYPE invalid_address IS address RANGE 2**47 .. -(2**47) - 1;

   -- An address guaranteed/proven to be 4 KiB aligned.
   SUBTYPE page_address IS address
      RANGE address'first .. address'last - (Paging.page_size'enum_rep - 1)
   WITH
      Dynamic_Predicate => page_address MOD Paging.page_size'enum_rep = 0
                              AND THEN
                           page_address IN address'first .. 2**47 - 1 |
                              -(2**47) .. address'last -
                                (Paging.page_size'enum_rep - 1);

   -- Just a simple `memcpy()`. Use with high amounts of caution. Keep its use
   -- limited to when you're just moving data around and don't want to waste
   -- time marshalling. Preferably avoid it and stick to loops.
   PROCEDURE Copy
     (Destination : IN canonical_address;
      Source      : IN canonical_address;
      Bytes       : IN number)
   WITH
      Global        => (In_Out => Memory_State),
      Import        => true,
      Convention    => C,
      External_Name => "memcpy",
      Pre           => Bytes < number(invalid_address'first) AND THEN
                       Destination + address(Bytes) NOT IN
                          invalid_address'range AND THEN
                       Source + address(Bytes) NOT IN invalid_address'range;

   -- This controls the heap size for the kernel. It's only static for now.
   -- This must be a multiple of 4 KiB.
   Kernel_Heap_Size     : CONSTANT := 100 * MiB;

   -- The dynamic frame address for the beginning of the kernel heap space.
   Kernel_Heap_Base     : page_address := 0
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__kernel_heap_base";

   -- The end address of the heap. A reminder that this is a boundary, the
   -- address value itself is not a part of the heap, but the address before it
   -- is.
   Kernel_Heap_End      : page_address := 0
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__kernel_heap_end";

   -- The alignment of the returned value by the memory manager (`malloc()`).
   -- This is often just 16 for x86-64 and it can also be retrieved via
   -- `Standard'Maximum_Alignment`, but that is apparently an implementation
   -- defined attribute whereas `Standard'System_Allocator_Alignment` is not,
   -- even though it is listed as one in the GNAT documentation under 4.58.
   -- Utilising the latter is a SPARK violation, so we'll manually define it.
   Allocation_Alignment : CONSTANT := 16;

   -- Contains the kernel layout's L4 address. See the assembly declaration for
   -- more information.
   Kernel_Page_Map      : address
   WITH
      Import         => true,
      Convention     => Assembler,
      External_Name  => "assembly__kernel_page_map_base_address",
      Linker_Section => ".isolated_bss";

   -- What follows below are useful symbol pointer values. Note that some of
   -- them have ranges on them to make `gnatprove` have some notion of what
   -- values it can realistically expect without needing assumptions. The
   -- address values should always be canonical and should also be placed on a
   -- 4 KiB boundary (page-aligned). Both are controlled through the kernel's
   -- linker script.

   -- TODO: Ada (the standard, not GNAT) seems to not understand how to define
   -- true compile-time constants that are imported. For some reason, it won't
   -- let you mark a package as pre-elaboration if you have an imported (but
   -- constant) variable and another package relies upon it. It calls them
   -- "non-static constants". It's something I would expect of a volatile
   -- constant which can externally change, but not of these below variables.
   -- A basic workaround is to just make a function return it instead of having
   -- it as a variable in here. It would be nice if a new aspect or something
   -- along it was introduced to alleviate this behaviour.

   Kernel_Virtual_Base       : CONSTANT page_address
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_virtual_base_address";

   Kernel_Virtual_End        : CONSTANT page_address
      RANGE Kernel_Virtual_Base ..
         address'last - (Paging.page_size'enum_rep - 1)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_virtual_end_address";

   Kernel_Text_Base          : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_text_base_address";

   Kernel_Text_End           : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_text_end_address";

   Kernel_RO_Data_Base       : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_rodata_base_address";

   Kernel_RO_Data_End        : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_rodata_end_address";

   Kernel_Data_Base          : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_data_base_address";

   Kernel_Data_End           : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_data_end_address";

   Kernel_BSS_Base           : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_bss_base_address";

   Kernel_BSS_End            : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_bss_end_address";

   Kernel_Isolated_Text_Base : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_text_base_address";

   Kernel_Isolated_Text_End  : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_text_end_address";

   Kernel_Isolated_Data_Base : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_data_base_address";

   Kernel_Isolated_Data_End  : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_data_end_address";

   Kernel_Isolated_BSS_Base  : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_bss_base_address";

   Kernel_Isolated_BSS_End   : CONSTANT page_address
      RANGE Kernel_Virtual_Base .. Kernel_Virtual_End
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_bss_end_address";

   Kernel_Size               : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_size_address";

   Kernel_Text_Size          : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_text_size_address";

   Kernel_RO_Data_Size       : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_rodata_size_address";

   Kernel_Data_Size          : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_data_size_address";

   Kernel_BSS_Size           : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_bss_size_address";

   Kernel_Isolated_Text_Size : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_text_size_address";

   Kernel_Isolated_Data_Size : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_data_size_address";

   Kernel_Isolated_BSS_Size : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "linker__kernel_isolated_bss_size_address";

END HAVK_Kernel.Memory;
