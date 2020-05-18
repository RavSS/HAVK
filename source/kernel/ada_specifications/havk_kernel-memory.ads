-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory.ads                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Paging;

-- This package contains logic for memory operations and everything else to do
-- with memory, like the kernel's memory layout.
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
      Pre    => Alignment /= 0 AND THEN (Alignment AND -Alignment) = Alignment,
      Post   => Align'result MOD Alignment = 0;

   -- Allocates a valid stack on the kernel heap that can go inside the RSP
   -- register and then returns a pointer to the start/base of it. The size is
   -- one-based and the returned address is 16-byte aligned, with the end
   -- of the stack (the lowest possible address) being aligned to a page.
   FUNCTION Allocate_System_Stack
     (Size : IN number)
      RETURN address
   WITH
      Pre  => Size /= 0 AND THEN Size MOD Paging.Page = 0,
      Post => Allocate_System_Stack'result MOD 16 = 0 AND THEN
             (number(Allocate_System_Stack'result) - Size) MOD Paging.Page = 0;

   -- Converts a virtual address inside kernel space (higher-half virtual
   -- memory) to a physical one. This is needed as the physical base address of
   -- the kernel is calculated during my bootloader's ELF loading sequence.
   FUNCTION Kernel_Virtual_To_Physical
     (Kernel_Virtual_Address : IN address)
      RETURN address
   WITH
      Inline => true;

   -- An x86-64 stack according to the System V ABI. 16-byte alignment is
   -- required for the ABI, but we'll additionally demand that it's aligned to
   -- a 4 KiB page boundary as well, as we often need to map it.
   TYPE x86_stack IS NEW bytes
   WITH
      Alignment => Paging.Page;

   -- A access to an x86-64 stack.
   TYPE access_x86_stack IS ACCESS x86_stack;

   -- For accessing the raw address of the stack.
   FUNCTION To_Address
     (Stack : IN access_x86_stack)
      RETURN address
   WITH
      Import     => true,
      Convention => Intrinsic;

   -- The alignment of the returned value by the memory manager (`malloc()`).
   -- This is often just 16 for x86-64 and it can also be retrieved via
   -- `Standard'Maximum_Alignment`, but that is apparently an implementation
   -- defined attribute whereas `Standard'System_Allocator_Alignment` is not,
   -- even though it is listed as one in the GNAT documentation under 4.58.
   -- Utilising the latter is a SPARK violation, so we'll manually define it.
   Allocation_Alignment : CONSTANT := 16;

   -- This controls the heap size for the kernel. It's only static for now.
   Kernel_Heap_Size     : CONSTANT := 100 * MiB;

   -- The dynamic value for the consecutive kernel heap memory.
   -- Must be initialised by the memory manager.
   Kernel_Heap_Base     : address  := 0;

   -- Contains the kernel layout's L4 address. See the assembly declaration for
   -- more information.
   Kernel_Page_Map      : address
   WITH
      Import         => true,
      Convention     => Assembler,
      External_Name  => "var__kernel_page_map_base_address",
      Linker_Section => ".isolated_bss";

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

   Kernel_Virtual_Base       : CONSTANT address
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_virtual_base_address";

   Kernel_Virtual_End        : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_virtual_end_address";

   Kernel_Text_Base          : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_text_base_address";

   Kernel_Text_End           : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_text_end_address";

   Kernel_RO_Data_Base       : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_rodata_base_address";

   Kernel_RO_Data_End        : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_rodata_end_address";

   Kernel_Data_Base          : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_data_base_address";

   Kernel_Data_End           : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_data_end_address";

   Kernel_BSS_Base           : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_bss_base_address";

   Kernel_BSS_End            : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_bss_end_address";

   Kernel_Isolated_Text_Base : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_text_base_address";

   Kernel_Isolated_Text_End  : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_text_end_address";

   Kernel_Isolated_Data_Base : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_data_base_address";

   Kernel_Isolated_Data_End  : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_data_end_address";

   Kernel_Isolated_BSS_Base  : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_bss_base_address";

   Kernel_Isolated_BSS_End   : CONSTANT address
      RANGE Kernel_Virtual_Base .. address'last
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_bss_end_address";

   Kernel_Size               : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_size_address";

   Kernel_Text_Size          : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_text_size_address";

   Kernel_RO_Data_Size       : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_rodata_size_address";

   Kernel_Data_Size          : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_data_size_address";

   Kernel_BSS_Size           : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_bss_size_address";

   Kernel_Isolated_Text_Size : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_text_size_address";

   Kernel_Isolated_Data_Size : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_data_size_address";

   Kernel_Isolated_BSS_Size : CONSTANT number
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "var__kernel_isolated_bss_size_address";

END HAVK_Kernel.Memory;
