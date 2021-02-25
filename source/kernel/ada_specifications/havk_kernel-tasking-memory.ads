-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-memory.ads                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Memory;

-- This package handles memory for tasks while defining ranges for the user's
-- virtual memory layout.
PACKAGE HAVK_Kernel.Tasking.Memory
IS
   -- The following virtual addresses are for tasking. Each one defines a
   -- "standard" virtual address base for various memory ranges that are
   -- exposed to the user. Right now, it goes from 64 TiB to the end of the
   -- canonical lower-half of virtual address space. This is indicative of a
   -- 48-bit address space, which is the standard address space for x86-64
   -- until 56-bit address spaces are supported. For these ranges, the range
   -- values should both be inclusive.
   SUBTYPE user_virtual_address IS address
      RANGE 16#4000_0000_0000# .. 16#7FFF_FFFF_FFFF#;

   -- This should be used for mapping MMIO address ranges into user space for
   -- tasks that are authorised to use them.
   SUBTYPE user_hardware_address IS user_virtual_address
      RANGE user_virtual_address'first .. user_virtual_address'first +
         2 * TiB - 1;

   SUBTYPE user_framebuffer_address IS user_hardware_address
      RANGE user_hardware_address'first .. user_hardware_address'first +
         1 * GiB - 1;

   -- All allocated memory frames/blocks given to tasks should be visible
   -- through these address ranges.
   SUBTYPE user_memory_address IS user_virtual_address
      RANGE user_hardware_address'last + 1 .. user_virtual_address'last;

   -- Extends the end of a task's heap's memory space by a page and then
   -- outputs the end of the task's heap memory space. The end address is the
   -- next page that is not (yet) mapped.
   PROCEDURE Increase
     (Task_Index   : IN number;
      End_Address  : OUT HAVK_Kernel.Memory.canonical_address;
      Error_Status : OUT error)
   WITH
      Post => Error_Status IN no_error | index_error | memory_error |
                 attempt_error AND THEN
             (IF Error_Status = no_error THEN
                 End_Address IN user_memory_address'range ELSE
                 End_Address IN 0 | user_memory_address'range);

END HAVK_Kernel.Tasking.Memory;
