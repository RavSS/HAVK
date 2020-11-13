-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory-allocator.ads                       --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   SPARK.Heap;

-- This package contains anything to do with memory allocation.
PACKAGE HAVK_Kernel.Memory.Allocator
IS
   -- This is a workaround for discriminant record allocation failure. Using
   -- the "NEW" keyword, the Ada code will assume all allocations succeeded
   -- (due to no "Storage_Error" being raised) and will then try to store the
   -- discriminant inside the record itself. This drastically fails due to an
   -- obvious null dereference. Also, use this with care, as it sidesteps the
   -- fact that SPARK requires dynamically allocated objects to be initialised.
   -- This does not do proper initialisation and it zeroes out the entire
   -- memory area, so the e.g. record must be able to be represented by zeros
   -- alone.
   GENERIC
      TYPE generic_discriminant IS MOD <>;
      TYPE generic_record(Discriminant : generic_discriminant) IS
         LIMITED PRIVATE;
      TYPE access_generic_record IS ACCESS generic_record;
   PROCEDURE Discriminant_Record
     (Allocation : OUT access_generic_record;
      Byte_Size  : IN number)
   WITH
      Global        => (In_Out => (SPARK.Heap.Dynamic_Memory, Memory_State)),
      Import        => true,
      Convention    => Assembly,
      External_Name => "assembly__memory_allocator_discriminant_record";

END HAVK_Kernel.Memory.Allocator;