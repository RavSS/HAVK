WITH
   HAVK_Kernel.Paging;
USE
   HAVK_Kernel.Paging;

-- This package contains logic for memory operations. As of now, there's an
-- extremely lazy/primitive allocator that purposefully disallows deallocation
-- and does things in a last-in-never-out fashion like a corrupted stack.
-- NOTE: If really needed, replace this with K&R `malloc()` repurposed for the
-- kernel level and a real page frame allocator.
PACKAGE HAVK_Kernel.Memory
IS
   -- This maps the heap in the respective page structure layout.
   -- It must be called before any heap allocations are made, or else all of
   -- the allocations will fail.
   PROCEDURE Prepare_Heap(
      Page_Structure : IN OUT page_layout);

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

   -- This value is the amount of conventional memory the running system has.
   System_Memory_Limit : num := 0;

PRIVATE
   -- For now, I've hard-coded in a maximum heap size of 128 MiB. It would be
   -- better to make this extendable depending on additional required memory.
   -- This cannot exceed 2-GiB minus the size of the kernel and any unused
   -- space before it. It also does not factor in any reserved memory, so the
   -- actual heap size depending on the memory map may not be this exact value.
   Kernel_Heap_Size : CONSTANT num := 128 * MiB;

   -- The heap starts right after the kernel's end, which is page aligned.
   SUBTYPE kernel_heap_virtual  IS num RANGE
      Kernel_End .. Kernel_End + Kernel_Heap_Size;

   -- The same as the "kernel_heap_virtual" range type, but has the virtual
   -- base subtracted from the heap's beginning and end.
   SUBTYPE kernel_heap_physical IS num RANGE
      kernel_heap_virtual'first - Kernel_Virtual_Base
      ..
      kernel_heap_virtual'last  - Kernel_Virtual_Base;

   -- After the heap is prepared, this will contain the amount of memory left
   -- for usage. Once this hits zero (again), heap allocation will not be
   -- possible and a null pointer will be returned.
   Kernel_Heap_Left : num RANGE 0 .. Kernel_Heap_Size := 0;

   -- A pointer that cannot be freed. A character is used to represent a byte,
   -- as the size of the type being accessed is not truly consequential here.
   -- The compiler will ignore it and implicitly use the size of the type
   -- being allocated on the heap.
   TYPE pointer IS NOT NULL ACCESS character
   WITH
      Convention => C;

   -- This should never be called directly and instead the "NEW" keyword
   -- should be utilised. This is normally in the "s-memory.ad{b,s}" RTS file,
   -- but I cannot (and would rather not) fit it there, as it requires
   -- freestanding knowledge of how the system's memory is laid out during
   -- runtime, not prior to it.
   FUNCTION Allocate(
      Size : IN num)
   RETURN pointer
   WITH
      SPARK_Mode    => off, -- The function must effect "Kernel_Heap_Left".
      Export        => true,
      Convention    => C,
      External_Name => "__gnat_malloc";

END HAVK_Kernel.Memory;
