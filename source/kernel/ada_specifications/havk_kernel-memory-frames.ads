-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory-frames.ads                          --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI;
USE TYPE
   HAVK_Kernel.UEFI.access_memory_descriptor;

-- This package manages physical frames of 4 KiB each. It currently uses a very
-- naive and simple way of allocating/deallocating them.
-- TODO: This entire package needs to be reworked, as its design is
-- embarassingly inefficient.
PACKAGE HAVK_Kernel.Memory.Frames
WITH
   Preelaborate   => true,
   Abstract_State => (Frame_Allocator_State)
IS
   SUBTYPE frame_limit IS number
      RANGE 0 .. number(address'last / address(Paging.Page));

   -- Returns the base address of the next free frame in the first parameter.
   -- This is a quasi-function, as it's a function in the form of a procedure
   -- due to the fact that it isn't pure and has side-effects.
   PROCEDURE Allocate
     (Frame_Base_Address : OUT page_address;
      Frame_Owner        : IN number;
      Frame_Count        : IN frame_limit := 1)
   WITH
      Pre => Frame_Count /= 0 AND THEN
             Frame_Owner <= 2**15 - 1;

   -- Converts a frame's base address to an element and sets it to unused.
   -- Can do a chain of free operations as well.
   PROCEDURE Deallocate
     (Frame_Base_Address : IN page_address;
      Frame_Count        : IN frame_limit := 1)
   WITH
      Pre => Frame_Count /= 0;

   -- This frees all frames that have been marked for a specific owner.
   PROCEDURE Deallocate_All_Owner_Frames
     (Frame_Owner : IN number);

   -- This is returned if there is no more free physical frames left or if the
   -- range of consecutive frames is not available. The value below is outside
   -- the current limit of RAM for x86-64 (above 256 TiB).
   Null_Frame_Address : CONSTANT page_address :=
      address'last - address(Paging.Page - 1);

   -- Must be called before the "NEW" keyword can be used, as it gives
   -- consecutive frames to the kernel itself. The memory manager is in the
   -- runtime files "s-cmallo.ad{b,s}", as AdaCore has already provided one
   -- that is perfect for my current use. It is however not in SPARK (it's
   -- using a linked list i.e. pointers anyway), but I presume that their one
   -- has no bugs.
   PROCEDURE Prepare_Kernel_Heap;

PRIVATE
   -- A 16-bit record that describes a frame. This bloats up the kernel ELF by
   -- a lot when stored in droves, as we're storing a word for every single 4
   -- KiB physical frame.
   TYPE frame IS RECORD
      -- A value that indicates an "owner" of a specific frame. As of now, this
      -- just indicates the task index/identity, with a value of zero being "no
      -- one" or belonging to the kernel.
      Owner : number RANGE 0 .. 2**15 - 1 := 0;
      -- When true, the frame is in use by the owner of the frame.
      Used  : boolean := false;
   END RECORD
   WITH
      Object_Size => 16;
   FOR frame USE RECORD
      Owner AT 0 RANGE 00 .. 14;
      Used  AT 0 RANGE 15 .. 15;
   END RECORD;

   -- Describes the current PC's (physical) memory in 4 KiB pages. Even when
   -- we're only using a single bit to indicate whether or not the page is
   -- used, this becomes a gigantic array that is around the size of 64 MiB.
   -- The lower 16 MiB memory space can be used for old ISA devices in regards
   -- to DMA (direct memory access). We'll ignore it for now. At its highest,
   -- HAVK will currently support a maximum 128 GiB of physical memory. That
   -- should be enough for everyone running a hobbyist operating system. We can
   -- always raise it later whenever we want in the future. Note that the
   -- maximum amount of frames we can address is 2^31, as the last index must
   -- be at or under `integer'last + 1`. That is a maximum of 8 TiB.
   -- TODO: Might want to look into a way to create the last index dynamically,
   -- as 128 GiB is an overkill estimation for systems in 2020 and a waste of
   -- space, as the UEFI memory map only covers the actual RAM. May also want
   -- to change the default physical frame size to something like 2 MiB.
   TYPE frame_collection IS ARRAY
     (number RANGE (16 * MiB) / Paging.Page .. (128 * GiB) / Paging.Page - 1)
         OF frame
   WITH
      Pack => true;

   -- Simply compares the frame (transformed to an address) against the memory
   -- map that the UEFI firmware provided to us. Returns true if it's reserved.
   -- Only checks for whether or not it's inside a conventional memory region.
   FUNCTION Frame_Is_Reserved
     (Frame_Index : IN number)
      RETURN boolean
   WITH
      Inline => true,
      Pre    => Frame_Index IN frame_collection'range;

   -- The actual variable for counting physical frames. The range is built into
   -- the "frame_collection" type itself, which is defined up above.
   Physical_Frames : frame_collection
   WITH
      Part_Of => Frame_Allocator_State;

END HAVK_Kernel.Memory.Frames;
