-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory-manager.ads                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Paging;

-- This package houses a frame allocator and a memory manager for specific
-- memory spaces. It's extremely basic and is just a static bitmap for the
-- former and a (doubly) linked list for the latter.
-- TODO: Things like fragmentation are not currently minimised and there are no
-- real optimisations for now. There is zero block merging and halving, so the
-- memory manager is not (space) efficient at all; however, we do have the
-- ability to free allocations, which is better than a static buffer allocator.
PACKAGE HAVK_Kernel.Memory.Manager
WITH
   Preelaborate   => true,
   Abstract_State => (Frame_Allocator_State, Kernel_Heap_State)
IS
   -- The main type for this package. Describes a memory space. The fields are
   -- private to prevent accidental changes.
   TYPE space IS PRIVATE;
   PRAGMA Preelaborable_Initialization(space);

   -- An address guaranteed/proven to be 4 KiB aligned.
   SUBTYPE frame_address IS address
      RANGE address'first .. address'last - address(Paging.Page - 1)
   WITH
      Dynamic_Predicate => frame_address MOD address(Paging.Page) = 0;

   -- Returns an empty record for default initialisation cases.
   FUNCTION New_Space
      RETURN space
   WITH
      Inline => true;

   -- Returns the base address of the space.
   FUNCTION Get_Base
     (Current_Space : IN space)
      RETURN address
   WITH
      Inline => true;

   -- Prepares a memory space by allocating a number of consecutive frames
   -- based on the memory size requested (which must be 4 KiB aligned).
   -- Returns a memory error if it's already initialised or a size error if the
   -- space cannot be initialised.
   PROCEDURE Initialise_Space
     (New_Space   : IN OUT space;
      Size        : IN number;
      Error_Check : OUT error)
   WITH
      Global => (In_Out => Frame_Allocator_State),
      Pre    => Size IN Paging.Page .. number(address'last) - Paging.Page - 1
                   AND THEN
                Size MOD Paging.Page = 0,
      Post   => Error_Check IN no_error | memory_error | size_error;

   -- Sets up a memory space internal to the package that controls the kernel
   -- heap. The size is obtained from the parent package to this one.
   PROCEDURE Prepare_Kernel_Heap
   WITH
      Global => (In_Out => (Kernel_Heap_State, Frame_Allocator_State,
                            Kernel_Heap_Base));

   -- A generic wrapper for the internal `Allocate()` procedure that can return
   -- a specific access type in one of its parameters. The size is in bytes.
   -- You can also specify an alignment (power-of-two only). If it's zero, then
   -- the allocation will be aligned to the system default. Note that it's a
   -- lazy alignment that happens outside the actual block management.
   GENERIC
      TYPE generic_data(<>) IS PRIVATE;
      TYPE generic_pointer IS ACCESS generic_data;
   PROCEDURE Allocator
     (Allocation_Space : IN OUT space;
      Allocation       : OUT generic_pointer;
      Size             : IN number;
      Alignment        : IN number := 0)
   WITH
      Inline => true,
      Pre    => Size + Alignment >= Size          AND THEN
                Size + Alignment >= Alignment     AND THEN
               (Alignment AND (Alignment - 1)) = 0; -- Zero is still accepted.

   -- A generic wrapper for the internal `Deallocate()` procedure takes in any
   -- access type and then tries to free it. If the value becomes null, then it
   -- was freed; otherwise, it was not deallocated. If you allocated an aligned
   -- address, then you must indicate that it was aligned so the deallocator
   -- can retrieve the actual allocated address behind the aligned allocation
   -- address.
   GENERIC
      TYPE generic_data(<>) IS PRIVATE;
      TYPE generic_pointer IS ACCESS generic_data;
   PROCEDURE Deallocator
     (Deallocation_Space : IN OUT space;
      Deallocation       : IN OUT generic_pointer;
      Aligned            : IN boolean := false)
   WITH
      Inline => true;

   -- These are returned if there is no more free frames or free allocations
   -- left. Note that these are non-canonical address, so these are impossible
   -- to use as pointers on x86-64 as of 2020-03-05. Check any changes for
   -- alignment requirements first.
   Null_Frame_Address      : CONSTANT frame_address := 16#DEADBEEF00000000#;
   Null_Allocation_Address : CONSTANT address       := 16#DEA110C8ED000000#;

PRIVATE
   -- A type that is to be used where the object pointed at by the pointer is
   -- irrelevant. Think of this as a universal generic pointer.
   TYPE pointer IS ACCESS void;

   -- Describes a 4 KiB region of memory. It only has a size of a bit for now,
   -- but this can be expanded if urgently needed. I've gone with only 4 KiB
   -- because it's easy, even though it uses a lot of space and storing more
   -- information for each frame is not cheap.
   TYPE frame IS RECORD
      Used : boolean := false;
   END RECORD;
   FOR frame USE RECORD
      Used AT 0 RANGE 0 .. 0;
   END RECORD;

   -- Describes the current PC's (physical) memory in 4 KiB pages. Even when
   -- we're only using a single bit to indicate whether or not the page is
   -- used, this becomes a gigantic array that is around the size of 4 MiB.
   -- The lower 16 MiB memory space can be used for old ISA devices in regards
   -- to DMA (direct memory access). We'll ignore it for now. At its highest,
   -- HAVK will currently support a maximum 128 GiB of physical memory. That
   -- should be enough for everyone running a hobbyist operating system. We can
   -- always raise it later whenever we want in the future. Note that the
   -- maximum amount of frames we can address is 2^31, as the last index must
   -- be at or under `integer'last + 1`. That is a maximum of 8 TiB and we
   -- we would inflate the size of the kernel by around 250~ MiB. Not too bad.
   TYPE frames IS ARRAY
     (number RANGE (16 * MiB) / Paging.Page .. (128 * GiB) / Paging.Page - 1)
      OF ALIASED frame
   WITH
      Component_Size => 1; -- Explicitly pack it.

   -- This magic number should read "ALLOCATED"; it's just a way of making
   -- manual memory probing a little easier when using e.g. QEMU.
   Magic : CONSTANT number := 16#A110C8ED#;

   -- A block indicating an allocation. This is used to create the linked list.
   TYPE block;
   TYPE access_block IS ACCESS block;
   TYPE block IS RECORD
      -- The two blocks that either traverse backwards or forwards.
      Last_Block : access_block;
      Next_Block : access_block;
      -- The magic number goes here. This should be removed in a later version,
      -- as it would have little purpose outside of debugging. There's also
      -- better ways to check for corruption, but this does the job for now.
      Magic      : number RANGE 0 .. 2**32 - 1;
      -- When true, the block's data space is currently in usage.
      Used       : boolean;
      -- The size of the data space. This does not include the block metadata,
      -- which is absolutely everything before the "Data" field itself.
      Size       : number;
      -- This value's address (when aligned to the allocation alignment
      -- upwards) is the address of the data that the block represents. The
      -- value at this field should not be touched when the block's data is
      -- being utilised, as it (usually) represents the first byte of it.
      Data       : ALIASED void;
   END RECORD
   WITH
      Dynamic_Predicate => Magic = Magic AND THEN
                           Size >= Allocation_Alignment;
   FOR block USE RECORD
      Last_Block AT 00 RANGE 0 .. 63;
      Next_Block AT 08 RANGE 0 .. 63;
      Magic      AT 16 RANGE 0 .. 31;
      Used       AT 20 RANGE 0 .. 07; -- Padded out to a full byte.
      Size       AT 21 RANGE 0 .. 63;
      Data       AT 29 RANGE 0 .. 07;
   END RECORD;

   -- This describes a dynamic memory space. Can be used for heaps, program
   -- spaces, etc. as long as the frame allocator knows about it physically.
   -- WARNING: One caveat is that it does not distinguish between virtual
   -- and physical addresses; the limits can be described by either of them.
   -- You must make sure that the addresses make sense in regards to paging.
   TYPE space IS RECORD
      Base_Block : access_block  := NULL;
      Base_Limit : frame_address := address(0);
      End_Limit  : frame_address := address(0);
      Frames_Set : boolean       := false;
   END RECORD
   WITH
      Dynamic_Predicate => (IF Frames_Set THEN Base_Limit /= 0 AND THEN
                               End_Limit >= Base_Limit);

   -- Returns the base address of the next free frame in the first parameter.
   -- If the value returned is the last possible 64-bit value, then there is no
   -- more free frames available. This is a quasi-function, as it's a function
   -- in the form of a procedure due to the fact that it isn't pure and has
   -- side-effects.
   PROCEDURE Get_Frames
     (Frames_Base_Address : OUT frame_address;
      Frames_Amount       : IN number := 1)
   WITH
      Pre  => Frames_Amount IN 1 .. number(address'last) / Paging.Page - 1,
      Post => (IF Frames_Base_Address /= Null_Frame_Address THEN
                  Frames_Base_Address + frame_address(Paging.Page *
                  Frames_Amount) > Frames_Base_Address);

   -- Converts a frame's base address to an element and sets it to unused.
   -- Can do a chain of free operations as well.
   PROCEDURE Free_Frames
     (Frames_Base_Address : IN frame_address;
      Frames_Amount       : IN number := 1)
   WITH
      Pre => Frames_Amount /= 0;

   -- Simply compares the frame (transformed to an address) against the memory
   -- map that the UEFI firmware provided to us. Returns true if it's reserved.
   -- Only checks for whether or not it's inside a conventional memory region.
   FUNCTION Frame_Is_Reserved
     (Frame_Index : IN number)
      RETURN boolean
   WITH
      Inline => true,
      Pre    => Frame_Index IN frames'range;

   -- Returns the base address of a block's data. This is a wrapper for the
   -- address attribute.
   FUNCTION Block_Data
     (Current_Block : NOT NULL ACCESS CONSTANT block)
      RETURN address
   WITH
      Volatile_Function => true,
      Global            => (Input => Memory_State),
      Inline            => true,
      Post              => Block_Data'result MOD
                              address(Allocation_Alignment) = 0;

   -- Checks if a pointer/access to a block is valid and then checks the magic
   -- value of the block. Does not check the accesses to other blocks within
   -- the block that was just passed.
   FUNCTION Block_Is_Valid
     (Base_Limit    : IN address;
      End_Limit     : IN address;
      Current_Block : NOT NULL ACCESS CONSTANT block)
      RETURN boolean
   WITH
      Volatile_Function => true,
      Global            => (Input => Memory_State),
      Inline            => true,
      Pre               => Base_Limit /= 0 AND THEN End_Limit >= Base_Limit;

   -- Allocates an address inside a specific space. If there is no more space
   -- left, this will assign the "Allocation" parameter a null value.
   PROCEDURE Allocate
     (Allocation_Space : IN OUT space;
      Allocation       : OUT address;
      Allocation_Size  : IN number)
   WITH
      Global => (Input => Memory_State),
      Pre    => Allocation_Space.Frames_Set AND THEN
                Allocation_Size >= Allocation_Alignment;

   -- Deallocates an address inside a specific space by marking its respective
   -- block as unused. You can pass any address value to this, but in order for
   -- it to be effective, it needs a value that its counterpart once returned
   -- for the same memory space.
   PROCEDURE Deallocate
     (Deallocation_Space : IN OUT space;
      Deallocation       : IN OUT address)
   WITH
      Global => (Input => Memory_State),
      Pre    => Deallocation_Space.Frames_Set AND THEN
                Deallocation_Space.End_Limit >= Deallocation_Space.Base_Limit,
      Post   => Deallocation = Deallocation'old OR ELSE
                Deallocation = Null_Allocation_Address;

   -- A wrapper for supporting the Ada keyword "NEW". For GNAT, it internally
   -- calls "__gnat_malloc". While it would be possible to "swap" the memory
   -- space that the keyword uses, I've decided to avoid that and keep it
   -- exclusive to kernel heap allocations.
   FUNCTION Kernel_Allocate
     (Size : IN number)
      RETURN pointer
   WITH
      SPARK_Mode    => off, -- This must be in the form of a non-pure function.
      Export        => true,
      Convention    => C, -- These two subprograms must be compatible with C.
      External_Name => "__gnat_malloc";

   -- This is utilised by the intrinsic subprogram "Ada.Unchecked_Deallocation"
   -- and it works as a call to a `free()` equivalent in C.
   PROCEDURE Kernel_Deallocate
     (Free_Address : IN OUT pointer)
   WITH
      Global        => (In_Out => Kernel_Heap_State, Input => Memory_State),
      Export        => true,
      Convention    => C,
      External_Name => "__gnat_free";

   -- The actual variable for counting physical frames. The range is built into
   -- the "frames" type itself, which is defined up above.
   Physical_Memory : frames
   WITH
      Part_Of => Frame_Allocator_State;

   -- The internal memory space for the kernel heap. Only this package can
   -- touch it through Ada, as it is specially handled, but sometimes you need
   -- it directly. Import it for that purpose.
   Kernel_Heap : space
   WITH
      Part_Of       => Kernel_Heap_State,
      Export        => true,
      External_Name => "ada__kernel_heap";

END HAVK_Kernel.Memory.Manager;
