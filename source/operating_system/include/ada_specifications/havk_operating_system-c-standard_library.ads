-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-c-standard_library.ads           --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

-- An Ada implementation of the C standard library's "stdlib.h".
PACKAGE HAVK_Operating_System.C.Standard_Library
WITH
   Preelaborate => true
IS
   EXIT_SUCCESS : CONSTANT := 0;
   EXIT_FAILURE : CONSTANT := 1;
   RAND_MAX     : CONSTANT := INT_MAX;

   PROCEDURE Exit_Task
     (Status : IN int)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => C,
      External_Name => "exit";

   PROCEDURE Abort_Task
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => C,
      External_Name => "abort";

   FUNCTION Absolute_Value
     (Value : IN int)
      RETURN int
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "abs";

   -- The pseudorandom number functions (and state) below were taken from
   -- Wikipedia and adapted into Ada code.
   -- READ: https://en.wikipedia.org/wiki/Xorshift#xorwow
   FUNCTION Random_Value
      RETURN int
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "rand";

   PROCEDURE Seed_Random_State
     (Seed : IN unsigned_int)
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "srand";

   FUNCTION Memory_Allocation
     (Byte_Size : IN size_t)
      RETURN void_pointer
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "malloc";

   FUNCTION Contiguous_Allocation
     (Block_Count     : IN size_t;
      Block_Byte_Size : IN size_t)
      RETURN void_pointer
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "calloc";

   PROCEDURE Free_Memory
     (Allocated : IN void_pointer)
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "free";

PRIVATE
   -- Needs manual seeding via `srand()`.
   TYPE random_state_information IS RECORD
      A, B, C, D : uint32_t;
      Counter    : uint32_t;
   END RECORD;

   Random_State : random_state_information;

END HAVK_Operating_System.C.Standard_Library;
