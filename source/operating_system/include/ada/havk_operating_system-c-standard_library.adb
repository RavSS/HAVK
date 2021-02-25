-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-c-standard_library.adb           --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System.Call.Tasking,
   HAVK_Operating_System.C.String;

PACKAGE BODY HAVK_Operating_System.C.Standard_Library
IS
   PROCEDURE Exit_Task -- `exit()`.
     (Status : IN int)
   IS
   BEGIN
      Call.Tasking.Exit_Task(number(Status));
   END Exit_Task;

   PROCEDURE Abort_Task -- `abort()`.
   IS
   BEGIN
      Exit_Task(EXIT_FAILURE);
   END Abort_Task;

   FUNCTION Absolute_Value -- `abs()`.
     (Value : IN int)
      RETURN int
   IS
     (ABS Value); -- Rather easy in Ada... hopefully it does the same thing.

   FUNCTION Random_Value -- `rand()`.
      RETURN int
   IS
      S               : CONSTANT uint32_t := Random_State.A;
      T               : uint32_t := Random_State.D;
      Generated_Value : uint32_t;
   BEGIN
      Random_State.D := Random_State.C;
      Random_State.C := Random_State.B;
      Random_State.B := S;

      T := T XOR Shift_Right(T, 2);
      T := T XOR Shift_Left(T, 1);
      T := T XOR (S XOR Shift_Left(S, 4));
      Random_State.A := T;

      Random_State.Counter := 362437;
      Generated_Value := T + Random_State.Counter;

      RETURN int(IF Generated_Value <= RAND_MAX THEN Generated_Value ELSE
         RAND_MAX);
   END Random_Value;

   PROCEDURE Seed_Random_State -- `srand()`.
     (Seed : IN unsigned_int)
   IS
   BEGIN
      Random_State := -- As long as not all remain zero, it should be fine.
        (A       => uint32_t(Shift_Right(Seed, 06) OR 1),
         B       => uint32_t(Shift_Right(Seed, 12) OR 1),
         C       => uint32_t(Shift_Right(Seed, 18) OR 1),
         D       => uint32_t(Shift_Right(Seed, 24) OR 1),
         Counter => 0); -- Non-zero does not apply to the counter.
   END Seed_Random_State;

   FUNCTION Memory_Allocation -- `malloc()`.
     (Byte_Size : IN size_t)
      RETURN void_pointer
   IS
      FUNCTION Internal_Memory_Allocation
        (Byte_Size : IN size_t)
         RETURN void_pointer
      WITH
         Import        => true,
         Convention    => C,
         External_Name => "__gnat_malloc";
   BEGIN
      RETURN Internal_Memory_Allocation(Byte_Size);
   END Memory_Allocation;

   FUNCTION Contiguous_Allocation -- `calloc()`.
     (Block_Count     : IN size_t;
      Block_Byte_Size : IN size_t)
      RETURN void_pointer
   IS
      Allocation : CONSTANT void_pointer :=
         Memory_Allocation(Block_Count * Block_Byte_Size);
   BEGIN
      IF
         Allocation /= NULL
      THEN
         RETURN
            C.String.Memory_Set(Allocation, 0, Block_Count * Block_Byte_Size);
      ELSE
         RETURN NULL;
      END IF;
   END Contiguous_Allocation;

   PROCEDURE Free_Memory -- `free()`.
     (Allocated : IN void_pointer)
   IS
      -- `Ada.Unchecked_Deallocation()` is not appropriate for this. I've found
      -- that it has caused crashing due to how it calculates the address it
      -- passes to the internal `__gnat_free()` procedure.
      PROCEDURE Internal_Free_Memory
        (Allocated : IN void_pointer)
      WITH
         Import        => true,
         Convention    => C,
         External_Name => "__gnat_free";
   BEGIN
      Internal_Free_Memory(Allocated);
   END Free_Memory;

END HAVK_Operating_System.C.Standard_Library;
