------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2013-2019, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Simple implementation for use with Ravenscar Minimal. This implementation
--  is based on a simple static buffer (whose bounds are defined in the linker
--  script), and allocation is performed through a protected object to
--  protect against concurrency.

PRAGMA Restrictions (No_Elaboration_Code);
--  This unit may be linked without being with'ed, so we need to ensure
--  there is no elaboration code (since this code might not be executed).

WITH System.Storage_Elements;

PACKAGE BODY System.Memory IS
   USE System.Storage_Elements;

   Heap_Start : character;
   FOR Heap_Start'Alignment USE Standard'Maximum_Alignment;
   PRAGMA Import (C, Heap_Start, "__kernel_heap_base");
   --  The address of the variable is the start of the heap

   Heap_End : character;
   PRAGMA Import (C, Heap_End, "__kernel_heap_end");
   --  The address of the variable is the end of the heap

   Top : ALIASED address := Heap_Start'Address;
   --  First not used address (always aligned to the maximum alignment).

   ----------------
   -- For C code --
   ----------------

   FUNCTION Malloc
      (Size : size_t)
      RETURN System.address;
   PRAGMA Export (C, Malloc, "malloc");

   FUNCTION Calloc
      (N_Elem    : size_t;
       Elem_Size : size_t)
      RETURN System.address;
   PRAGMA Export (C, Calloc, "calloc");

   PROCEDURE Free
      (Ptr : System.address);
   PRAGMA Export (C, Free, "free");

   -----------
   -- Alloc --
   -----------

   FUNCTION Alloc
      (Size : size_t)
      RETURN System.address
   IS
      FUNCTION Compare_And_Swap
         (Ptr     : ACCESS address;
          Old_Val : integer_address;
          New_Val : integer_address)
         RETURN boolean;
      PRAGMA Import (Intrinsic, Compare_And_Swap,
          "__sync_bool_compare_and_swap_"
          & (CASE System.Word_Size IS WHEN 32 => "4", WHEN 64 => "8",
              WHEN OTHERS                     => "unexpected"));
      Max_Align : CONSTANT := Standard'Maximum_Alignment;
      Max_Size  : storage_count;
      Res       : address;

   BEGIN
      IF Size = 0
      THEN

         --  Change size from zero to non-zero. We still want a proper pointer
         --  for the zero case because pointers to zero length objects have to
         --  be distinct.

         Max_Size := max_align;

      ELSE
         --  Detect overflow in the addition below. Note that we know that
         --  upper bound of size_t is bigger than the upper bound of
         --  Storage_Count.

         IF Size > size_t (storage_count'Last - max_align)
         THEN
            RAISE Storage_Error;
         END IF;

         --  Compute aligned size

         Max_Size :=
            ((storage_count (Size) + max_align - 1) / max_align) * max_align;
      END IF;

      LOOP
         Res := Top;

         --  Detect too large allocation

         IF Max_Size >= storage_count (Heap_End'Address - Res)
         THEN
            RAISE Storage_Error;
         END IF;

         --  Atomically update the top of the heap. Restart in case of
         --  failure (concurrent allocation).

         EXIT WHEN Compare_And_Swap
               (Top'Access, integer_address (Res),
                integer_address (Res + Max_Size));
      END LOOP;

      RETURN Res;
   END Alloc;

   ------------
   -- Malloc --
   ------------

   FUNCTION Malloc
      (Size : size_t)
      RETURN System.address
   IS
   BEGIN
      RETURN Alloc (Size);
   END Malloc;

   ------------
   -- Calloc --
   ------------

   FUNCTION Calloc
      (N_Elem    : size_t;
       Elem_Size : size_t)
      RETURN System.address
   IS
   BEGIN
      RETURN Malloc (N_Elem * Elem_Size);
   END Calloc;

   ----------
   -- Free --
   ----------

   PROCEDURE Free
      (Ptr : System.address)
   IS
      PRAGMA Unreferenced (Ptr);
   BEGIN
      NULL;
   END Free;

END System.Memory;
