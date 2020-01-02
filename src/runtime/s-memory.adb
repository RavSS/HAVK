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

pragma Restrictions (No_Elaboration_Code);
--  This unit may be linked without being with'ed, so we need to ensure
--  there is no elaboration code (since this code might not be executed).

with System.Storage_Elements;

package body System.Memory is
   use System.Storage_Elements;

   Heap_Start : Character;
   for Heap_Start'Alignment use Standard'Maximum_Alignment;
   pragma Import (C, Heap_Start, "__kernel_heap_base");
   --  The address of the variable is the start of the heap

   Heap_End : Character;
   pragma Import (C, Heap_End, "__kernel_heap_end");
   --  The address of the variable is the end of the heap

   Top : aliased Address := Heap_Start'Address;
   --  First not used address (always aligned to the maximum alignment).

   ----------------
   -- For C code --
   ----------------

   function Malloc (Size : size_t) return System.Address;
   pragma Export (C, Malloc, "malloc");

   function Calloc (N_Elem : size_t; Elem_Size : size_t) return System.Address;
   pragma Export (C, Calloc, "calloc");

   procedure Free (Ptr : System.Address);
   pragma Export (C, Free, "free");

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address
   is
      function Compare_And_Swap
        (Ptr     : access Address;
         Old_Val : Integer_Address;
         New_Val : Integer_Address) return Boolean;
      pragma Import (Intrinsic, Compare_And_Swap,
                     "__sync_bool_compare_and_swap_" &
                       (case System.Word_Size is
                          when 32 => "4",
                          when 64 => "8",
                          when others => "unexpected"));
      Max_Align : constant := Standard'Maximum_Alignment;
      Max_Size  : Storage_Count;
      Res       : Address;

   begin
      if Size = 0 then

         --  Change size from zero to non-zero. We still want a proper pointer
         --  for the zero case because pointers to zero length objects have to
         --  be distinct.

         Max_Size := Max_Align;

      else
         --  Detect overflow in the addition below. Note that we know that
         --  upper bound of size_t is bigger than the upper bound of
         --  Storage_Count.

         if Size > size_t (Storage_Count'Last - Max_Align) then
            raise Storage_Error;
         end if;

         --  Compute aligned size

         Max_Size :=
           ((Storage_Count (Size) + Max_Align - 1) / Max_Align) * Max_Align;
      end if;

      loop
         Res := Top;

         --  Detect too large allocation

         if Max_Size >= Storage_Count (Heap_End'Address - Res) then
            raise Storage_Error;
         end if;

         --  Atomically update the top of the heap. Restart in case of
         --  failure (concurrent allocation).

         exit when Compare_And_Swap
           (Top'Access,
            Integer_Address (Res),
            Integer_Address (Res + Max_Size));
      end loop;

      return Res;
   end Alloc;

   ------------
   -- Malloc --
   ------------

   function Malloc (Size : size_t) return System.Address is
   begin
      return Alloc (Size);
   end Malloc;

   ------------
   -- Calloc --
   ------------

   function Calloc
     (N_Elem : size_t; Elem_Size : size_t) return System.Address
   is
   begin
      return Malloc (N_Elem * Elem_Size);
   end Calloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
      pragma Unreferenced (Ptr);
   begin
      null;
   end Free;

end System.Memory;
