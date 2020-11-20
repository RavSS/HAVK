------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . C . M A L L O C                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2011-2020, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

WITH
   System.Storage_Elements,
   Ada.Unchecked_Conversion,
   HAVK_Operating_System,
   HAVK_Operating_System.Call;
USE
   HAVK_Operating_System,
   HAVK_Operating_System.Call;

PACKAGE BODY System.C.Malloc IS
   PACKAGE SSE RENAMES System.Storage_Elements;
   USE SSE;

   -- RavSS: I've changed the heap range variables from what were essentially
   -- void pointers into pointers to an address value, along with changing the
   -- symbols to reflect my code style.
   Heap_Start : address := Null_Address;
   --  The address of the variable is the start of the heap

   Heap_End : address := Null_Address;
   --  The address of the variable is the end of the heap

   -- RavSS: I've added an atomic lock to this package so it can be used for
   -- safer multi-tasking.
   TYPE atomic_lock IS MOD 2**64
   WITH
      Atomic => true;
   Lock : ALIASED atomic_lock := 0;
   TYPE big_boolean IS NEW boolean
   WITH
      Size => 64;

   FUNCTION Locked
     (Lock_Pointer  : ACCESS atomic_lock := Lock'access;
      Value_To_Test : big_boolean := true)
      RETURN big_boolean
   WITH
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__sync_lock_test_and_set_8";

   PROCEDURE Release_Lock
     (Lock_Pointer : ACCESS atomic_lock := Lock'access)
   WITH
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__sync_lock_release_8";

   FUNCTION Get_Cell_Data
      (Cell : cell_acc)
       RETURN address;
   PROCEDURE Add_Free_Cell
      (Cell : free_cell_acc);
      --  Add a cell to the free chain

   PROCEDURE Remove_Free_Cell
      (Cell : free_cell_acc);
      --  Remove free cell from free chain

   FUNCTION To_Cell_Acc IS NEW Ada.Unchecked_Conversion (address, cell_acc);
   FUNCTION To_Cell_Acc IS NEW Ada.Unchecked_Conversion
      (free_cell_acc, cell_acc);

   FUNCTION To_Address IS NEW Ada.Unchecked_Conversion (cell_acc, address);

   FUNCTION To_Free_Cell_Acc IS NEW Ada.Unchecked_Conversion
      (cell_acc, free_cell_acc);
   FUNCTION To_Free_Cell_Acc IS NEW Ada.Unchecked_Conversion
      (address, free_cell_acc);

   Cell_Size : CONSTANT SSE.Storage_Offset := cell_type'Size / Storage_Unit;
   Free_Cell_Size : CONSTANT SSE.Storage_Offset :=
      free_cell_type'Size / Storage_Unit;

   --------------------------
   -- RavSS: Alloc_Wrapper --
   --------------------------
   FUNCTION Alloc_Wrapper
     (Size : IN size_t)
      RETURN address
   IS
      Heap_Extension : arguments := (heap_increase_operation, OTHERS => 0);
      Allocation     : address;
   BEGIN
      IF -- Initialise the heap.
         Heap_Start = Null_Address
      THEN
         IF
            System_Call(Heap_Extension) = no_error
         THEN
            Heap_Start := address(Heap_Extension.Argument_1 - 16#1000#);
            Heap_End   := address(Heap_Extension.Argument_1);
         ELSE
            RETURN Null_Address;
         END IF;
      END IF;

      LOOP -- Keep trying and extending until the latter also starts failing.
         Allocation := Alloc(Size);
         IF
            Allocation /= Null_Address
         THEN
            RETURN Allocation;
         ELSIF
            System_Call(Heap_Extension) = no_error
         THEN
            Heap_End := address(Heap_Extension.Argument_1);
         ELSE
            RETURN Null_Address;
         END IF;
      END LOOP;
   END Alloc_Wrapper;

   -------------------
   -- Add_Free_Cell --
   -------------------
   PROCEDURE Add_Free_Cell
      (Cell : free_cell_acc)
   IS
      Next : free_cell_acc;
      Cur  : free_cell_acc;
   BEGIN
      --  Follow the chain until NEXT is larger then CELL

      Next := Free_List;
      Cur  := NULL;

      WHILE Next /= NULL
      LOOP
         EXIT WHEN Next.Cell.Size >= Cell.Cell.Size;
         Cur  := Next;
         Next := Next.Next_Free;
      END LOOP;
      --  Insert

      Cell.Prev_Free := Cur;

      IF Cur = NULL
      THEN
         Cell.Next_Free := Free_List;

         IF Free_List /= NULL
         THEN
            Free_List.Prev_Free := Cell;
         END IF;
         Free_List := Cell;

      ELSE
         Cell.Next_Free := Next;

         IF Next /= NULL
         THEN
            Next.Prev_Free := Cell;
         END IF;
         Cur.Next_Free := Cell;
      END IF;
   END Add_Free_Cell;

   -----------
   -- Alloc --
   -----------
   FUNCTION Alloc
      (Size : size_t)
       RETURN address
   IS
      Rounded_Size : size_t;
   BEGIN
      --  Return null address for zero length request

      IF Size = 0
      THEN
         RETURN Null_Address;
      END IF;

      -- RavSS: Lock the memory manager and stop spinning when it's unlocked.
      LOOP
         EXIT WHEN NOT Locked;
      END LOOP;

      --  Round size up

      Rounded_Size := (Size + Standard'Maximum_Alignment);
      Rounded_Size :=
         Rounded_Size - Rounded_Size REM Standard'Maximum_Alignment;
         --  Find a free cell

      DECLARE
         Res           : free_cell_acc;
         Next_Cell     : free_cell_acc;
         New_Next_Cell : free_cell_acc;
      BEGIN
         Res := Free_List;

         WHILE Res /= NULL
         LOOP
            --  The last cell is not a free cell

            PRAGMA Assert (To_Cell_Acc (Res) /= Last_Cell);

            IF Res.Cell.Size >= Rounded_Size
            THEN
               --  Remove it from the list

               Remove_Free_Cell (Res);
               --  Can we split it?

               IF Res.Cell.Size - Rounded_Size >= size_t (Free_Cell_Size)
               THEN
                  Next_Cell :=
                     To_Free_Cell_Acc (Get_Next_Cell (To_Cell_Acc (Res)));
                  --  Create the new cell

                  New_Next_Cell :=
                     To_Free_Cell_Acc
                        (Get_Cell_Data (To_Cell_Acc (Res)) +
                         Storage_Offset (Rounded_Size));
                  New_Next_Cell.Cell :=
                     (Size =>
                         Res.Cell.Size - Rounded_Size - size_t (Cell_Size),
                      Prev => To_Cell_Acc (Res), Free => True);
                  Next_Cell.Cell.Prev := To_Cell_Acc (New_Next_Cell);
                  --  Resize the returned cell

                  Res.Cell.Size := Rounded_Size;
                  --  Add the new cell to the free list

                  Add_Free_Cell (New_Next_Cell);
               END IF;
               Res.Cell.Free := False;
               Release_Lock; -- RavSS: Unlock it.
               RETURN Get_Cell_Data (To_Cell_Acc (Res));
            END IF;
            Res := Res.Next_Free;
         END LOOP;
      END;
         --  No free block so create a new block

      DECLARE
         Res : cell_acc;
      BEGIN
         IF Last_Cell = NULL
         THEN
            --  Do we need to check alignment ???
            Res := Get_First_Cell;
         ELSE
            Res := Get_Next_Cell (Last_Cell);
         END IF;

         Res.ALL := (Prev => Last_Cell, Size => Rounded_Size, Free => False);
         --  Check heap exhaustion, and if so return null address

         IF To_Address (Get_Next_Cell (Res)) > Heap_End
         THEN
            Release_Lock; -- RavSS: Unlock it.
            RETURN Null_Address;
         END IF;
         Last_Cell := Res;
         Release_Lock; -- RavSS: Unlock it.
         RETURN Get_Cell_Data (Res);
      END;
   END Alloc;

   ----------
   -- Free --
   ----------
   PROCEDURE Free
      (Ptr : address)
   IS
      Cell : cell_acc;
   BEGIN
      -- RavSS: Lock the memory manager and stop spinning when it's unlocked.
      LOOP
         EXIT WHEN NOT Locked;
      END LOOP;

      -- RavSS: I've purposefully made it raise an exception if a null address
      -- is passed, as that should not happen.
      PRAGMA Assert(Ptr /= Null_Address, "Cannot free a null address.");

      Cell := To_Cell_Acc (Ptr - Cell_Size);
      PRAGMA Assert (NOT Cell.Free);
      Cell.Free := True;
      --  If Cell is the last one, free it directly

      IF Cell = Last_Cell
      THEN
         Last_Cell := Cell.Prev;
         --  The one before the last may be free too

         IF Last_Cell /= NULL AND THEN Last_Cell.Free
         THEN
            --  Remove it from the free list

            Remove_Free_Cell (To_Free_Cell_Acc (Last_Cell));
            Last_Cell := Last_Cell.Prev;
            --  There can be only one free block before

            PRAGMA Assert (Last_Cell = NULL OR ELSE NOT Last_Cell.Free);
         END IF;
         Release_Lock; -- RavSS: Unlock it.
         RETURN;
      END IF;
      --  Merge with the next cell?

      IF Cell /= Last_Cell
      THEN
         DECLARE

            Next_Cell : CONSTANT cell_acc := Get_Next_Cell (Cell);

         BEGIN
            IF Next_Cell.Free
            THEN
               --  Remove it from the free list

               Remove_Free_Cell (To_Free_Cell_Acc (Next_Cell));
               --  Do the merge

               IF Next_Cell /= Last_Cell
               THEN
                  Get_Next_Cell (Next_Cell).Prev := Cell;
               END IF;
               Cell.Size := Cell.Size + Next_Cell.Size + size_t (Cell_Size);
            END IF;
         END;
      END IF;
      --  Merge with prev cell?

      IF Cell.Prev /= NULL AND THEN Cell.Prev.Free
      THEN
         DECLARE

            Prev_Cell : CONSTANT cell_acc := Cell.Prev;

         BEGIN
            Remove_Free_Cell (To_Free_Cell_Acc (Prev_Cell));
            --  Do the merge

            IF Cell /= Last_Cell
            THEN
               Get_Next_Cell (Cell).Prev := Prev_Cell;
            END IF;
            Prev_Cell.Size := Prev_Cell.Size + Cell.Size + size_t (Cell_Size);
            Cell           := Prev_Cell;
         END;
      END IF;
      Add_Free_Cell (To_Free_Cell_Acc (Cell));
      Release_Lock; -- RavSS: Unlock it.
   END Free;

   -------------------
   -- Get_Cell_Data --
   -------------------
   FUNCTION Get_Cell_Data
      (Cell : cell_acc)
       RETURN address
   IS
   BEGIN
      RETURN Cell.ALL'Address + Cell_Size;
   END Get_Cell_Data;

   --------------------
   -- Get_First_Cell --
   --------------------
   FUNCTION Get_First_Cell
       RETURN cell_acc
   IS
   BEGIN
      RETURN To_Cell_Acc (Heap_Start);
   END Get_First_Cell;

   -------------------
   -- Get_Next_Cell --
   -------------------
   FUNCTION Get_Next_Cell
      (Cell : cell_acc)
       RETURN cell_acc
   IS
   BEGIN
      RETURN To_Cell_Acc (Get_Cell_Data (Cell) + Storage_Offset (Cell.Size));
   END Get_Next_Cell;

   ----------------------
   -- Remove_Free_Cell --
   ----------------------
   PROCEDURE Remove_Free_Cell
      (Cell : free_cell_acc)
   IS
   BEGIN
      IF Cell.Next_Free /= NULL
      THEN
         Cell.Next_Free.Prev_Free := Cell.Prev_Free;
      END IF;

      IF Cell.Prev_Free /= NULL
      THEN
         Cell.Prev_Free.Next_Free := Cell.Next_Free;

      ELSE
         Free_List := Cell.Next_Free;
      END IF;
   END Remove_Free_Cell;

END System.C.Malloc;
