------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . C . M A L L O C                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2011-2020, AdaCore                     --
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

--  A simple implementation of storage allocation (malloc etc) for ZFP use

PRAGMA Restrictions (No_Elaboration_Code);

PACKAGE System.C.Malloc
WITH
   Preelaborate => true
IS
   -- My own wrapper that extends the heap boundary if needed.
   FUNCTION Alloc_Wrapper
     (Size : IN size_t)
      RETURN address
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "__gnat_malloc";

   FUNCTION Alloc
      (Size : size_t)
       RETURN address;
   PROCEDURE Free
      (Ptr : address);
   PRAGMA Export (C, Free, "__gnat_free");

PRIVATE
   --  The basic implementation structures are made private in the spec so that
   --  a child package could add extensions (statistics, consistency checks...)

   TYPE cell_type;
   --  A cell is the header before the chunk of memory. This implementation
   --  uses doubly-linked list of cells.

   TYPE cell_acc IS ACCESS Cell_Type;
   PRAGMA No_Strict_Aliasing (Cell_Acc);
   --  Get rid of strict aliasing error message because we will convert this
   --  access type to address and Free_Cell_Acc.

   SUBTYPE cell_size_t IS size_t RANGE 0 .. 2**(Standard'Address_Size - 2);

   TYPE cell_type IS RECORD
      Prev : Cell_Acc;
      --  The cell just before this one or null if this is the first cell.
      --  There is no Next as this can be deduced from Size.

      Size : Cell_Size_T;
      --  Size of this cell rounded up to multiple of Max_Alignment

      Free : Boolean;
      --  Status flag, used to coalize blocks
   END RECORD;
   PRAGMA Pack (Cell_Type);
   FOR Cell_Type'Size USE 2 * Standard'Address_Size;
   FOR Cell_Type'Alignment USE Standard'Maximum_Alignment;
   TYPE free_cell_type;

   TYPE free_cell_acc IS ACCESS Free_Cell_Type;
   PRAGMA No_Strict_Aliasing (Free_Cell_Acc);
      --  Get rid of strict aliasing error message because we will convert this
      --  access type to address and Cell_Acc.

   TYPE free_cell_type IS RECORD
      Cell : Cell_Type;
      --  Free cells have two additional fields over busy cells

      Prev_Free : Free_Cell_Acc;
      Next_Free : Free_Cell_Acc;
      --  Doubly linked list of free blocks
   END RECORD;
   Free_List : Free_Cell_Acc;
   --  Linked list of free cells ordered by increasing size

   FUNCTION Get_First_Cell
       RETURN Cell_Acc;
   --  The first cell. Valid only if the heap is not empty (which can be
   --  checked with Last_Cell).

   Last_Cell : Cell_Acc;
   --  Last allocated cell (it must not be a free cell)

   FUNCTION Get_Next_Cell
      (Cell : Cell_Acc)
       RETURN Cell_Acc;
      --  Next adjacent cell
END System.C.Malloc;
