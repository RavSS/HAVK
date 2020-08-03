------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M .  M E M O R Y _ M O V E                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006-2019, Free Software Foundation, Inc.       --
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

--  This package provides a general block copy mechanism analogous to that
--  provided by the C routine memmove allowing for copies with overlap.

WITH System.Memory_Types;

PACKAGE System.Memory_Move IS
   PRAGMA No_Elaboration_Code_All;
   PRAGMA Preelaborate;

   FUNCTION memmove
      (Dest : address;
       Src  : address;
       N    : Memory_Types.size_t)
      RETURN address;
   PRAGMA Export (C, memmove, "memmove");
   --  Copies N storage units from area starting at Src to area starting
   --  at Dest without any check for buffer overflow. The difference between
   --  this memmove and memcpy is that with memmove, the storage areas may
   --  overlap (forwards or backwards) and the result is correct (i.e. it
   --  is as if Src is first moved to a temporary area, and then this area
   --  is copied to Dst in a separate step).

END System.Memory_Move;
