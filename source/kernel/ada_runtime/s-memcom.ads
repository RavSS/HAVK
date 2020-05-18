------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M .  M E M O R Y _ C O M P A R E              --
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

WITH System.Memory_Types;

PACKAGE System.Memory_Compare IS
   PRAGMA No_Elaboration_Code_All;
   PRAGMA Preelaborate;

   FUNCTION memcmp
      (S1 : address;
       S2 : address;
       N  : Memory_Types.size_t)
      RETURN integer;
   PRAGMA Export (C, memcmp, "memcmp");
   --  Compares the first n bytes of the memory areas s1 and s2.  It returns
   --  an integer less than, equal  to,  or  greater  than zero  if  s1  is
   --  found, respectively, to be less than, to match, or be greater than s2.

END System.Memory_Compare;
