------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                      S Y S T E M . C O N C A T _ 2                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008-2019, Free Software Foundation, Inc.       --
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

PRAGMA Compiler_Unit_Warning;

PACKAGE BODY System.Concat_2 IS

   PRAGMA Suppress (All_Checks);

   ------------------
   -- Str_Concat_2 --
   ------------------

   PROCEDURE Str_Concat_2
      (R      : OUT string;
       S1, S2 :     string)
   IS
      F, L : natural;

   BEGIN
      F          := R'First;
      L          := F + S1'Length - 1;
      R (F .. L) := S1;

      F          := L + 1;
      L          := R'Last;
      R (F .. L) := S2;
   END Str_Concat_2;

   -------------------------
   -- Str_Concat_Bounds_2 --
   -------------------------

   PROCEDURE Str_Concat_Bounds_2
      (Lo, Hi : OUT natural;
       S1, S2 :     string)
   IS
   BEGIN
      IF S1 = ""
      THEN
         Lo := S2'First;
         Hi := S2'Last;
      ELSE
         Lo := S1'First;
         Hi := S1'Last + S2'Length;
      END IF;
   END Str_Concat_Bounds_2;

END System.Concat_2;
