------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . C A S E _ U T I L                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2019, Free Software Foundation, Inc.         --
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

--  Simple casing functions

--  This package provides simple casing functions that do not require the
--  overhead of the full casing tables found in Ada.Characters.Handling.

--  Note that all the routines in this package are available to the user
--  via GNAT.Case_Util, which imports all the entities from this package.

PRAGMA Compiler_Unit_Warning;

PACKAGE System.Case_Util IS
   PRAGMA Pure;

   --  Note: all the following functions handle the full Latin-1 set

   FUNCTION To_Upper
      (A : character)
      RETURN character;
      --  Converts A to upper case if it is a lower case letter, otherwise
      --  returns the input argument unchanged.

   PROCEDURE To_Upper
      (A : IN OUT string);
   FUNCTION To_Upper
      (A : string)
      RETURN string;
      --  Folds all characters of string A to upper case

   FUNCTION To_Lower
      (A : character)
      RETURN character;
      --  Converts A to lower case if it is an upper case letter, otherwise
      --  returns the input argument unchanged.

   PROCEDURE To_Lower
      (A : IN OUT string);
   FUNCTION To_Lower
      (A : string)
      RETURN string;
      --  Folds all characters of string A to lower case

   PROCEDURE To_Mixed
      (A : IN OUT string);
   FUNCTION To_Mixed
      (A : string)
      RETURN string;
      --  Converts A to mixed case (i.e. lower case, except for initial
      --  character and any character after an underscore, which are
      --  converted to upper case.

END System.Case_Util;
