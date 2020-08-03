------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . C A S E _ U T I L                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2019, AdaCore                     --
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

PACKAGE BODY System.Case_Util IS

   --------------
   -- To_Lower --
   --------------

   FUNCTION To_Lower
      (A : character)
      RETURN character
   IS
      A_Val : CONSTANT natural := character'Pos (A);

   BEGIN
      IF A IN 'A' .. 'Z' OR ELSE A_Val IN 16#C0# .. 16#D6#
         OR ELSE A_Val IN 16#D8# .. 16#DE#
      THEN
         RETURN character'Val (A_Val + 16#20#);
      ELSE
         RETURN A;
      END IF;
   END To_Lower;

   PROCEDURE To_Lower
      (A : IN OUT string)
   IS
   BEGIN
      FOR J IN A'Range
      LOOP
         A (J) := To_Lower (A (J));
      END LOOP;
   END To_Lower;

   --------------
   -- To_Mixed --
   --------------

   PROCEDURE To_Mixed
      (A : IN OUT string)
   IS
      Ucase : boolean := true;

   BEGIN
      FOR J IN A'Range
      LOOP
         IF Ucase
         THEN
            A (J) := To_Upper (A (J));
         ELSE
            A (J) := To_Lower (A (J));
         END IF;

         Ucase := A (J) = '_';
      END LOOP;
   END To_Mixed;

   --------------
   -- To_Upper --
   --------------

   FUNCTION To_Upper
      (A : character)
      RETURN character
   IS
      A_Val : CONSTANT natural := character'Pos (A);

   BEGIN
      IF A IN 'a' .. 'z' OR ELSE A_Val IN 16#E0# .. 16#F6#
         OR ELSE A_Val IN 16#F8# .. 16#FE#
      THEN
         RETURN character'Val (A_Val - 16#20#);
      ELSE
         RETURN A;
      END IF;
   END To_Upper;

   PROCEDURE To_Upper
      (A : IN OUT string)
   IS
   BEGIN
      FOR J IN A'Range
      LOOP
         A (J) := To_Upper (A (J));
      END LOOP;
   END To_Upper;

END System.Case_Util;
