------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M .  M E M O R Y _ C O M P A R E              --
--                                                                          --
--                                 B o d y                                  --
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
USE System.Memory_Types;

PACKAGE BODY System.Memory_Compare IS

   ------------
   -- memcmp --
   ------------

   FUNCTION memcmp
      (S1 : address;
       S2 : address;
       N  : size_t)
      RETURN integer
   IS
      S1_A   : ia     := To_IA (S1);
      S2_A   : ia     := To_IA (S2);
      C      : size_t := N;
      V1, V2 : byte;

   BEGIN
      --  Try to compare word by word if alignment constraints are respected.
      --  Compare as long as words are equal.

      IF ((S1_A OR S2_A) AND (Word'Alignment - 1)) = 0
      THEN
         WHILE C >= word_unit
         LOOP
            EXIT WHEN To_Word_Ptr (S1_A).ALL /= To_Word_Ptr (S2_A).ALL;
            S1_A := S1_A + word_unit;
            S2_A := S2_A + word_unit;
            C    := C - word_unit;
         END LOOP;
      END IF;

      --  Finish byte per byte

      WHILE C > 0
      LOOP
         V1 := To_Byte_Ptr (S1_A).ALL;
         V2 := To_Byte_Ptr (S2_A).ALL;
         IF V1 < V2
         THEN
            RETURN -1;
         ELSIF V1 > V2
         THEN
            RETURN 1;
         END IF;

         S1_A := S1_A + byte_unit;
         S2_A := S2_A + byte_unit;
         C    := C - byte_unit;
      END LOOP;

      RETURN 0;
   END memcmp;

END System.Memory_Compare;
