------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M .  M E M O R Y _ S E T                   --
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

WITH System;
USE System;
WITH System.Memory_Types;
USE System.Memory_Types;

PACKAGE BODY System.Memory_Set IS

   FUNCTION Shift_Left
      (V      : word;
       Amount : natural)
      RETURN word;
   PRAGMA Import (Intrinsic, Shift_Left);

   ------------
   -- memset --
   ------------

   FUNCTION memset
      (M    : address;
       C    : integer;
       Size : size_t)
      RETURN address
   IS
      B  : CONSTANT byte := byte (C MOD 256);
      D  : ia            := To_IA (M);
      N  : size_t        := Size;
      CW : word;

   BEGIN
      --  Try to set per word, if alignment constraints are respected

      IF (D AND (Word'Alignment - 1)) = 0
      THEN
         CW := word (B);
         CW := Shift_Left (CW, 8) OR CW;
         CW := Shift_Left (CW, 16) OR CW;

         --  For 64 bit machine (condition is always true/false)
         PRAGMA Warnings (Off);
         IF word_unit > 4
         THEN
            CW := Shift_Left (CW, 32) OR CW;
         END IF;
         PRAGMA Warnings (On);

         WHILE N >= word_unit
         LOOP
            To_Word_Ptr (D).ALL := CW;
            N                   := N - word_unit;
            D                   := D + word_unit;
         END LOOP;
      END IF;

      --  Set the remaining byte per byte

      WHILE N > 0
      LOOP
         To_Byte_Ptr (D).ALL := B;
         N                   := N - byte_unit;
         D                   := D + byte_unit;
      END LOOP;

      RETURN M;
   END memset;

END System.Memory_Set;
