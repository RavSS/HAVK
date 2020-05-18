------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M .  M E M O R Y _ M O V E                 --
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

PACKAGE BODY System.Memory_Move IS

   -------------
   -- memmove --
   -------------

   FUNCTION memmove
      (Dest : address;
       Src  : address;
       N    : size_t)
      RETURN address
   IS
      D : ia := To_IA (Dest);
      S : ia := To_IA (Src);
      C : ia := ia (N);
   BEGIN
      --  There was an early exit if there are no bytes to copy. There are no
      --  reasons to handle this very rare case specially, as it is handled
      --  correctly by the common path.

      --  This function must handle overlapping memory regions for the source
      --  and destination. If the Dest buffer is located past the Src buffer
      --  then we use backward copying, and forward copying otherwise.

      IF D > S AND THEN D < S + C
      THEN

         --  Backward copy

         D := D + C;
         S := S + C;

         --  Try to copy per word, if alignment constraints are respected

         IF ((D OR S) AND (Word'Alignment - 1)) = 0
         THEN
            WHILE C >= word_unit
            LOOP
               D                   := D - word_unit;
               S                   := S - word_unit;
               To_Word_Ptr (D).ALL := To_Word_Ptr (S).ALL;

               C := C - word_unit;
            END LOOP;
         END IF;

         --  Copy the remainder byte by byte

         WHILE C /= 0
         LOOP
            D                   := D - byte_unit;
            S                   := S - byte_unit;
            To_Byte_Ptr (D).ALL := To_Byte_Ptr (S).ALL;

            C := C - byte_unit;
         END LOOP;
      ELSE
         --  Try to copy per word, if alignment constraints are respected

         IF ((D OR S) AND (Word'Alignment - 1)) = 0
         THEN
            WHILE C >= word_unit
            LOOP
               To_Word_Ptr (D).ALL := To_Word_Ptr (S).ALL;
               D                   := D + word_unit;
               S                   := S + word_unit;
               C                   := C - word_unit;
            END LOOP;
         END IF;

         --  Copy the remainder byte by byte

         WHILE C /= 0
         LOOP
            To_Byte_Ptr (D).ALL := To_Byte_Ptr (S).ALL;
            D                   := D + byte_unit;
            S                   := S + byte_unit;
            C                   := C - byte_unit;
         END LOOP;
      END IF;

      RETURN Dest;
   END memmove;

END System.Memory_Move;
