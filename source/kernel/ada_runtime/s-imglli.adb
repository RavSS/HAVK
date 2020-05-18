------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L I                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

PACKAGE BODY System.Img_LLI IS

   PROCEDURE Set_Digits
      (T :        long_long_integer;
       S : IN OUT string;
       P : IN OUT natural);
      --  Set digits of absolute value of T, which is zero or negative. We work
      --  with the negative of the value so that the largest negative number is
      --  not a special case.

      -----------------------------
      -- Image_Long_Long_Integer --
      -----------------------------

   PROCEDURE Image_Long_Long_Integer
      (V :        long_long_integer;
       S : IN OUT string;
       P :    OUT natural)
   IS
      PRAGMA Assert (S'First = 1);

   BEGIN
      IF V >= 0
      THEN
         S (1) := ' ';
         P     := 1;
      ELSE
         P := 0;
      END IF;

      Set_Image_Long_Long_Integer (V, S, P);
   END Image_Long_Long_Integer;

   ----------------
   -- Set_Digits --
   ----------------

   PROCEDURE Set_Digits
      (T :        long_long_integer;
       S : IN OUT string;
       P : IN OUT natural)
   IS
   BEGIN
      IF T <= -10
      THEN
         Set_Digits (T / 10, S, P);
         P     := P + 1;
         S (P) := character'Val (48 - (T REM 10));
      ELSE
         P     := P + 1;
         S (P) := character'Val (48 - T);
      END IF;
   END Set_Digits;

   ---------------------------------
   -- Set_Image_Long_Long_Integer --
   --------------------------------

   PROCEDURE Set_Image_Long_Long_Integer
      (V :        long_long_integer;
       S : IN OUT string;
       P : IN OUT natural)
   IS
   BEGIN
      IF V >= 0
      THEN
         Set_Digits (-V, S, P);
      ELSE
         P     := P + 1;
         S (P) := '-';
         Set_Digits (V, S, P);
      END IF;
   END Set_Image_Long_Long_Integer;

END System.Img_LLI;
