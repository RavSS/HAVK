------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L B                        --
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

WITH System.Unsigned_Types;
USE System.Unsigned_Types;

PACKAGE BODY System.Img_LLB IS

   ---------------------------------------
   -- Set_Image_Based_Long_Long_Integer --
   ---------------------------------------

   PROCEDURE Set_Image_Based_Long_Long_Integer
      (V :        long_long_integer;
       B :        natural;
       W :        integer;
       S :    OUT string;
       P : IN OUT natural)
   IS
      Start : natural;

   BEGIN
      --  Positive case can just use the unsigned circuit directly

      IF V >= 0
      THEN
         Set_Image_Based_Long_Long_Unsigned
            (long_long_unsigned (V), B, W, S, P);

      --  Negative case has to set a minus sign. Note also that we have to be
      --  careful not to generate overflow with the largest negative number.

      ELSE
         P     := P + 1;
         S (P) := ' ';
         Start := P;

         DECLARE
            PRAGMA Suppress (Overflow_Check);
            PRAGMA Suppress (Range_Check);
         BEGIN
            Set_Image_Based_Long_Long_Unsigned
               (long_long_unsigned (-V), B, W - 1, S, P);
         END;

         --  Set minus sign in last leading blank location. Because of the
         --  code above, there must be at least one such location.

         WHILE S (Start + 1) = ' '
         LOOP
            Start := Start + 1;
         END LOOP;

         S (Start) := '-';
      END IF;

   END Set_Image_Based_Long_Long_Integer;

   ----------------------------------------
   -- Set_Image_Based_Long_Long_Unsigned --
   ----------------------------------------

   PROCEDURE Set_Image_Based_Long_Long_Unsigned
      (V :        long_long_unsigned;
       B :        natural;
       W :        integer;
       S :    OUT string;
       P : IN OUT natural)
   IS
      Start : CONSTANT natural := P;
      F, T  : natural;
      BU    : CONSTANT long_long_unsigned := long_long_unsigned (B);
      Hex   : CONSTANT ARRAY (long_long_unsigned RANGE 0 .. 15) OF character :=
         "0123456789ABCDEF";

      PROCEDURE Set_Digits
         (T : long_long_unsigned);
         --  Set digits of absolute value of T

         ----------------
         -- Set_Digits --
         ----------------

      PROCEDURE Set_Digits
         (T : long_long_unsigned)
      IS
      BEGIN
         IF T >= BU
         THEN
            Set_Digits (T / BU);
            P     := P + 1;
            S (P) := Hex (T MOD BU);
         ELSE
            P     := P + 1;
            S (P) := Hex (T);
         END IF;
      END Set_Digits;

      --  Start of processing for Set_Image_Based_Long_Long_Unsigned

   BEGIN

      IF B >= 10
      THEN
         P     := P + 1;
         S (P) := '1';
      END IF;

      P     := P + 1;
      S (P) := character'Val (character'Pos ('0') + B MOD 10);

      P     := P + 1;
      S (P) := '#';

      Set_Digits (V);

      P     := P + 1;
      S (P) := '#';

      --  Add leading spaces if required by width parameter

      IF P - Start < W
      THEN
         F := P;
         P := Start + W;
         T := P;

         WHILE F > Start
         LOOP
            S (T) := S (F);
            T     := T - 1;
            F     := F - 1;
         END LOOP;

         FOR J IN Start + 1 .. T
         LOOP
            S (J) := ' ';
         END LOOP;
      END IF;

   END Set_Image_Based_Long_Long_Unsigned;

END System.Img_LLB;
