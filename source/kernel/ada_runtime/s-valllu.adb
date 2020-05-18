------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ L L U                        --
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
WITH System.Val_Util;
USE System.Val_Util;

PACKAGE BODY System.Val_LLU IS

   ---------------------------------
   -- Scan_Raw_Long_Long_Unsigned --
   ---------------------------------

   FUNCTION Scan_Raw_Long_Long_Unsigned
      (Str : string;
       Ptr : NOT NULL ACCESS integer;
       Max : integer)
      RETURN long_long_unsigned
   IS
      P : integer;
      --  Local copy of the pointer

      Uval : long_long_unsigned;
      --  Accumulated unsigned integer result

      Expon : integer;
      --  Exponent value

      Overflow : boolean := false;
      --  Set True if overflow is detected at any point

      Base_Char : character;
      --  Base character (# or :) in based case

      Base : long_long_unsigned := 10;
      --  Base value (reset in based case)

      Digit : long_long_unsigned;
      --  Digit value

   BEGIN
      --  We do not tolerate strings with Str'Last = Positive'Last

      IF Str'Last = positive'Last
      THEN
         RAISE Program_Error
            WITH "string upper bound is Positive'Last, not supported";
      END IF;

      P    := Ptr.ALL;
      Uval := character'Pos (Str (P)) - character'Pos ('0');
      P    := P + 1;

      --  Scan out digits of what is either the number or the base.
      --  In either case, we are definitely scanning out in base 10.

      DECLARE
         Umax : CONSTANT := (long_long_unsigned'Last - 9) / 10;
         --  Max value which cannot overflow on accumulating next digit

         Umax10 : CONSTANT := long_long_unsigned'Last / 10;
         --  Numbers bigger than Umax10 overflow if multiplied by 10

      BEGIN
         --  Loop through decimal digits
         LOOP
            EXIT WHEN P > Max;

            Digit := character'Pos (Str (P)) - character'Pos ('0');

            --  Non-digit encountered

            IF Digit > 9
            THEN
               IF Str (P) = '_'
               THEN
                  Scan_Underscore (Str, P, Ptr, Max, false);
               ELSE
                  EXIT;
               END IF;

               --  Accumulate result, checking for overflow

            ELSE
               IF Uval <= umax
               THEN
                  Uval := 10 * Uval + Digit;

               ELSIF Uval > umax10
               THEN
                  Overflow := true;

               ELSE
                  Uval := 10 * Uval + Digit;

                  IF Uval < umax10
                  THEN
                     Overflow := true;
                  END IF;
               END IF;

               P := P + 1;
            END IF;
         END LOOP;
      END;

      Ptr.ALL := P;

      --  Deal with based case. We recognize either the standard '#' or the
      --  allowed alternative replacement ':' (see RM J.2(3)).

      IF P < Max AND THEN (Str (P) = '#' OR ELSE Str (P) = ':')
      THEN
         Base_Char := Str (P);
         P         := P + 1;
         Base      := Uval;
         Uval      := 0;

         --  Check base value. Overflow is set True if we find a bad base, or
         --  a digit that is out of range of the base. That way, we scan out
         --  the numeral that is still syntactically correct, though illegal.
         --  We use a safe base of 16 for this scan, to avoid zero divide.

         IF Base NOT IN 2 .. 16
         THEN
            Overflow := true;
            Base     := 16;
         END IF;

         --  Scan out based integer

         DECLARE
            Umax : CONSTANT long_long_unsigned :=
               (long_long_unsigned'Last - Base + 1) / Base;
            --  Max value which cannot overflow on accumulating next digit

            UmaxB : CONSTANT long_long_unsigned :=
               long_long_unsigned'Last / Base;
            --  Numbers bigger than UmaxB overflow if multiplied by base

         BEGIN
            --  Loop to scan out based integer value

            LOOP
               --  We require a digit at this stage

               IF Str (P) IN '0' .. '9'
               THEN
                  Digit := character'Pos (Str (P)) - character'Pos ('0');

               ELSIF Str (P) IN 'A' .. 'F'
               THEN
                  Digit :=
                     character'Pos (Str (P)) - (character'Pos ('A') - 10);

               ELSIF Str (P) IN 'a' .. 'f'
               THEN
                  Digit :=
                     character'Pos (Str (P)) - (character'Pos ('a') - 10);

                  --  If we don't have a digit, then this is not a based number
                  --  after all, so we use the value we scanned out as the base
                  --  (now in Base), and the pointer to the base character was
                  --  already stored in Ptr.all.

               ELSE
                  Uval := Base;
                  EXIT;
               END IF;

               --  If digit is too large, just signal overflow and continue.
               --  The idea here is to keep scanning as long as the input is
               --  syntactically valid, even if we have detected overflow

               IF Digit >= Base
               THEN
                  Overflow := true;

                  --  Here we accumulate the value, checking overflow

               ELSIF Uval <= Umax
               THEN
                  Uval := Base * Uval + Digit;

               ELSIF Uval > UmaxB
               THEN
                  Overflow := true;

               ELSE
                  Uval := Base * Uval + Digit;

                  IF Uval < UmaxB
                  THEN
                     Overflow := true;
                  END IF;
               END IF;

               --  If at end of string with no base char, not a based number
               --  but we signal Constraint_Error and set the pointer past
               --  the end of the field, since this is what the ACVC tests
               --  seem to require, see CE3704N, line 204.

               P := P + 1;

               IF P > Max
               THEN
                  Ptr.ALL := P;
                  Bad_Value (Str);
               END IF;

               --  If terminating base character, we are done with loop

               IF Str (P) = Base_Char
               THEN
                  Ptr.ALL := P + 1;
                  EXIT;

                  --  Deal with underscore

               ELSIF Str (P) = '_'
               THEN
                  Scan_Underscore (Str, P, Ptr, Max, true);
               END IF;

            END LOOP;
         END;
      END IF;

      --  Come here with scanned unsigned value in Uval. The only remaining
      --  required step is to deal with exponent if one is present.

      Expon := Scan_Exponent (Str, Ptr, Max);

      IF Expon /= 0 AND THEN Uval /= 0
      THEN

         --  For non-zero value, scale by exponent value. No need to do this
         --  efficiently, since use of exponent in integer literals is rare,
         --  and in any case the exponent cannot be very large.

         DECLARE
            UmaxB : CONSTANT long_long_unsigned :=
               long_long_unsigned'Last / Base;
            --  Numbers bigger than UmaxB overflow if multiplied by base

         BEGIN
            FOR J IN 1 .. Expon
            LOOP
               IF Uval > UmaxB
               THEN
                  Overflow := true;
                  EXIT;
               END IF;

               Uval := Uval * Base;
            END LOOP;
         END;
      END IF;

      --  Return result, dealing with sign and overflow

      IF Overflow
      THEN
         Bad_Value (Str);
      ELSE
         RETURN Uval;
      END IF;
   END Scan_Raw_Long_Long_Unsigned;

   -----------------------------
   -- Scan_Long_Long_Unsigned --
   -----------------------------

   FUNCTION Scan_Long_Long_Unsigned
      (Str : string;
       Ptr : NOT NULL ACCESS integer;
       Max : integer)
      RETURN long_long_unsigned
   IS
      Start : positive;
      --  Save location of first non-blank character

   BEGIN
      Scan_Plus_Sign (Str, Ptr, Max, Start);

      IF Str (Ptr.ALL) NOT IN '0' .. '9'
      THEN
         Ptr.ALL := Start;
         RAISE Constraint_Error;
      END IF;

      RETURN Scan_Raw_Long_Long_Unsigned (Str, Ptr, Max);
   END Scan_Long_Long_Unsigned;

   ------------------------------
   -- Value_Long_Long_Unsigned --
   ------------------------------

   FUNCTION Value_Long_Long_Unsigned
      (Str : string)
      RETURN long_long_unsigned
   IS
   BEGIN
      --  We have to special case Str'Last = Positive'Last because the normal
      --  circuit ends up setting P to Str'Last + 1 which is out of bounds. We
      --  deal with this by converting to a subtype which fixes the bounds.

      IF Str'Last = positive'Last
      THEN
         DECLARE
            SUBTYPE NT IS string (1 .. Str'Length);
         BEGIN
            RETURN Value_Long_Long_Unsigned (nt (Str));
         END;

         --  Normal case where Str'Last < Positive'Last

      ELSE
         DECLARE
            V : long_long_unsigned;
            P : ALIASED integer := Str'First;
         BEGIN
            V := Scan_Long_Long_Unsigned (Str, P'Access, Str'Last);
            Scan_Trailing_Blanks (Str, P);
            RETURN V;
         END;
      END IF;
   END Value_Long_Long_Unsigned;

END System.Val_LLU;
