------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ U T I L                       --
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

WITH System.Case_Util;
USE System.Case_Util;

PACKAGE BODY System.Val_Util IS

   ---------------
   -- Bad_Value --
   ---------------

   PROCEDURE Bad_Value
      (S : string)
   IS
   BEGIN
      --  Bad_Value might be called with very long strings allocated on the
      --  heap. Limit the size of the message so that we avoid creating a
      --  Storage_Error during error handling.
      IF S'Length > 127
      THEN
         RAISE Constraint_Error
            WITH "bad input for 'Value: """ & S (S'First .. S'First + 127)
            & "...""";
      ELSE
         RAISE Constraint_Error WITH "bad input for 'Value: """ & S & '"';
      END IF;
   END Bad_Value;

   ----------------------
   -- Normalize_String --
   ----------------------

   PROCEDURE Normalize_String
      (S    : IN OUT string;
       F, L :    OUT integer)
   IS
   BEGIN
      F := S'First;
      L := S'Last;

      --  Scan for leading spaces

      WHILE F <= L AND THEN S (F) = ' '
      LOOP
         F := F + 1;
      END LOOP;

      --  Check for case when the string contained no characters

      IF F > L
      THEN
         Bad_Value (S);
      END IF;

      --  Scan for trailing spaces

      WHILE S (L) = ' '
      LOOP
         L := L - 1;
      END LOOP;

      --  Except in the case of a character literal, convert to upper case

      IF S (F) /= '''
      THEN
         FOR J IN F .. L
         LOOP
            S (J) := To_Upper (S (J));
         END LOOP;
      END IF;
   END Normalize_String;

   -------------------
   -- Scan_Exponent --
   -------------------

   FUNCTION Scan_Exponent
      (Str  : string;
       Ptr  : NOT NULL ACCESS integer;
       Max  : integer;
       Real : boolean := false)
      RETURN integer
   IS
      P : natural := Ptr.ALL;
      M : boolean;
      X : integer;

   BEGIN
      IF P >= Max OR ELSE (Str (P) /= 'E' AND THEN Str (P) /= 'e')
      THEN
         RETURN 0;
      END IF;

      --  We have an E/e, see if sign follows

      P := P + 1;

      IF Str (P) = '+'
      THEN
         P := P + 1;

         IF P > Max
         THEN
            RETURN 0;
         ELSE
            M := false;
         END IF;

      ELSIF Str (P) = '-'
      THEN
         P := P + 1;

         IF P > Max OR ELSE NOT Real
         THEN
            RETURN 0;
         ELSE
            M := true;
         END IF;

      ELSE
         M := false;
      END IF;

      IF Str (P) NOT IN '0' .. '9'
      THEN
         RETURN 0;
      END IF;

      --  Scan out the exponent value as an unsigned integer. Values larger
      --  than (Integer'Last / 10) are simply considered large enough here.
      --  This assumption is correct for all machines we know of (e.g. in the
      --  case of 16 bit integers it allows exponents up to 3276, which is
      --  large enough for the largest floating types in base 2.)

      X := 0;

      LOOP
         IF X < (integer'Last / 10)
         THEN
            X := X * 10 + (character'Pos (Str (P)) - character'Pos ('0'));
         END IF;

         P := P + 1;

         EXIT WHEN P > Max;

         IF Str (P) = '_'
         THEN
            Scan_Underscore (Str, P, Ptr, Max, false);
         ELSE
            EXIT WHEN Str (P) NOT IN '0' .. '9';
         END IF;
      END LOOP;

      IF M
      THEN
         X := -X;
      END IF;

      Ptr.ALL := P;
      RETURN X;
   END Scan_Exponent;

   --------------------
   -- Scan_Plus_Sign --
   --------------------

   PROCEDURE Scan_Plus_Sign
      (Str   :     string;
       Ptr   :     NOT NULL ACCESS integer;
       Max   :     integer;
       Start : OUT positive)
   IS
      P : natural := Ptr.ALL;

   BEGIN
      IF P > Max
      THEN
         Bad_Value (Str);
      END IF;

      --  Scan past initial blanks

      WHILE Str (P) = ' '
      LOOP
         P := P + 1;

         IF P > Max
         THEN
            Ptr.ALL := P;
            Bad_Value (Str);
         END IF;
      END LOOP;

      Start := P;

      --  Skip past an initial plus sign

      IF Str (P) = '+'
      THEN
         P := P + 1;

         IF P > Max
         THEN
            Ptr.ALL := Start;
            Bad_Value (Str);
         END IF;
      END IF;

      Ptr.ALL := P;
   END Scan_Plus_Sign;

   ---------------
   -- Scan_Sign --
   ---------------

   PROCEDURE Scan_Sign
      (Str   :     string;
       Ptr   :     NOT NULL ACCESS integer;
       Max   :     integer;
       Minus : OUT boolean;
       Start : OUT positive)
   IS
      P : natural := Ptr.ALL;

   BEGIN
      --  Deal with case of null string (all blanks). As per spec, we raise
      --  constraint error, with Ptr unchanged, and thus > Max.

      IF P > Max
      THEN
         Bad_Value (Str);
      END IF;

      --  Scan past initial blanks

      WHILE Str (P) = ' '
      LOOP
         P := P + 1;

         IF P > Max
         THEN
            Ptr.ALL := P;
            Bad_Value (Str);
         END IF;
      END LOOP;

      Start := P;

      --  Remember an initial minus sign

      IF Str (P) = '-'
      THEN
         Minus := true;
         P     := P + 1;

         IF P > Max
         THEN
            Ptr.ALL := Start;
            Bad_Value (Str);
         END IF;

         --  Skip past an initial plus sign

      ELSIF Str (P) = '+'
      THEN
         Minus := false;
         P     := P + 1;

         IF P > Max
         THEN
            Ptr.ALL := Start;
            Bad_Value (Str);
         END IF;

      ELSE
         Minus := false;
      END IF;

      Ptr.ALL := P;
   END Scan_Sign;

   --------------------------
   -- Scan_Trailing_Blanks --
   --------------------------

   PROCEDURE Scan_Trailing_Blanks
      (Str : string;
       P   : positive)
   IS
   BEGIN
      FOR J IN P .. Str'Last
      LOOP
         IF Str (J) /= ' '
         THEN
            Bad_Value (Str);
         END IF;
      END LOOP;
   END Scan_Trailing_Blanks;

   ---------------------
   -- Scan_Underscore --
   ---------------------

   PROCEDURE Scan_Underscore
      (Str :        string;
       P   : IN OUT natural;
       Ptr :        NOT NULL ACCESS integer;
       Max :        integer;
       Ext :        boolean)
   IS
      C : character;

   BEGIN
      P := P + 1;

      --  If underscore is at the end of string, then this is an error and we
      --  raise Constraint_Error, leaving the pointer past the underscore. This
      --  seems a bit strange. It means e.g. that if the field is:

      --    345_

      --  that Constraint_Error is raised. You might think that the RM in this
      --  case would scan out the 345 as a valid integer, leaving the pointer
      --  at the underscore, but the ACVC suite clearly requires an error in
      --  this situation (see for example CE3704M).

      IF P > Max
      THEN
         Ptr.ALL := P;
         Bad_Value (Str);
      END IF;

      --  Similarly, if no digit follows the underscore raise an error. This
      --  also catches the case of double underscore which is also an error.

      C := Str (P);

      IF C IN '0' .. '9'
         OR ELSE (Ext AND THEN (C IN 'A' .. 'F' OR ELSE C IN 'a' .. 'f'))
      THEN
         RETURN;
      ELSE
         Ptr.ALL := P;
         Bad_Value (Str);
      END IF;
   END Scan_Underscore;

END System.Val_Util;
