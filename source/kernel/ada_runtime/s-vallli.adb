------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ L L I                        --
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
WITH System.Val_LLU;
USE System.Val_LLU;
WITH System.Val_Util;
USE System.Val_Util;

PACKAGE BODY System.Val_LLI IS

   ----------------------------
   -- Scan_Long_Long_Integer --
   ----------------------------

   FUNCTION Scan_Long_Long_Integer
      (Str : string;
       Ptr : NOT NULL ACCESS integer;
       Max : integer)
      RETURN long_long_integer
   IS
      Uval : long_long_unsigned;
      --  Unsigned result

      Minus : boolean := false;
      --  Set to True if minus sign is present, otherwise to False

      Start : positive;
      --  Saves location of first non-blank

   BEGIN
      Scan_Sign (Str, Ptr, Max, Minus, Start);

      IF Str (Ptr.ALL) NOT IN '0' .. '9'
      THEN
         Ptr.ALL := Start;
         Bad_Value (Str);
      END IF;

      Uval := Scan_Raw_Long_Long_Unsigned (Str, Ptr, Max);

      --  Deal with overflow cases, and also with maximum negative number

      IF Uval > long_long_unsigned (long_long_integer'Last)
      THEN
         IF Minus
            AND THEN Uval = long_long_unsigned (-(long_long_integer'First))
         THEN
            RETURN long_long_integer'First;
         ELSE
            Bad_Value (Str);
         END IF;

         --  Negative values

      ELSIF Minus
      THEN
         RETURN -(long_long_integer (Uval));

         --  Positive values

      ELSE
         RETURN long_long_integer (Uval);
      END IF;
   END Scan_Long_Long_Integer;

   -----------------------------
   -- Value_Long_Long_Integer --
   -----------------------------

   FUNCTION Value_Long_Long_Integer
      (Str : string)
      RETURN long_long_integer
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
            RETURN Value_Long_Long_Integer (nt (Str));
         END;

         --  Normal case where Str'Last < Positive'Last

      ELSE
         DECLARE
            V : long_long_integer;
            P : ALIASED integer := Str'First;
         BEGIN
            V := Scan_Long_Long_Integer (Str, P'Access, Str'Last);
            Scan_Trailing_Blanks (Str, P);
            RETURN V;
         END;
      END IF;
   END Value_Long_Long_Integer;

END System.Val_LLI;
