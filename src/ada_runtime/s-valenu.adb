------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ E N U M                       --
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

WITH Ada.Unchecked_Conversion;

WITH System.Val_Util;
USE System.Val_Util;

PACKAGE BODY System.Val_Enum IS

   -------------------------
   -- Value_Enumeration_8 --
   -------------------------

   FUNCTION Value_Enumeration_8
      (Names   : string;
       Indexes : System.address;
       Num     : natural;
       Str     : string)
      RETURN natural
   IS
      F : natural;
      L : natural;
      S : string (Str'Range) := Str;

      TYPE Natural_8 IS RANGE 0 .. 2**7 - 1;
      TYPE Index_Table IS ARRAY (natural) OF natural_8;
      TYPE Index_Table_Ptr IS ACCESS index_table;

      FUNCTION To_Index_Table_Ptr IS NEW Ada.Unchecked_Conversion
         (System.address, index_table_ptr);

      IndexesT : CONSTANT index_table_ptr := To_Index_Table_Ptr (Indexes);

   BEGIN
      Normalize_String (S, F, L);

      FOR J IN 0 .. Num
      LOOP
         IF Names (natural (IndexesT (J)) .. natural (IndexesT (J + 1)) - 1)
            = S (F .. L)
         THEN
            RETURN J;
         END IF;
      END LOOP;

      Bad_Value (Str);
   END Value_Enumeration_8;

   --------------------------
   -- Value_Enumeration_16 --
   --------------------------

   FUNCTION Value_Enumeration_16
      (Names   : string;
       Indexes : System.address;
       Num     : natural;
       Str     : string)
      RETURN natural
   IS
      F : natural;
      L : natural;
      S : string (Str'Range) := Str;

      TYPE Natural_16 IS RANGE 0 .. 2**15 - 1;
      TYPE Index_Table IS ARRAY (natural) OF natural_16;
      TYPE Index_Table_Ptr IS ACCESS index_table;

      FUNCTION To_Index_Table_Ptr IS NEW Ada.Unchecked_Conversion
         (System.address, index_table_ptr);

      IndexesT : CONSTANT index_table_ptr := To_Index_Table_Ptr (Indexes);

   BEGIN
      Normalize_String (S, F, L);

      FOR J IN 0 .. Num
      LOOP
         IF Names (natural (IndexesT (J)) .. natural (IndexesT (J + 1)) - 1)
            = S (F .. L)
         THEN
            RETURN J;
         END IF;
      END LOOP;

      Bad_Value (Str);
   END Value_Enumeration_16;

   --------------------------
   -- Value_Enumeration_32 --
   --------------------------

   FUNCTION Value_Enumeration_32
      (Names   : string;
       Indexes : System.address;
       Num     : natural;
       Str     : string)
      RETURN natural
   IS
      F : natural;
      L : natural;
      S : string (Str'Range) := Str;

      TYPE Natural_32 IS RANGE 0 .. 2**31 - 1;
      TYPE Index_Table IS ARRAY (natural) OF natural_32;
      TYPE Index_Table_Ptr IS ACCESS index_table;

      FUNCTION To_Index_Table_Ptr IS NEW Ada.Unchecked_Conversion
         (System.address, index_table_ptr);

      IndexesT : CONSTANT index_table_ptr := To_Index_Table_Ptr (Indexes);

   BEGIN
      Normalize_String (S, F, L);

      FOR J IN 0 .. Num
      LOOP
         IF Names (natural (IndexesT (J)) .. natural (IndexesT (J + 1)) - 1)
            = S (F .. L)
         THEN
            RETURN J;
         END IF;
      END LOOP;

      Bad_Value (Str);
   END Value_Enumeration_32;

END System.Val_Enum;
