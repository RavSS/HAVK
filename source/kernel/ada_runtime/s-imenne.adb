------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . I M G _ E N U M _ N E W                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2019, Free Software Foundation, Inc.         --
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

WITH Ada.Unchecked_Conversion;

PACKAGE BODY System.Img_Enum_New IS

   -------------------------
   -- Image_Enumeration_8 --
   -------------------------

   PROCEDURE Image_Enumeration_8
      (Pos     :        natural;
       S       : IN OUT string;
       P       :    OUT natural;
       Names   :        string;
       Indexes :        System.address)
   IS
      PRAGMA Assert (S'First = 1);

      TYPE Natural_8 IS RANGE 0 .. 2**7 - 1;
      TYPE Index_Table IS ARRAY (natural) OF natural_8;
      TYPE Index_Table_Ptr IS ACCESS index_table;

      FUNCTION To_Index_Table_Ptr IS NEW Ada.Unchecked_Conversion
         (System.address, index_table_ptr);

      IndexesT : CONSTANT index_table_ptr := To_Index_Table_Ptr (Indexes);

      Start : CONSTANT natural := natural (IndexesT (Pos));
      Next  : CONSTANT natural := natural (IndexesT (Pos + 1));

   BEGIN
      S (1 .. Next - Start) := Names (Start .. Next - 1);
      P                     := Next - Start;
   END Image_Enumeration_8;

   --------------------------
   -- Image_Enumeration_16 --
   --------------------------

   PROCEDURE Image_Enumeration_16
      (Pos     :        natural;
       S       : IN OUT string;
       P       :    OUT natural;
       Names   :        string;
       Indexes :        System.address)
   IS
      PRAGMA Assert (S'First = 1);

      TYPE Natural_16 IS RANGE 0 .. 2**15 - 1;
      TYPE Index_Table IS ARRAY (natural) OF natural_16;
      TYPE Index_Table_Ptr IS ACCESS index_table;

      FUNCTION To_Index_Table_Ptr IS NEW Ada.Unchecked_Conversion
         (System.address, index_table_ptr);

      IndexesT : CONSTANT index_table_ptr := To_Index_Table_Ptr (Indexes);

      Start : CONSTANT natural := natural (IndexesT (Pos));
      Next  : CONSTANT natural := natural (IndexesT (Pos + 1));

   BEGIN
      S (1 .. Next - Start) := Names (Start .. Next - 1);
      P                     := Next - Start;
   END Image_Enumeration_16;

   --------------------------
   -- Image_Enumeration_32 --
   --------------------------

   PROCEDURE Image_Enumeration_32
      (Pos     :        natural;
       S       : IN OUT string;
       P       :    OUT natural;
       Names   :        string;
       Indexes :        System.address)
   IS
      PRAGMA Assert (S'First = 1);

      TYPE Natural_32 IS RANGE 0 .. 2**31 - 1;
      TYPE Index_Table IS ARRAY (natural) OF natural_32;
      TYPE Index_Table_Ptr IS ACCESS index_table;

      FUNCTION To_Index_Table_Ptr IS NEW Ada.Unchecked_Conversion
         (System.address, index_table_ptr);

      IndexesT : CONSTANT index_table_ptr := To_Index_Table_Ptr (Indexes);

      Start : CONSTANT natural := natural (IndexesT (Pos));
      Next  : CONSTANT natural := natural (IndexesT (Pos + 1));

   BEGIN
      S (1 .. Next - Start) := Names (Start .. Next - 1);
      P                     := Next - Start;
   END Image_Enumeration_32;

END System.Img_Enum_New;
