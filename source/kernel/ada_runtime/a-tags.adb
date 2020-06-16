------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--  This is the HI-E version of this file. Some functionality has been removed
--  in order to simplify this run-time unit.

WITH Ada.Unchecked_Conversion;
WITH System.Storage_Elements;
USE System.Storage_Elements;

PACKAGE BODY Ada.Tags IS
   -----------------------
   -- Local Subprograms --
   -----------------------

FUNCTION Length
      (Str : Cstring_Ptr)
       RETURN Natural;
      --  Length of string represented by the given pointer (treating the
      --  string as a C-style string, which is Nul terminated).

      --  Unchecked Conversions

   FUNCTION To_Addr_Ptr IS NEW Ada.Unchecked_Conversion
      (System.Address, Addr_Ptr);
   FUNCTION To_Address IS NEW Ada.Unchecked_Conversion (Tag, System.Address);
   FUNCTION To_Type_Specific_Data_Ptr IS NEW Ada.Unchecked_Conversion
      (System.Address, Type_Specific_Data_Ptr);
      -------------------
      -- Expanded_Name --
      -------------------

   FUNCTION Expanded_Name
      (T : Tag)
       RETURN String
   IS

      Result  : Cstring_Ptr;
      TSD_Ptr : Addr_Ptr;
      TSD     : Type_Specific_Data_Ptr;

   BEGIN
      IF T = No_Tag
      THEN
         RAISE Tag_Error;
      END IF;
      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     := To_Type_Specific_Data_Ptr (TSD_Ptr.ALL);
      Result  := TSD.Expanded_Name;
      RETURN Result (1 .. Length (Result));
   END Expanded_Name;
      ------------------
      -- External_Tag --
      ------------------

   FUNCTION External_Tag
      (T : Tag)
       RETURN String
   IS

      Result  : Cstring_Ptr;
      TSD_Ptr : Addr_Ptr;
      TSD     : Type_Specific_Data_Ptr;

   BEGIN
      IF T = No_Tag
      THEN
         RAISE Tag_Error;
      END IF;
      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     := To_Type_Specific_Data_Ptr (TSD_Ptr.ALL);
      Result  := TSD.External_Tag;
      RETURN Result (1 .. Length (Result));
   END External_Tag;
      ------------
      -- Length --
      ------------

   FUNCTION Length
      (Str : Cstring_Ptr)
       RETURN Natural
   IS

      Len : Integer;

   BEGIN
      Len := 1;

      WHILE Str (Len) /= character'val(0)
      LOOP
         Len := Len + 1;
      END LOOP;
      RETURN Len - 1;
   END Length;
      ----------------
      -- Parent_Tag --
      ----------------

   FUNCTION Parent_Tag
      (T : Tag)
       RETURN Tag
   IS

      TSD_Ptr : Addr_Ptr;
      TSD     : Type_Specific_Data_Ptr;

   BEGIN
      IF T = No_Tag
      THEN
         RAISE Tag_Error;
      END IF;
      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     := To_Type_Specific_Data_Ptr (TSD_Ptr.ALL);
      --  The Parent_Tag of a root-level tagged type is defined to be No_Tag.
      --  The first entry in the Ancestors_Tags array will be null for such a
      --  type, but it's better to be explicit about returning No_Tag in this
      --  case.

      IF TSD.Idepth = 0
      THEN
         RETURN No_Tag;

      ELSE
         RETURN TSD.Tags_Table (1);
      END IF;
   END Parent_Tag;

END Ada.Tags;
