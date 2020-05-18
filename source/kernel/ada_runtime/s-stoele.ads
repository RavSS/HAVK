------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the implementation dependent sections of this file.      --
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

--  Warning: declarations in this package are ambiguous with respect to the
--  extra declarations that can be introduced into System using Extend_System.
--  It is a good idea to avoid use clauses for this package.

PRAGMA Compiler_Unit_Warning;

PACKAGE System.Storage_Elements IS
   PRAGMA Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada 2005,
   --  this is Pure in any case (AI-362).

   --  We also add the pragma Pure_Function to the operations in this package,
   --  because otherwise functions with parameters derived from Address are
   --  treated as non-pure by the back-end (see exp_ch6.adb). This is because
   --  in many cases such a parameter is used to hide read/out access to
   --  objects, and it would be unsafe to treat such functions as pure.

   TYPE Storage_Offset IS
      RANGE -(2**(integer'(Standard'Address_Size) - 1)) ..
            +(2**(integer'(Standard'Address_Size) - 1))
            - long_long_integer'(1);
   --  Note: the reason for the Long_Long_Integer qualification here is to
   --  avoid a bogus ambiguity when this unit is analyzed in an rtsfind
   --  context. It may be possible to remove this in the future, but it is
   --  certainly harmless in any case ???

   SUBTYPE Storage_Count IS storage_offset RANGE 0 .. storage_offset'Last;

   TYPE Storage_Element IS MOD 2**storage_unit;
   FOR storage_element'Size USE storage_unit;

   PRAGMA Universal_Aliasing (Storage_Element);
   --  This type is used by the expander to implement aggregate copy

   TYPE Storage_Array IS
      ARRAY (storage_offset RANGE <>) OF ALIASED storage_element;
   FOR storage_array'Component_Size USE storage_unit;

   --  Address arithmetic

   FUNCTION "+"
      (Left  : address;
       Right : storage_offset)
      RETURN address;
   PRAGMA Convention (Intrinsic, "+");
   PRAGMA Inline_Always ("+");
   PRAGMA Pure_Function ("+");

   FUNCTION "+"
      (Left  : storage_offset;
       Right : address)
      RETURN address;
   PRAGMA Convention (Intrinsic, "+");
   PRAGMA Inline_Always ("+");
   PRAGMA Pure_Function ("+");

   FUNCTION "-"
      (Left  : address;
       Right : storage_offset)
      RETURN address;
   PRAGMA Convention (Intrinsic, "-");
   PRAGMA Inline_Always ("-");
   PRAGMA Pure_Function ("-");

   FUNCTION "-"
      (Left, Right : address)
      RETURN storage_offset;
   PRAGMA Convention (Intrinsic, "-");
   PRAGMA Inline_Always ("-");
   PRAGMA Pure_Function ("-");

   FUNCTION "mod"
      (Left  : address;
       Right : storage_offset)
      RETURN storage_offset;
   PRAGMA Convention (Intrinsic, "mod");
   PRAGMA Inline_Always ("mod");
   PRAGMA Pure_Function ("mod");

   --  Conversion to/from integers

   TYPE Integer_Address IS MOD memory_size;

   FUNCTION To_Address
      (Value : integer_address)
      RETURN address;
   PRAGMA Convention (Intrinsic, To_Address);
   PRAGMA Inline_Always (To_Address);
   PRAGMA Pure_Function (To_Address);

   FUNCTION To_Integer
      (Value : address)
      RETURN integer_address;
   PRAGMA Convention (Intrinsic, To_Integer);
   PRAGMA Inline_Always (To_Integer);
   PRAGMA Pure_Function (To_Integer);

END System.Storage_Elements;
