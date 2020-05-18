------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           I N T E R F A C E S                            --
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

PRAGMA Compiler_Unit_Warning;

PACKAGE Interfaces IS
   PRAGMA No_Elaboration_Code_All;
   PRAGMA Pure;

   --  All identifiers in this unit are implementation defined

   PRAGMA Implementation_Defined;

   TYPE Integer_8 IS RANGE -2**7 .. 2**7 - 1;
   FOR integer_8'Size USE 8;

   TYPE Integer_16 IS RANGE -2**15 .. 2**15 - 1;
   FOR integer_16'Size USE 16;

   TYPE Integer_32 IS RANGE -2**31 .. 2**31 - 1;
   FOR integer_32'Size USE 32;

   TYPE Integer_64 IS NEW long_long_integer;
   FOR integer_64'Size USE 64;
   --  Note: we use Long_Long_Integer'First instead of -2 ** 63 to allow this
   --  unit to compile when using custom target configuration files where the
   --  maximum integer is 32 bits. This is useful for static analysis tools
   --  such as SPARK or CodePeer. In the normal case Long_Long_Integer is
   --  always 64-bits so we get the desired 64-bit type.

   TYPE Unsigned_8 IS MOD 2**8;
   FOR unsigned_8'Size USE 8;

   TYPE Unsigned_16 IS MOD 2**16;
   FOR unsigned_16'Size USE 16;

   TYPE Unsigned_24 IS MOD 2**24;
   FOR unsigned_24'Size USE 24;
   --  Declare this type for compatibility with legacy Ada compilers.
   --  This is particularly useful in the context of CodePeer analysis.

   TYPE Unsigned_32 IS MOD 2**32;
   FOR unsigned_32'Size USE 32;

   TYPE Unsigned_64 IS MOD 2**long_long_integer'Size;
   FOR unsigned_64'Size USE 64;
   --  See comment on Integer_64 above

   FUNCTION Shift_Left
      (Value  : unsigned_8;
       Amount : natural)
      RETURN unsigned_8;

   FUNCTION Shift_Right
      (Value  : unsigned_8;
       Amount : natural)
      RETURN unsigned_8;

   FUNCTION Shift_Right_Arithmetic
      (Value  : unsigned_8;
       Amount : natural)
      RETURN unsigned_8;

   FUNCTION Rotate_Left
      (Value  : unsigned_8;
       Amount : natural)
      RETURN unsigned_8;

   FUNCTION Rotate_Right
      (Value  : unsigned_8;
       Amount : natural)
      RETURN unsigned_8;

   FUNCTION Shift_Left
      (Value  : unsigned_16;
       Amount : natural)
      RETURN unsigned_16;

   FUNCTION Shift_Right
      (Value  : unsigned_16;
       Amount : natural)
      RETURN unsigned_16;

   FUNCTION Shift_Right_Arithmetic
      (Value  : unsigned_16;
       Amount : natural)
      RETURN unsigned_16;

   FUNCTION Rotate_Left
      (Value  : unsigned_16;
       Amount : natural)
      RETURN unsigned_16;

   FUNCTION Rotate_Right
      (Value  : unsigned_16;
       Amount : natural)
      RETURN unsigned_16;

   FUNCTION Shift_Left
      (Value  : unsigned_32;
       Amount : natural)
      RETURN unsigned_32;

   FUNCTION Shift_Right
      (Value  : unsigned_32;
       Amount : natural)
      RETURN unsigned_32;

   FUNCTION Shift_Right_Arithmetic
      (Value  : unsigned_32;
       Amount : natural)
      RETURN unsigned_32;

   FUNCTION Rotate_Left
      (Value  : unsigned_32;
       Amount : natural)
      RETURN unsigned_32;

   FUNCTION Rotate_Right
      (Value  : unsigned_32;
       Amount : natural)
      RETURN unsigned_32;

   FUNCTION Shift_Left
      (Value  : unsigned_64;
       Amount : natural)
      RETURN unsigned_64;

   FUNCTION Shift_Right
      (Value  : unsigned_64;
       Amount : natural)
      RETURN unsigned_64;

   FUNCTION Shift_Right_Arithmetic
      (Value  : unsigned_64;
       Amount : natural)
      RETURN unsigned_64;

   FUNCTION Rotate_Left
      (Value  : unsigned_64;
       Amount : natural)
      RETURN unsigned_64;

   FUNCTION Rotate_Right
      (Value  : unsigned_64;
       Amount : natural)
      RETURN unsigned_64;

   PRAGMA Import (Intrinsic, Shift_Left);
   PRAGMA Import (Intrinsic, Shift_Right);
   PRAGMA Import (Intrinsic, Shift_Right_Arithmetic);
   PRAGMA Import (Intrinsic, Rotate_Left);
   PRAGMA Import (Intrinsic, Rotate_Right);

   --  IEEE Floating point types

   TYPE IEEE_Float_32 IS DIGITS 6;
   FOR ieee_float_32'Size USE 32;

   TYPE IEEE_Float_64 IS DIGITS 15;
   FOR ieee_float_64'Size USE 64;

   --  If there is an IEEE extended float available on the machine, we assume
   --  that it is available as Long_Long_Float.

   --  Note: it is harmless, and explicitly permitted, to include additional
   --  types in interfaces, so it is not wrong to have IEEE_Extended_Float
   --  defined even if the extended format is not available.

   TYPE IEEE_Extended_Float IS NEW long_long_float;

END Interfaces;
