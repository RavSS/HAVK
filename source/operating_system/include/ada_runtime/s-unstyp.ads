------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--                S Y S T E M . U N S I G N E D _ T Y P E S                 --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains definitions of standard unsigned types that
--  correspond in size to the standard signed types declared in Standard,
--  and (unlike the types in Interfaces) have corresponding names. It
--  also contains some related definitions for other specialized types
--  used by the compiler in connection with packed array types.

PRAGMA Compiler_Unit_Warning;

PACKAGE System.Unsigned_Types IS
   PRAGMA Pure;
   PRAGMA No_Elaboration_Code_All;

   TYPE Short_Short_Unsigned IS MOD 2**short_short_integer'Size;
   TYPE Short_Unsigned IS MOD 2**short_integer'Size;
   TYPE Unsigned IS MOD 2**integer'Size;
   TYPE Long_Unsigned IS MOD 2**long_integer'Size;
   TYPE Long_Long_Unsigned IS MOD 2**long_long_integer'Size;

   TYPE Float_Unsigned IS MOD 2**float'Size;
   --  Used in the implementation of Is_Negative intrinsic (see Exp_Intr)

   TYPE Packed_Byte IS MOD 2**8;
   PRAGMA Universal_Aliasing (Packed_Byte);
   FOR packed_byte'Size USE 8;
   --  Component type for Packed_Bytes1, Packed_Bytes2 and Packed_Byte4 arrays.
   --  As this type is used by the compiler to implement operations on user
   --  packed array, it needs to be able to alias any type.

   TYPE Packed_Bytes1 IS ARRAY (natural RANGE <>) OF ALIASED packed_byte;
   FOR packed_bytes1'Alignment USE 1;
   FOR packed_bytes1'Component_Size USE packed_byte'Size;
   PRAGMA Suppress_Initialization (Packed_Bytes1);
   --  This is the type used to implement packed arrays where no alignment
   --  is required. This includes the cases of 1,2,4 (where we use direct
   --  masking operations), and all odd component sizes (where the clusters
   --  are not aligned anyway, see, e.g. System.Pack_07 in file s-pack07
   --  for details.

   TYPE Packed_Bytes2 IS NEW packed_bytes1;
   FOR packed_bytes2'Alignment USE integer'Min (2, Standard'Maximum_Alignment);
   PRAGMA Suppress_Initialization (Packed_Bytes2);
   --  This is the type used to implement packed arrays where an alignment
   --  of 2 (is possible) is helpful for maximum efficiency of the get and
   --  set routines in the corresponding library unit. This is true of all
   --  component sizes that are even but not divisible by 4 (other than 2 for
   --  which we use direct masking operations). In such cases, the clusters
   --  can be assumed to be 2-byte aligned if the array is aligned. See for
   --  example System.Pack_10 in file s-pack10).

   TYPE Packed_Bytes4 IS NEW packed_bytes1;
   FOR packed_bytes4'Alignment USE integer'Min (4, Standard'Maximum_Alignment);
   PRAGMA Suppress_Initialization (Packed_Bytes4);
   --  This is the type used to implement packed arrays where an alignment
   --  of 4 (if possible) is helpful for maximum efficiency of the get and
   --  set routines in the corresponding library unit. This is true of all
   --  component sizes that are divisible by 4 (other than powers of 2, which
   --  are either handled by direct masking or not packed at all). In such
   --  cases the clusters can be assumed to be 4-byte aligned if the array
   --  is aligned (see System.Pack_12 in file s-pack12 as an example).

   TYPE Bits_1 IS MOD 2**1;
   TYPE Bits_2 IS MOD 2**2;
   TYPE Bits_4 IS MOD 2**4;
   --  Types used for packed array conversions

   SUBTYPE Bytes_F IS packed_bytes4 (1 .. float'Size / 8);
   --  Type used in implementation of Is_Negative intrinsic (see Exp_Intr)

   FUNCTION Shift_Left
      (Value  : short_short_unsigned;
       Amount : natural)
      RETURN short_short_unsigned;

   FUNCTION Shift_Right
      (Value  : short_short_unsigned;
       Amount : natural)
      RETURN short_short_unsigned;

   FUNCTION Shift_Right_Arithmetic
      (Value  : short_short_unsigned;
       Amount : natural)
      RETURN short_short_unsigned;

   FUNCTION Rotate_Left
      (Value  : short_short_unsigned;
       Amount : natural)
      RETURN short_short_unsigned;

   FUNCTION Rotate_Right
      (Value  : short_short_unsigned;
       Amount : natural)
      RETURN short_short_unsigned;

   FUNCTION Shift_Left
      (Value  : short_unsigned;
       Amount : natural)
      RETURN short_unsigned;

   FUNCTION Shift_Right
      (Value  : short_unsigned;
       Amount : natural)
      RETURN short_unsigned;

   FUNCTION Shift_Right_Arithmetic
      (Value  : short_unsigned;
       Amount : natural)
      RETURN short_unsigned;

   FUNCTION Rotate_Left
      (Value  : short_unsigned;
       Amount : natural)
      RETURN short_unsigned;

   FUNCTION Rotate_Right
      (Value  : short_unsigned;
       Amount : natural)
      RETURN short_unsigned;

   FUNCTION Shift_Left
      (Value  : unsigned;
       Amount : natural)
      RETURN unsigned;

   FUNCTION Shift_Right
      (Value  : unsigned;
       Amount : natural)
      RETURN unsigned;

   FUNCTION Shift_Right_Arithmetic
      (Value  : unsigned;
       Amount : natural)
      RETURN unsigned;

   FUNCTION Rotate_Left
      (Value  : unsigned;
       Amount : natural)
      RETURN unsigned;

   FUNCTION Rotate_Right
      (Value  : unsigned;
       Amount : natural)
      RETURN unsigned;

   FUNCTION Shift_Left
      (Value  : long_unsigned;
       Amount : natural)
      RETURN long_unsigned;

   FUNCTION Shift_Right
      (Value  : long_unsigned;
       Amount : natural)
      RETURN long_unsigned;

   FUNCTION Shift_Right_Arithmetic
      (Value  : long_unsigned;
       Amount : natural)
      RETURN long_unsigned;

   FUNCTION Rotate_Left
      (Value  : long_unsigned;
       Amount : natural)
      RETURN long_unsigned;

   FUNCTION Rotate_Right
      (Value  : long_unsigned;
       Amount : natural)
      RETURN long_unsigned;

   FUNCTION Shift_Left
      (Value  : long_long_unsigned;
       Amount : natural)
      RETURN long_long_unsigned;

   FUNCTION Shift_Right
      (Value  : long_long_unsigned;
       Amount : natural)
      RETURN long_long_unsigned;

   FUNCTION Shift_Right_Arithmetic
      (Value  : long_long_unsigned;
       Amount : natural)
      RETURN long_long_unsigned;

   FUNCTION Rotate_Left
      (Value  : long_long_unsigned;
       Amount : natural)
      RETURN long_long_unsigned;

   FUNCTION Rotate_Right
      (Value  : long_long_unsigned;
       Amount : natural)
      RETURN long_long_unsigned;

   PRAGMA Import (Intrinsic, Shift_Left);
   PRAGMA Import (Intrinsic, Shift_Right);
   PRAGMA Import (Intrinsic, Shift_Right_Arithmetic);
   PRAGMA Import (Intrinsic, Rotate_Left);
   PRAGMA Import (Intrinsic, Rotate_Right);

   --  The following definitions are obsolescent. They were needed by the
   --  previous version of the compiler and runtime, but are not needed
   --  by the current version. We retain them to help with bootstrap path
   --  problems. Also they seem harmless, and if any user programs have
   --  been using these types, why discombobulate them?

   SUBTYPE Packed_Bytes IS packed_bytes4;
   SUBTYPE Packed_Bytes_Unaligned IS packed_bytes1;

END System.Unsigned_Types;
