------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         I N T E R F A C E S . C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This version contains only the type definitions for standard interfacing
--  with C. All functions have been removed from the original spec.

PACKAGE Interfaces.C IS
   PRAGMA No_Elaboration_Code_All;
   PRAGMA Pure;

   --  Declaration's based on C's <limits.h>

   CHAR_BIT  : CONSTANT := 8;
   SCHAR_MIN : CONSTANT := -128;
   SCHAR_MAX : CONSTANT := 127;
   UCHAR_MAX : CONSTANT := 255;

   --  Signed and Unsigned Integers. Note that in GNAT, we have ensured that
   --  the standard predefined Ada types correspond to the standard C types

   TYPE int IS NEW integer;
   TYPE short IS NEW short_integer;
   TYPE long IS NEW long_integer;

   TYPE signed_char IS RANGE schar_min .. schar_max;
   FOR signed_char'Size USE char_bit;

   TYPE unsigned IS MOD 2**int'Size;
   TYPE unsigned_short IS MOD 2**short'Size;
   TYPE unsigned_long IS MOD 2**long'Size;

   TYPE unsigned_char IS MOD (uchar_max + 1);
   FOR unsigned_char'Size USE char_bit;

   SUBTYPE plain_char IS unsigned_char;

   TYPE ptrdiff_t IS
      RANGE -(2**(Standard'Address_Size - 1)) ..
            +(2**(Standard'Address_Size - 1) - 1);

   TYPE size_t IS MOD 2**Standard'Address_Size;

   --  Floating-Point

   TYPE C_float IS NEW float;
   TYPE double IS NEW Standard.long_float;
   TYPE long_double IS NEW Standard.long_long_float;

   ----------------------------
   -- Characters and Strings --
   ----------------------------

   TYPE char IS NEW character;

   nul : CONSTANT char := char'First;

   TYPE char_array IS ARRAY (size_t RANGE <>) OF ALIASED char;
   FOR char_array'Component_Size USE char_bit;

   ------------------------------------
   -- Wide Character and Wide String --
   -- Ravjot: I've disabled these since they're useless for me.
   ------------------------------------

   -- type wchar_t is new Wide_Character;
   -- for wchar_t'Size use Standard'Wchar_T_Size;

   -- wide_nul : constant wchar_t := wchar_t'First;

   -- type wchar_array is array (size_t range <>) of aliased wchar_t;

END Interfaces.C;
