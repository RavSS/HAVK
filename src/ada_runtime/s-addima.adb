------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . A D D R E S S _ I M A G E                 --
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

FUNCTION System.Address_Image
   (A : address)
   RETURN string
IS

   Result : string (1 .. 2 * address'Size / storage_unit);

   TYPE Byte IS MOD 2**8;
   FOR byte'Size USE 8;

   Hexdigs : CONSTANT ARRAY (byte RANGE 0 .. 15) OF character :=
      "0123456789ABCDEF";

   TYPE Bytes IS ARRAY (1 .. address'Size / storage_unit) OF byte;
   FOR bytes'Size USE address'Size;

   FUNCTION To_Bytes IS NEW Ada.Unchecked_Conversion (address, bytes);

   Byte_Sequence : CONSTANT bytes := To_Bytes (A);

   LE : CONSTANT := Standard'Default_Bit_Order;
   BE : CONSTANT := 1 - LE;
   --  Set to 1/0 for True/False for Little-Endian/Big-Endian

   Start : CONSTANT Natural := BE * (1) + LE * (Bytes'Length);
   Incr  : CONSTANT Integer := BE * (1) + LE * (-1);
   --  Start and increment for accessing characters of address string

   Ptr : natural;
   --  Scan address string

BEGIN
   Ptr := Start;
   FOR N IN bytes'Range
   LOOP
      Result (2 * N - 1) := Hexdigs (Byte_Sequence (Ptr) / 16);
      Result (2 * N)     := Hexdigs (Byte_Sequence (Ptr) MOD 16);
      Ptr                := Ptr + Incr;
   END LOOP;

   RETURN Result;

END System.Address_Image;
