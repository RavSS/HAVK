------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 6 4                       --
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

--  This unit provides software routines for doing arithmetic on 64-bit
--  signed integer values in cases where either overflow checking is
--  required, or intermediate results are longer than 64 bits.

PRAGMA Restrictions (No_Elaboration_Code);
--  Allow direct call from gigi generated code

WITH Interfaces;

PACKAGE System.Arith_64 IS
   PRAGMA Pure;

   SUBTYPE Int64 IS Interfaces.integer_64;

   FUNCTION Add_With_Ovflo_Check
      (X, Y : int64)
      RETURN int64;
      --  Raises Constraint_Error if sum of operands overflows 64 bits,
      --  otherwise returns the 64-bit signed integer sum.

   FUNCTION Subtract_With_Ovflo_Check
      (X, Y : int64)
      RETURN int64;
      --  Raises Constraint_Error if difference of operands overflows 64
      --  bits, otherwise returns the 64-bit signed integer difference.

   FUNCTION Multiply_With_Ovflo_Check
      (X, Y : int64)
      RETURN int64;
   PRAGMA Export (C, Multiply_With_Ovflo_Check, "__gnat_mulv64");
   --  Raises Constraint_Error if product of operands overflows 64
   --  bits, otherwise returns the 64-bit signed integer product.
   --  GIGI may also call this routine directly.

   PROCEDURE Scaled_Divide
      (X, Y, Z :     int64;
       Q, R    : OUT int64;
       Round   :     boolean);
      --  Performs the division of (X * Y) / Z, storing the quotient in Q
      --  and the remainder in R. Constraint_Error is raised if Z is zero,
      --  or if the quotient does not fit in 64-bits. Round indicates if
      --  the result should be rounded. If Round is False, then Q, R are
      --  the normal quotient and remainder from a truncating division.
      --  If Round is True, then Q is the rounded quotient. The remainder
      --  R is not affected by the setting of the Round flag.

   PROCEDURE Double_Divide
      (X, Y, Z :     int64;
       Q, R    : OUT int64;
       Round   :     boolean);
   --  Performs the division X / (Y * Z), storing the quotient in Q and
   --  the remainder in R. Constraint_Error is raised if Y or Z is zero,
   --  or if the quotient does not fit in 64-bits. Round indicates if the
   --  result should be rounded. If Round is False, then Q, R are the normal
   --  quotient and remainder from a truncating division. If Round is True,
   --  then Q is the rounded quotient. The remainder R is not affected by the
   --  setting of the Round flag.

END System.Arith_64;
