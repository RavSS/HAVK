------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L B                        --
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

--  Contains the routine for computing the image in based format of signed and
--  unsigned integers whose size > Integer'Size for use by Text_IO.Integer_IO
--  and Text_IO.Modular_IO.

WITH System.Unsigned_Types;

PACKAGE System.Img_LLB IS
   PRAGMA Preelaborate;

   PROCEDURE Set_Image_Based_Long_Long_Integer
      (V :        long_long_integer;
       B :        natural;
       W :        integer;
       S :    OUT string;
       P : IN OUT natural);
   --  Sets the signed image of V in based format, using base value B (2..16)
   --  starting at S (P + 1), updating P to point to the last character stored.
   --  The image includes a leading minus sign if necessary, but no leading
   --  spaces unless W is positive, in which case leading spaces are output if
   --  necessary to ensure that the output string is no less than W characters
   --  long. The caller promises that the buffer is large enough and no check
   --  is made for this. Constraint_Error will not necessarily be raised if
   --  this is violated, since it is perfectly valid to compile this unit with
   --  checks off.

   PROCEDURE Set_Image_Based_Long_Long_Unsigned
      (V :        System.Unsigned_Types.long_long_unsigned;
       B :        natural;
       W :        integer;
       S :    OUT string;
       P : IN OUT natural);
   --  Sets the unsigned image of V in based format, using base value B (2..16)
   --  starting at S (P + 1), updating P to point to the last character stored.
   --  The image includes no leading spaces unless W is positive, in which case
   --  leading spaces are output if necessary to ensure that the output string
   --  is no less than W characters long. The caller promises that the buffer
   --  is large enough and no check is made for this. Constraint_Error will not
   --  necessarily be raised if this is violated, since it is perfectly valid
   --  to compile this unit with checks off).

END System.Img_LLB;
