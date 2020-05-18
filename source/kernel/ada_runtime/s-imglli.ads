------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ L L I                        --
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

--  This package contains the routines for supporting the Image attribute for
--  signed integer types larger than Size Integer'Size, and also for conversion
--  operations required in Text_IO.Integer_IO for such types.

PACKAGE System.Img_LLI IS
   PRAGMA Pure;

   PROCEDURE Image_Long_Long_Integer
      (V :        long_long_integer;
       S : IN OUT string;
       P :    OUT natural);
      --  Computes Long_Long_Integer'Image (V) and stores the result in
      --  S (1 .. P) setting the resulting value of P. The caller guarantees
      --  that S is long enough to hold the result, and that S'First is 1.

   PROCEDURE Set_Image_Long_Long_Integer
      (V :        long_long_integer;
       S : IN OUT string;
       P : IN OUT natural);
   --  Stores the image of V in S starting at S (P + 1), P is updated to point
   --  to the last character stored. The value stored is identical to the value
   --  of Long_Long_Integer'Image (V) except that no leading space is stored
   --  when V is non-negative. The caller guarantees that S is long enough to
   --  hold the result. S need not have a lower bound of 1.

END System.Img_LLI;
