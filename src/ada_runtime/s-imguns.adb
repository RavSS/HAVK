------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M G _ U N S                        --
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

WITH System.Unsigned_Types;
USE System.Unsigned_Types;

PACKAGE BODY System.Img_Uns IS

   --------------------
   -- Image_Unsigned --
   --------------------

   PROCEDURE Image_Unsigned
      (V :        System.Unsigned_Types.unsigned;
       S : IN OUT string;
       P :    OUT natural)
   IS
      PRAGMA Assert (S'First = 1);
   BEGIN
      S (1) := ' ';
      P     := 1;
      Set_Image_Unsigned (V, S, P);
   END Image_Unsigned;

   ------------------------
   -- Set_Image_Unsigned --
   ------------------------

   PROCEDURE Set_Image_Unsigned
      (V :        unsigned;
       S : IN OUT string;
       P : IN OUT natural)
   IS
   BEGIN
      IF V >= 10
      THEN
         Set_Image_Unsigned (V / 10, S, P);
         P     := P + 1;
         S (P) := character'Val (48 + (V REM 10));

      ELSE
         P     := P + 1;
         S (P) := character'Val (48 + V);
      END IF;
   END Set_Image_Unsigned;

END System.Img_Uns;
