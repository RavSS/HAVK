------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . I M G _ C H A R                       --
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

PACKAGE BODY System.Img_Char IS

   ---------------------
   -- Image_Character --
   ---------------------

   PROCEDURE Image_Character
      (V :        character;
       S : IN OUT string;
       P :    OUT natural)
   IS
      PRAGMA Assert (S'First = 1);

      SUBTYPE Cname IS string (1 .. 3);

      SUBTYPE C0_Range IS
         character RANGE character'Val (16#00#) .. character'Val (16#1F#);

      C0 : CONSTANT ARRAY (c0_range) OF cname :=
         (character'Val (16#00#) => "NUL",
          character'Val (16#01#) => "SOH",
          character'Val (16#02#) => "STX",
          character'Val (16#03#) => "ETX",
          character'Val (16#04#) => "EOT",
          character'Val (16#05#) => "ENQ",
          character'Val (16#06#) => "ACK",
          character'Val (16#07#) => "BEL",
          character'Val (16#08#) => "BS ",
          character'Val (16#09#) => "HT ",
          character'Val (16#0A#) => "LF ",
          character'Val (16#0B#) => "VT ",
          character'Val (16#0C#) => "FF ",
          character'Val (16#0D#) => "CR ",
          character'Val (16#0E#) => "SO ",
          character'Val (16#0F#) => "SI ",
          character'Val (16#10#) => "DLE",
          character'Val (16#11#) => "DC1",
          character'Val (16#12#) => "DC2",
          character'Val (16#13#) => "DC3",
          character'Val (16#14#) => "DC4",
          character'Val (16#15#) => "NAK",
          character'Val (16#16#) => "SYN",
          character'Val (16#17#) => "ETB",
          character'Val (16#18#) => "CAN",
          character'Val (16#19#) => "EM ",
          character'Val (16#1A#) => "SUB",
          character'Val (16#1B#) => "ESC",
          character'Val (16#1C#) => "FS ",
          character'Val (16#1D#) => "GS ",
          character'Val (16#1E#) => "RS ",
          character'Val (16#1F#) => "US ");

      SUBTYPE C1_Range IS
         character RANGE character'Val (16#7F#) .. character'Val (16#9F#);

      C1 : CONSTANT ARRAY (c1_range) OF cname :=
         (character'Val (16#7F#) => "DEL",
          character'Val (16#80#) => "res",
          character'Val (16#81#) => "res",
          character'Val (16#82#) => "BPH",
          character'Val (16#83#) => "NBH",
          character'Val (16#84#) => "res",
          character'Val (16#85#) => "NEL",
          character'Val (16#86#) => "SSA",
          character'Val (16#87#) => "ESA",
          character'Val (16#88#) => "HTS",
          character'Val (16#89#) => "HTJ",
          character'Val (16#8A#) => "VTS",
          character'Val (16#8B#) => "PLD",
          character'Val (16#8C#) => "PLU",
          character'Val (16#8D#) => "RI ",
          character'Val (16#8E#) => "SS2",
          character'Val (16#8F#) => "SS3",
          character'Val (16#90#) => "DCS",
          character'Val (16#91#) => "PU1",
          character'Val (16#92#) => "PU2",
          character'Val (16#93#) => "STS",
          character'Val (16#94#) => "CCH",
          character'Val (16#95#) => "MW ",
          character'Val (16#96#) => "SPA",
          character'Val (16#97#) => "EPA",
          character'Val (16#98#) => "SOS",
          character'Val (16#99#) => "res",
          character'Val (16#9A#) => "SCI",
          character'Val (16#9B#) => "CSI",
          character'Val (16#9C#) => "ST ",
          character'Val (16#9D#) => "OSC",
          character'Val (16#9E#) => "PM ",
          character'Val (16#9F#) => "APC");

   BEGIN
      --  Control characters are represented by their names (RM 3.5(32))

      IF V IN c0_range
      THEN
         S (1 .. 3) := C0 (V);
         P          := (IF S (3) = ' ' THEN 2 ELSE 3);

      ELSIF V IN c1_range
      THEN
         S (1 .. 3) := C1 (V);

         IF S (1) /= 'r'
         THEN
            P := (IF S (3) = ' ' THEN 2 ELSE 3);

         --  Special case, res means RESERVED_nnn where nnn is the three digit
         --  decimal value corresponding to the code position (more efficient
         --  to compute than to store).

         ELSE
            DECLARE
               VP : CONSTANT natural := character'Pos (V);
            BEGIN
               S (1 .. 9) := "RESERVED_";
               S (10)     := character'Val (48 + VP / 100);
               S (11)     := character'Val (48 + (VP / 10) MOD 10);
               S (12)     := character'Val (48 + VP MOD 10);
               P          := 12;
            END;
         END IF;

      --  Normal characters yield the character enclosed in quotes (RM 3.5(32))

      ELSE
         S (1) := ''';
         S (2) := V;
         S (3) := ''';
         P     := 3;
      END IF;
   END Image_Character;

   ------------------------
   -- Image_Character_05 --
   ------------------------

   PROCEDURE Image_Character_05
      (V :        character;
       S : IN OUT string;
       P :    OUT natural)
   IS
      PRAGMA Assert (S'First = 1);
   BEGIN
      IF V = character'Val (16#00AD#)
      THEN
         P          := 11;
         S (1 .. P) := "SOFT_HYPHEN";
      ELSE
         Image_Character (V, S, P);
      END IF;
   END Image_Character_05;

END System.Img_Char;
