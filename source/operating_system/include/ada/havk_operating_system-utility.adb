-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-utility.adb                      --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Operating_System.Utility
WITH
   Refined_State => (CPU_Port_State => NULL)
IS
   FUNCTION Bit_Test
     (Value : IN number;
      Bit   : IN number)
      RETURN boolean
   IS
     (boolean'val(Shift_Right(Value, natural(Bit)) AND 1));

   FUNCTION Image
     (Value   : IN number;
      Base    : IN number   := 10;
      Padding : IN positive := 01)
      RETURN image_string
   IS
      Base_16   : CONSTANT ARRAY(number RANGE 16#0# .. 16#0F#) OF character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
         'A', 'B', 'C', 'D', 'E', 'F');
      Imaged    : image_string := (OTHERS => NUL);
      Temporary : number  := Value;
      Pad_Count : natural := Padding;
   BEGIN
      IF
         Base = 10
      THEN
         FOR
            Index IN REVERSE Imaged'range
         LOOP
            IF
               Temporary = 0
            THEN
               EXIT WHEN Pad_Count = 0;
               Pad_Count := Pad_Count - 1;
            ELSE
               Pad_Count := (IF Pad_Count /= 0 THEN
                  Pad_Count - 1 ELSE Pad_Count);
            END IF;

            Imaged(Index) :=
               character'val((Temporary MOD Base) + character'pos('0'));
            Temporary     := Temporary / Base;
         END LOOP;
      ELSIF
         Base = 16
      THEN
         FOR
            Index IN REVERSE Imaged'range
         LOOP
            IF
               Temporary = 0
            THEN
               EXIT WHEN Pad_Count = 0;
               Pad_Count := Pad_Count - 1;
            ELSE
               Pad_Count := (IF Pad_Count /= 0 THEN
                  Pad_Count - 1 ELSE Pad_Count);
            END IF;

            Imaged(Index) := Base_16(Temporary AND Base - 1);
            Temporary     := Shift_Right(Temporary, 4);
         END LOOP;
      END IF;

      RETURN Imaged;
   END Image;

END HAVK_Operating_System.Utility;