-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel.adb                                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Debug;

PACKAGE BODY HAVK_Kernel
WITH
   Refined_State => (Logging_State => Logs)
IS
   PROCEDURE Log
     (Information : IN string;
      Tag         : IN string  := "N/A";
      Warn        : IN boolean := false;
      Critical    : IN boolean := false)
   IS
      Log_Prefix  : CONSTANT string := (IF Critical THEN "[CRITICAL] {" ELSIF
         Warn THEN "[WARNING] {" ELSE "[NOMINAL] {") & Tag & "} ";
   BEGIN
      -- Always send it over COM* first, as it does not have a length limit
      -- below that of the string type's own indexing type range constraint.
      IF
         Information'last < (positive'last / 2) - Log_Prefix'length
      THEN
         Debug.Message(Log_Prefix & Information);
      ELSE
         Debug.Message(Log_Prefix & Information
           (Information'first .. (positive'last / 2) - Log_Prefix'length));
      END IF;

      IF -- Now check if the log's information can actually be stored.
         Information'first >= Logs.Log_List(Logs.Last_Log).Information'first
            AND THEN
         Information'last <= Logs.Log_List(Logs.Last_Log).Information'last
      THEN
         FOR
            I IN Information'range
         LOOP
            Logs.Log_List(Logs.Last_Log).Information(I) := Information(I);
         END LOOP;

         FOR -- The log tag will be proven to be its length prior to the call.
            I IN Tag'range
         LOOP
            Logs.Log_List(Logs.Last_Log).Tag(I) := Tag(I);
         END LOOP;

         Logs.Log_List(Logs.Last_Log).Warned   := Warn;
         Logs.Log_List(Logs.Last_Log).Critical := Critical;
      ELSE
         -- Don't use recursion in case this message also exceeds the length.
         Debug.Message("[WARNING] Above log message is outside the " &
            "storage length.");

         -- Quickly describe why the log's information is missing if I can.
         -- The if-statement check here is so the log information size is
         -- easily configurable in the future. It will be optimised out.
         IF
            Logs.Log_List(Logs.Last_Log).Information'length >= 3
         THEN
            Logs.Log_List(Logs.Last_Log).Information :=
              ('C', 'U', 'T', OTHERS => character'val(0));

            Logs.Log_List(Logs.Last_Log).Warned := true;
         END IF;
      END IF;

      IF
         Logs.Last_Log = log_entry_limit'last
      THEN
         FOR
            L IN log_entry_limit'first .. log_entry_limit'last - 1
         LOOP
            Logs.Log_List(L) := Logs.Log_List(L + 1);
         END LOOP;

         -- Empty the last log's information.
         Logs.Log_List(Logs.Last_Log) := (OTHERS => <>);
         Logs.Last_Log := Logs.Last_Log - 1;
      ELSE
         Logs.Last_Log := Logs.Last_Log + 1;
      END IF;
   END Log;

   PROCEDURE External_Log
     (Log_Pointer : IN address;
      Tag_Pointer : IN address;
      Warn        : IN character;
      Critical    : IN character)
   IS
      -- To be used if there was no log or tag supplied.
      Null_Log      : CONSTANT string :=
         "A null or incorrect external log was passed.";
      External_Tag  : CONSTANT string := "EXTERNAL";

      -- Copy the characters byte-by-byte. An element size value passed by the
      -- foreign caller seems unnecessary.
      Address_Offset  : address RANGE 1 .. address'last; -- Anything but null.
      Log_Information : string(log_string_limit'range) := (OTHERS => NUL);
      Log_Tag         : string(log_tag_limit'range) := (OTHERS => NUL);

      -- Convert the 8-bit characters to Ada booleans.
      Warn_Bool     : CONSTANT boolean := character'pos(Warn) /= 0;
      Critical_Bool : CONSTANT boolean := character'pos(Critical) /= 0;
   BEGIN
      IF
         Log_Pointer = 0
      THEN -- Warn if the log information itself is missing.
         Log(Null_Log, External_Tag, Warn => true,
            Critical => Critical_Bool);
         RETURN;
      END IF;

      Address_Offset := Log_Pointer;

      FOR
         Log_Index IN Log_Information'range
      LOOP
         DECLARE
            Character_Byte : ALIASED CONSTANT byte
            WITH
               Import     => true,
               Convention => C,
               Address    => Address_Offset;
         BEGIN
            EXIT WHEN Character_Byte = 0;
            Log_Information(Log_Index) := character'val(Character_Byte);
         END;

         EXIT WHEN Address_Offset = address'last;
         Address_Offset := Address_Offset + 1;
      END LOOP;

      IF
         Tag_Pointer /= 0
      THEN
         Address_Offset := Tag_Pointer;

         FOR
            Tag_Index IN Log_Tag'range
         LOOP
            DECLARE
               Character_Byte : ALIASED CONSTANT byte
               WITH
                  Import     => true,
                  Convention => C,
                  Address    => Address_Offset;
            BEGIN
               EXIT WHEN Character_Byte = 0;
               Log_Tag(Tag_Index) := character'val(Character_Byte);
            END;

            EXIT WHEN Address_Offset = address'last;
            Address_Offset := Address_Offset + 1;
         END LOOP;
      END IF;

      IF
         Log_Information(Log_Information'first) = NUL
      THEN
         Log(Null_Log, External_Tag, Warn => Warn_Bool,
            Critical => Critical_Bool);
      ELSIF
         Log_Tag(Log_Tag'first) = NUL
      THEN
         Log(Log_Information, External_Tag, Warn => Warn_Bool,
            Critical => Critical_Bool);
      ELSE
         Log(Log_Information, Log_Tag, Warn => Warn_Bool,
            Critical => Critical_Bool);
      END IF;
   END External_Log;

   FUNCTION Image
     (Value   : IN number;
      Base    : IN number   := 10;
      Padding : IN positive := 01)
      RETURN image_string
   IS
      Base_16   : CONSTANT ARRAY(number RANGE 16#0# .. 16#F#) OF character :=
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

   FUNCTION Image
     (Value   : IN address;
      Base    : IN number   := 16;
      Padding : IN positive := 01)
      RETURN image_string
   IS
     (Image(number(Value), Base => Base, Padding => Padding));

   FUNCTION Scan
     (Imaged : IN string)
      RETURN number
   IS
      Start_Index : natural RANGE 0 .. Imaged'last := 0;
      End_Index   : natural RANGE 0 .. Imaged'last := 0;
      Value       : number := 0;
   BEGIN
      FOR -- Find the start index.
         Index IN Imaged'range
      LOOP
         IF
            Imaged(Index) IN '0' .. '9'
         THEN
            Start_Index := Index;
            EXIT WHEN true;
         END IF;
      END LOOP;

      FOR -- Find the end index.
         Index IN REVERSE Imaged'range
      LOOP
         IF
            Imaged(Index) IN '0' .. '9'
         THEN
            End_Index := Index;
            EXIT WHEN true;
         END IF;
      END LOOP;

      IF -- Failed to find a starting (most significant) number.
         Start_Index NOT IN Imaged'range OR ELSE
         End_Index NOT IN Imaged'range
      THEN
         RETURN number'last;
      END IF;

      FOR -- Now do the actual conversion.
         ASCII_Digit OF Imaged(Start_Index .. End_Index)
      LOOP
         IF -- Make sure that all characters in-between are all ASCII digits.
            ASCII_Digit NOT IN '0' .. '9'
         THEN
            RETURN number'last;
         END IF;

         IF
            Value /= 0
         THEN
            FOR -- An inefficient way of doing this, but the provers accept it.
               Overflow_Check IN number RANGE 1 .. 10
            LOOP
               IF
                  Value >= Value * Overflow_Check
               THEN
                  RETURN number'last;
               END IF;
            END LOOP;
         END IF;

         Value := (Value * 10) +
           (character'pos(ASCII_Digit) - character'pos('0'));
      END LOOP;

      RETURN Value;
   END Scan;

END HAVK_Kernel;
