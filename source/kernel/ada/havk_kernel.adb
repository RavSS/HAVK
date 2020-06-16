-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel.adb                                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
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
         Warn THEN "[WARNING] {" ELSE "{") & Tag & "} ";
   BEGIN
      -- Always send it over COM* first, as it does not have a length limit
      -- below that of the string type's own indexing type range constraint.
      IF
         Information'last < positive'last - Log_Prefix'Length
      THEN
         Debug.Message(Log_Prefix & Information);
      ELSE
         Debug.Message(Log_Prefix & Information
           (Information'first .. positive'last - Log_Prefix'Length));
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
      FUNCTION Length
        (Pointer  : IN address;
         Limit    : IN positive)
         RETURN natural
      WITH
         Global        => NULL,
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__string_length",
         Pre           => Pointer /= 0,
         Post          => Length'result <= Limit;

      -- To be used if there was no log or tag supplied.
      Null_Log      : CONSTANT string :=
         "A null or incorrect external log was passed.";
      External_Tag  : CONSTANT string := "EXTERNAL";

      -- Remember that the standard string type in Ada only supports a positive
      -- index (one-based or higher), so a length of zero needs to be
      -- interpreted as a length of one.
      Log_C_Length  : CONSTANT natural := (IF Log_Pointer /= 0 THEN
         Length(Log_Pointer, log_string_limit'last) ELSE 0);
      Tag_C_Length  : CONSTANT natural := (IF Tag_Pointer /= 0 THEN
         Length(Tag_Pointer, log_tag_limit'last) ELSE 0);

      Log_C_String  : CONSTANT string
        (1 .. (IF Log_C_Length /= 0 THEN Log_C_Length ELSE 1))
      WITH
         Import     => true,
         Convention => C,
         Address    => Log_Pointer,
         Annotate   => (GNATprove, False_Positive,
                        "object with constraints on bit representation *",
                        "It's just an array of bytes, no inner alignment.");

      Tag_C_String  : CONSTANT string
        (1 .. (IF Tag_C_Length /= 0 THEN Tag_C_Length ELSE 1))
      WITH
         Import     => true,
         Convention => C,
         Address    => Tag_Pointer;

      -- Convert the 8-bit characters to Ada booleans.
      Warn_Bool     : CONSTANT boolean := character'pos(Warn) /= 0 OR ELSE
         Log_C_Length = 0; -- Warn if the log information itself is missing.
      Critical_Bool : CONSTANT boolean := character'pos(Critical) /= 0;
   BEGIN
      Log((IF Log_C_Length /= 0 THEN Log_C_String ELSE Null_Log),
         Tag => (IF Tag_C_Length /= 0 THEN Tag_C_String ELSE External_Tag),
         Warn => Warn_Bool, Critical => Critical_Bool);
   END External_Log;

   FUNCTION Image
     (Value   : IN number;
      Base    : IN number := 10;
      Padding : IN number := 0)
      RETURN string
   IS
      FUNCTION Get_Digits
         RETURN number
      WITH
         Inline => true,
         Pre    => Base IN 10 | 16 AND THEN Padding < 64,
         Post   => (IF Padding = 0 THEN Get_Digits'result IN 1 .. 64
                       ELSE Get_Digits'result IN Padding .. 64);

      FUNCTION Get_Digits
         RETURN number
      IS
         Digit_Count : number RANGE 0 .. 64 := 0;
         Temporary   : number := Value;
      BEGIN
         IF
            Base = 10
         THEN -- TODO: The digit counts aren't automatically provable.
            WHILE
               Temporary /= 0
            LOOP
               PRAGMA Loop_Invariant(Digit_Count <= Base * 2);

               IF
                  Digit_Count < Base * 2
               THEN
                  Digit_Count := Digit_Count + 1;
               END IF;

               Temporary := Temporary / Base;
            END LOOP;
         ELSIF
            Base = 16
         THEN
            WHILE
               Temporary /= 0
            LOOP
               PRAGMA Loop_Invariant(Digit_Count <= Base);

               IF
                  Digit_Count < Base
               THEN
                  Digit_Count := Digit_Count + 1;
               END IF;

               Temporary := Shift_Right(Temporary, 4);
            END LOOP;
         END IF;

         IF
            Digit_Count < Padding
         THEN
            RETURN Padding;
         ELSIF
            Value /= 0
         THEN
            RETURN Digit_Count;
         ELSE -- Even a value of zero has a single digit.
            RETURN 1;
         END IF;
      END Get_Digits;

      Base_16   : CONSTANT ARRAY(number RANGE 16#0# .. 16#0F#) OF character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
         'A', 'B', 'C', 'D', 'E', 'F');
      Imaged    : string(1 .. positive(Get_Digits)) := (OTHERS => '0');
      Temporary : number := Value;
   BEGIN
      IF
         Base = 10
      THEN
         FOR
            Index IN REVERSE Imaged'range
         LOOP
            EXIT WHEN Temporary = 0;
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
            EXIT WHEN Temporary = 0;
            Imaged(Index) := Base_16(Temporary AND Base - 1);
            Temporary     := Shift_Right(Temporary, 4);
         END LOOP;
      END IF;

      RETURN Imaged;
   END Image;

   FUNCTION Image
     (Value   : IN address;
      Base    : IN number := 16;
      Padding : IN number := 0)
      RETURN string
   IS
     (Image(number(Value), Base => Base, Padding => Padding));
END HAVK_Kernel;
