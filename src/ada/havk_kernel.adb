-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel.adb                                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Debug;

PACKAGE BODY HAVK_Kernel
IS
   PROCEDURE Log
     (Information : IN string;
      Priority    : IN urgency := trivial)
   IS
      PROCEDURE Serial_Message
      WITH
         Inline => true;

      PROCEDURE Serial_Message
      IS
      BEGIN
         CASE
            Priority
         IS
            WHEN trivial => Debug.Message("[Trivial] | " & Information);
            WHEN nominal => Debug.Message("[Nominal] | " & Information);
            WHEN warning => Debug.Message("[WARNING] | " & Information);
            WHEN fatal   => Debug.Message("[ FATAL ] | " & Information);
         END CASE;
      END Serial_Message;
   BEGIN
      PRAGMA Debug(Serial_Message); -- Always send it over COM* first.

      IF -- Check if the log's information can actually be stored.
         Information'first >= Logs.Log_List(Logs.Last_Log).Information'first
            AND THEN
         Information'last <= Logs.Log_List(Logs.Last_Log).Information'last
      THEN
         FOR
            I IN Information'range
         LOOP
            Logs.Log_List(Logs.Last_Log).Information(I) := Information(I);
         END LOOP;

         Logs.Log_List(Logs.Last_Log).Priority := Priority;
      ELSE
         -- Don't use recursion in case this message also exceeds the length.
         PRAGMA Debug(Debug.Message("[WARNING] | Above log message " &
            "is outside storage length."));

         -- Quickly describe why the log's information is missing if I can.
         -- The if-statement check here is so the log information size is
         -- easily configurable in the future. It will be optimised out.
         IF
            Logs.Log_List(Logs.Last_Log).Information'length >= 3
         THEN
            Logs.Log_List(Logs.Last_Log).Information :=
               ('C', 'U', 'T', OTHERS => character'val(0));
            Logs.Log_List(Logs.Last_Log).Priority    := warning;
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
         Logs.Log_List(Logs.Last_Log) := -- Empty the last log's information.
            ((OTHERS => character'val(0)), trivial);
         Logs.Last_Log := Logs.Last_Log - 1;
      ELSE
         Logs.Last_Log := Logs.Last_Log + 1;
      END IF;
   END Log;

   PROCEDURE External_Log
     (Pointer    : IN address;
      Priority   : IN urgency)
   IS
      FUNCTION Length
        (Pointer : IN address)
         RETURN integer
      WITH
         Global        => NULL,
         Import        => true,
         Inline        => true,
         Convention    => Assembler,
         External_Name => "assembly__string_length";

      C_String   : CONSTANT string(1 .. Length(Pointer) + 1)
      WITH
         Import     => true,
         Convention => C,
         Address    => Pointer;
   BEGIN
      PRAGMA Warnings(GNATprove, off,
         "attribute Valid is assumed to return True",
         Reason => "If not true, then an internal warning log is created.");
      IF
         Pointer = 0
      THEN
         Log("An null external log was passed.", warning);
      ELSIF
         C_String'length NOT IN log_string_limit'range OR ELSE
         NOT Priority'valid
      THEN
         Log("An incorrect external log was passed of length" &
            number'image(C_String'length) & '.', warning);
      ELSE
         Log(C_String, Priority);
      END IF;
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
         CASE
            Base
         IS -- TODO: Unsure why the digit counts aren't automatically provable.
            WHEN 10 =>
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
            WHEN 16 =>
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
            WHEN OTHERS =>
               RETURN 1;
         END CASE;

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
      CASE
         Base
      IS
         WHEN 10 =>
            FOR
               Index IN REVERSE Imaged'range
            LOOP
               EXIT WHEN Temporary = 0;
               Imaged(Index) :=
                  character'val((Temporary MOD Base) + character'pos('0'));
               Temporary     := Temporary / Base;
            END LOOP;
         WHEN 16 =>
            FOR
               Index IN REVERSE Imaged'range
            LOOP
               EXIT WHEN Temporary = 0;
               Imaged(Index) := Base_16(Temporary AND Base - 1);
               Temporary     := Shift_Right(Temporary, 4);
            END LOOP;
         WHEN OTHERS =>
            NULL;
      END CASE;

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
