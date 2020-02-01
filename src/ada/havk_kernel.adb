-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel.adb                                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   System.Address_Image,
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

   FUNCTION Hex_Image
     (Value : IN address)
      RETURN string
   IS
      Imaged : CONSTANT string := System.Address_Image(Value);
   BEGIN
      IF
         Imaged'first = 1 AND THEN Imaged'last = 16
      THEN
         RETURN Imaged;
      ELSE
         RETURN "????????????????";
      END IF;
   END Hex_Image;
END HAVK_Kernel;
