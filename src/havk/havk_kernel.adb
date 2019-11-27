WITH
   HAVK_Kernel.Debug;

PACKAGE BODY HAVK_Kernel
IS
   PROCEDURE Log(
      Information : IN string;
      Priority    : IN urgency := trivial)
   IS
      PROCEDURE Serial_Message
      WITH
         Inline_Always => true;

      PROCEDURE Serial_Message
      IS
      BEGIN
         CASE Priority IS
            WHEN trivial =>
               Debug.Message("[Trivial] | " & Information);
            WHEN nominal =>
               Debug.Message("[Nominal] | " & Information);
            WHEN warning =>
               Debug.Message("[WARNING] | " & Information);
            WHEN fatal   =>
               Debug.Message("[ FATAL ] | " & Information);
         END CASE;
      END Serial_Message;
   BEGIN
      PRAGMA Debug(Serial_Message); -- Always send it over COM* first.

      IF -- Check if the log's information can actually be stored.
         Information'first >= Logs(Last_Log).Information'first AND
         THEN Information'last <= Logs(Last_Log).Information'last
      THEN
         FOR I IN Information'range LOOP
            Logs(Last_Log).Information(I) := Information(I);
         END LOOP;

         Logs(Last_Log).Priority := Priority;
      ELSE
         -- Don't use recursion in case this message also exceeds the length.
         PRAGMA Debug(Debug.Message("[WARNING] | Above log message " &
            "is outside storage length."));

         -- Quickly describe why the log's information is missing if I can.
         -- The if-statement check here is so the log information size is
         -- easily configurable in the future. It will be optimised out.
         IF Logs(Last_Log).Information'length >= 3 THEN
            Logs(Last_Log).Information := ('C', 'U', 'T',
               OTHERS => character'val(0));
            Logs(Last_Log).Priority    := warning;
         END IF;
      END IF;

      IF Last_Log = log_entry_limit'last THEN
         FOR L IN log_entry_limit'first .. log_entry_limit'last - 1 LOOP
            Logs(L) := Logs(L + 1);
         END LOOP;
         Logs(Last_Log) := ((OTHERS => character'val(0)), trivial); -- Emptied.
         Last_Log := Last_Log - 1;
      ELSE
         Last_Log := Last_Log + 1;
      END IF;
   END Log;
END HAVK_Kernel;
