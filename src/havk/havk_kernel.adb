WITH
   HAVK_Kernel.Debug;

PACKAGE BODY HAVK_Kernel IS
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

      IF Information'length <= Logs(Last_Log).Information'length THEN
         FOR I IN Information'range LOOP
            Logs(Last_Log).Information(I) := Information(I);
         END LOOP;
      ELSE
         -- Don't use recursion in case this message also exceeds the length.
         PRAGMA Debug(Debug.Message("[WARNING] | Above log message " &
            "exceeded storage length."));
         RETURN;
      END IF;

      -- Pad out the rest of the log's string with zeroes so the entry can
      -- be reused later without garbage characters occuring at the end.
      FOR I IN Information'last + 1 .. Logs(1).Information'last LOOP
         Logs(Last_Log).Information(I) := character'val(0);
      END LOOP;

      Logs(Last_Log).Priority := Priority;

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
