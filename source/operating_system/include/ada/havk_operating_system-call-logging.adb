-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-call-logging.adb                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Operating_System.Call.Logging
IS
   PRAGMA Warnings(off,
      "formal parameter ""*"" is not referenced",
      Reason => "The kernel system call doesn't handle these yet.");
   PROCEDURE Log
     (Information : IN string;
      Tag         : IN string  := "N/A";
      Warn        : IN boolean := false;
      Critical    : IN boolean := false)
   IS
      Call_Arguments : arguments :=
        (Operation_Call => log_operation, OTHERS => <>);
      Call_String    : XMM_string := (OTHERS => NUL);

      Prefix : CONSTANT string := '[' & Tag & "] ";
   BEGIN
      FOR
         Index IN 1 .. Prefix'length
      LOOP
         EXIT WHEN Index NOT IN Call_String'range;
         Call_String(Index) := Prefix(Index);
      END LOOP;

      FOR
         Index IN 1 .. Information'length
      LOOP
         EXIT WHEN Prefix'length + Index NOT IN Call_String'range;
         EXIT WHEN Index NOT IN Information'range;
         Call_String(Prefix'length + Index) := Information(Index);
      END LOOP;

      System_Call(Call_Arguments, Call_String);
   END Log;

END HAVK_Operating_System.Call.Logging;