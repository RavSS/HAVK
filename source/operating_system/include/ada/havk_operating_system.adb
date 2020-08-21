-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system.adb                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Operating_System
IS
   PROCEDURE Last_Chance_Handler
     (String_Address : IN address;
      Line           : IN natural)
   IS
      Message        : CONSTANT XMM_string
      WITH
         Import     => true,
         Convention => C,
         Address    => String_Address;

      Message_Length : natural RANGE Message'range := 1;

      Call_Arguments : arguments :=
        (Operation_Call => log_operation, OTHERS => <>);
      Call_String : XMM_string := (OTHERS => NUL);

      Imaged_Line : CONSTANT string := Line'image;
   BEGIN
      FOR
         ASCII OF Message
      LOOP
         EXIT WHEN ASCII = NUL;
         Message_Length := Message_Length + 1;
      END LOOP;

      Call_String(1 .. Message_Length) := Message(1 .. Message_Length);

      IF
         Line /= 0 AND THEN
         Message_Length + Imaged_Line'first IN Call_String'range AND THEN
         Message_Length + Imaged_Line'last IN Call_String'range
      THEN
         FOR
            Index IN Imaged_Line'range
         LOOP
            Call_String(Message_Length + Index) := Imaged_Line(Index);
         END LOOP;
         -- Replace the empty space produced by the image function in the RTS.
         Call_String(Message_Length + Imaged_Line'first) := ':';
      END IF;

      System_Call(Call_Arguments, Call_String);

      Call_Arguments :=
        (Operation_Call => exit_task_operation,
         Argument_1     => 1,
         OTHERS         => <>);
      System_Call(Call_Arguments);

      LOOP
         NULL;
      END LOOP;
   END Last_Chance_Handler;

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

END HAVK_Operating_System;
