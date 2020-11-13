-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system.adb                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Operating_System
WITH
   Refined_State => (CPU_Port_State => NULL)
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

   PROCEDURE Task_Finder
     (Task_Name : IN string;
      Status    : OUT task_status)
   IS
      Identity_Data   : arguments := (identify_task_operation, OTHERS => 0);
      Identity_String : XMM_string := (OTHERS => NUL);
   BEGIN
      FOR
         Identity_Index IN number RANGE 1 .. 255
      LOOP
         Identity_Data.Argument_1 := general_register(Identity_Index);

         IF -- Check its name and if it's a living task.
            System_Call(Identity_Data, Identity_String) = no_error AND THEN
            To_Status(Identity_String).Data.Name(Task_Name'range) = Task_Name
         THEN
            Status := To_Status(Identity_String).Data;
            RETURN;
         END IF;
      END LOOP;

      Status := (Index => 0, OTHERS => <>);
   END Task_Finder;

END HAVK_Operating_System;
