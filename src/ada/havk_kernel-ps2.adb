-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-ps2.adb                                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.PS2
IS
   FUNCTION Ready_To_Receive
     (Object    : IN controller)
      RETURN boolean
   IS
      Current_Status_Byte : CONSTANT number := Input_Byte(Command);
      Current_Status      : CONSTANT status
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Current_Status_Byte'address;
   BEGIN
      IF
         Object.Port_1_Device /= Unrecognised OR ELSE
         Object.Port_2_Device /= Unrecognised
      THEN
         RETURN Current_Status.Output_Ready;
      ELSE
         RETURN false;
      END IF;
   END Ready_To_Receive;

   FUNCTION Ready_To_Send
     (Object    : IN controller)
      RETURN boolean
   IS
      Current_Status_Byte : CONSTANT number := Input_Byte(Command);
      Current_Status      : CONSTANT status
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Current_Status_Byte'address;
   BEGIN
      IF
         Object.Port_1_Device /= Unrecognised OR ELSE
         Object.Port_2_Device /= Unrecognised
      THEN
         RETURN NOT Current_Status.Input_Full; -- Invert it so it makes sense.
      ELSE
         RETURN false;
      END IF;
   END Ready_To_Send;

   FUNCTION Send(
      Object    : IN controller;
      Port_Type : IN port;
      Byte      : IN number;
      Port_2    : IN boolean;
      Verify    : IN boolean)
   RETURN boolean IS
   BEGIN
      IF
         Port_2
      THEN
         Output_Byte(Command, Port_2_Data); -- Avoid recursion.
      END IF;

      FOR
         R IN 1 .. Object.Retry_Rate
      LOOP
         IF
            Object.Ready_To_Send
         THEN
            FOR
               I IN 1 .. Object.Retry_Rate
            LOOP
               Output_Byte(Port_Type, Byte);
               IF
                  NOT Verify OR ELSE
                  Object.Receive(Data) = Data_Acknowledged
               THEN
                  RETURN true;
               ELSE
                  RETURN false;
               END IF;
            END LOOP;
         END IF;
      END LOOP;

      RETURN false;
   END Send;

   FUNCTION Send_Controller_Command
     (Object    : IN controller;
      Operation : IN controller_command)
      RETURN boolean
   IS
      (Object.Send(Command, Operation, false, false));

   FUNCTION Send_Keyboard_Command
     (Object    : IN controller;
      Operation : IN keyboard_command;
      Port_2    : IN boolean := false)
      RETURN boolean
   IS
      (Object.Send(Data, Operation, Port_2, true));

   FUNCTION Send_Mouse_Command
     (Object    : IN controller;
      Operation : IN mouse_command;
      Port_2    : IN boolean := true)
      RETURN boolean
   IS
      (Object.Send(Data, Operation, Port_2, true));

   FUNCTION Send_Data
     (Object    : IN controller;
      Byte_Data : IN number;
      Port_2    : IN boolean := false;
      Verify    : IN boolean := true)
      RETURN boolean
   IS
      (Object.Send(Data, Byte_Data, Port_2, Verify));

   FUNCTION Receive
     (Object    : IN controller;
      Port_Type : IN port)
      RETURN response
   IS
      Received  : response;
   BEGIN
      FOR
         R IN 1 .. Object.Retry_Rate
      LOOP
         IF
            Object.Ready_To_Receive
         THEN
            Received := Input_Byte(Port_Type);
            IF
               Received /= Test_Port_Pass       AND THEN
               Received /= Test_Controller_Pass AND THEN
               Received /= Data_Acknowledged    AND THEN
               Received /= Failure              AND THEN
               Received /= Data_Resend
            THEN
               RETURN Failure;
            ELSE
               RETURN Received;
            END IF;
         END IF;
      END LOOP;

      RETURN Failure;
   END Receive;

   PROCEDURE Flush
     (Object  : IN controller)
   IS
      -- Magic variable name for GNAT. See pragma "Unmodified".
      Discard : number;
   BEGIN
      WHILE
         Object.Ready_To_Receive
      LOOP
         Discard := Input_Byte(Data);
      END LOOP;
   END Flush;

   -- The device ID command and disable/enable scancode/reporting commands
   -- are the same across devices (0xFE, 0xF5, and 0xF4 respectively),
   -- but the device IDs themselves are not.
   PROCEDURE Identify_Device
     (Object     : IN OUT controller;
      Port_2     : IN boolean)
   IS
      Port_Image : CONSTANT character := (IF Port_2 THEN '2' ELSE '1');

      FUNCTION Identity_Resolve
        (ID      : IN number) -- Only really need the first byte.
         RETURN device
      WITH
         Inline => true;

      FUNCTION Identity_Resolve
        (ID      : IN number)
         RETURN device
      IS
      (
         CASE ID IS
         WHEN Standard_Keyboard    => Standard_Keyboard,
         WHEN Standard_Mouse       => Standard_Mouse,
         WHEN Mouse_With_Scroll    => Mouse_With_Scroll,
         WHEN Mouse_With_5_Buttons => Mouse_With_5_Buttons,
         WHEN OTHERS               => Unrecognised
      );

      -- Default values in this array are for the while loop.
      ID_Bytes          : numbers(1 .. 2) := (Data_Acknowledged, 256);
      Identified_Device : device;
   BEGIN
      Object.Flush;

      IF
         NOT Port_2 AND THEN
         NOT Object.Send_Keyboard_Command(Keyboard_Identity)
      THEN
         Object.Port_1_Device := Unrecognised;
         RETURN;
      ELSIF
         Port_2 AND THEN
         NOT Object.Send_Mouse_Command(Mouse_Identity)
      THEN
         Object.Port_2_Device := Unrecognised;
         RETURN;
      END IF;

      WHILE
         ID_Bytes(1)  = Data_Acknowledged
      LOOP
         ID_Bytes(1) := Input_Byte(Data);
      END LOOP;

      ID_Bytes(2) := Input_Byte(Data);
      Object.Flush;
      Identified_Device := Identity_Resolve(ID_Bytes(1));

      CASE
         ID_Bytes(1)
      IS
         WHEN Standard_Keyboard    =>
            Log("PS/2 port " & Port_Image & " has a standard keyboard.",
               nominal);
         WHEN Standard_Mouse       =>
            Object.Mouse_Support := true;
            Log("PS/2 port " & Port_Image & " has a standard mouse.",
               nominal);
         WHEN Mouse_With_Scroll    =>
            Object.Mouse_Support := true;
            Log("PS/2 port " & Port_Image & " has a scroll-wheel mouse.",
               nominal);
         WHEN Mouse_With_5_Buttons =>
            Object.Mouse_Support := true;
            Log("PS/2 port " & Port_Image & " has a 5-button mouse.",
               nominal);
         WHEN OTHERS               =>
            Log("PS/2 port " & Port_Image & " has an unrecognised device -" &
               number'image(ID_Bytes(1)) & number'image(ID_Bytes(2)) & '.',
               warning);
      END CASE;

      IF
         NOT Port_2
      THEN
         Object.Port_1_Device := Identified_Device;
      ELSE
         Object.Port_2_Device := Identified_Device;
      END IF;
   END Identify_Device;

   FUNCTION Send_Configuration
     (Object    : IN controller)
      RETURN boolean
   IS
      Configuration_Settings : CONSTANT configuration :=
         Input_Controller.Current_Configuration;

      Configuration_Byte     : CONSTANT number RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Configuration_Settings'address;
   BEGIN
      IF
         NOT Object.Send_Controller_Command(Configuration_Write)
      THEN
         RETURN false;
      ELSE
         RETURN Object.Send_Data(Configuration_Byte, Verify => false);
      END IF;
   END Send_Configuration;

   FUNCTION Send_Typematics
     (Object             : IN controller;
      Port_2             : IN boolean := false)
      RETURN boolean
   IS
      Typematic_Settings : CONSTANT typematics :=
         Input_Controller.Current_Typematics;

      Typematics_Byte    : CONSTANT number RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Typematic_Settings'address;
   BEGIN
      IF
         Port_2                   AND THEN
        (NOT Object.Port_2_Support OR ELSE
            Object.Port_2_Device /= Standard_Keyboard)
      THEN
         RETURN false;
      END IF;

      IF
         NOT Object.Send_Keyboard_Command(Typematics_Write, Port_2)
      THEN
         RETURN false;
      ELSE
         RETURN Object.Send_Data(Typematics_Byte);
      END IF;
   END Send_Typematics;

   FUNCTION Send_Scancode_Set
     (Object    : IN controller;
      Port_2    : IN boolean := false)
      RETURN boolean
   IS
   BEGIN
      IF
         Port_2                   AND THEN
        (NOT Object.Port_2_Support OR ELSE
             Object.Port_2_Device /= Standard_Keyboard)
      THEN
         RETURN false;
      END IF;

      IF
         NOT Input_Controller.Send_Keyboard_Command
            (Scancode_Set_Options, Port_2)
      THEN
         RETURN false;
      ELSE
         RETURN Input_Controller.Send_Data
           (Input_Controller.Current_Scancode_Set);
      END IF;
   END Send_Scancode_Set;

   FUNCTION Check_Condition
      RETURN controller_condition
   IS
      (Input_Controller.Current_Condition);

   FUNCTION Mouse_Exists
      RETURN boolean
   IS
      (Input_Controller.Mouse_Support);

   PROCEDURE Setup -- TODO: Perhaps break this procedure up into separate ones.
   IS
      -- Make read-only copies of the variable so a warning doesn't appear.
      Configuration_Settings     : CONSTANT configuration :=
         Input_Controller.Current_Configuration;
      Typematics_Settings        : CONSTANT    typematics :=
         Input_Controller.Current_Typematics;

      Current_Configuration_Byte : CONSTANT number RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Configuration_Settings'address;

      Old_Configuration_Byte     :          number RANGE 0 .. 16#FF#;

      Current_Typematics_Byte    : CONSTANT number RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Typematics_Settings'address;
   BEGIN
      -- First, stop any interrupts that'll ruin our expected IO communication.
      -- An alternative would be to disable interrupts in the configuration,
      -- but this is quicker and more reliable.
      Disable_Interrupts;

      -- Flush anything in the output buffer to make sure.
      Input_Controller.Flush;

      -- Get the old configuration before we modify it. Needed to make sure
      -- a port 2 exists, which is nearly always where the PS/2 mouse is.
      IF
         NOT Input_Controller.Send_Controller_Command(Configuration_Read)
      THEN
         Log("Could not read default PS/2 controller configuration.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      ELSE
         Old_Configuration_Byte := Input_Byte(Data);
      END IF;

      -- Disable any scanning or reporting by the devices. Note that the
      -- disabling commands are the same byte. The difference between the
      -- two send functions here are to do with default ports.
      IF
         NOT Input_Controller.Send_Keyboard_Command(Scanning_Disable)
      THEN
         Log("Failed to disable PS/2 port 1 data.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      ELSIF
         Bit_Test(Old_Configuration_Byte, 5) -- Check for "Port_2_Clock".
      THEN
         IF
            NOT Input_Controller.Send_Mouse_Command(Reporting_Disable)
         THEN
            Log("Failed to disable PS/2 port 2 data.", fatal);
            Input_Controller.Current_Condition := unreliable;
            RETURN;
         ELSE -- Don't enable support before the port gets tested.
            Input_Controller.Current_Configuration.Port_2_Enabled := true;
            Input_Controller.Current_Configuration.Port_2_Clock   := true;
         END IF;
      END IF;

      -- Run the controller and port tests.
      -- First, test the PS/2 controller itself.
      Log("Testing PS/2 controller.", nominal);
      IF
         NOT Input_Controller.Send_Controller_Command
            (Test_Controller_Begin) OR ELSE
         Input_Controller.Receive(Data) /= Test_Controller_Pass
      THEN
         Input_Controller.Current_Condition := unreliable;
         Log("PS/2 controller test fail.", fatal);
         RETURN;
      END IF;
      Log("PS/2 controller test success.", nominal);

      -- Secondly, test the first PS/2 data port for a device.
      Log("Testing PS/2 port 1.");
      IF
         NOT Input_Controller.Send_Controller_Command
            (Test_Port_1_Begin) OR ELSE
         Input_Controller.Receive(Data) /= Test_Port_Pass
      THEN
         Input_Controller.Current_Condition := unreliable;
         Log("PS/2 port 1 test fail.", fatal);
         RETURN;
      END IF;
      Log("PS/2 port 1 test success.", nominal);

      -- Now determine if we have a second port and check the port 2 clock if
      -- it is enabled already by the system. See the configuration record.
      IF
         Input_Controller.Current_Configuration.Port_2_Clock
      THEN
         Log("Testing PS/2 port 2.");
         IF
            NOT Input_Controller.Send_Controller_Command
               (Test_Port_2_Begin) OR ELSE
            Input_Controller.Receive(Data) /= Test_Port_Pass
         THEN
            -- Since the previous port worked, we will ignore this extra port.
            Log("PS/2 port 2 test fail.", warning);
         ELSE
            Log("PS/2 port 2 test success.", nominal);
            Input_Controller.Port_2_Support := true;
         END IF;
      ELSE
         Log("PS/2 controller does not support port 2.", warning);
      END IF;

      -- Flush any incoming bytes left over from the tests and leave.
      -- Depends on the PS/2 controller's implementation.
      Input_Controller.Flush;

      -- See what devices are connected to the ports.
      Input_Controller.Identify_Device(Port_2 => false);
      IF
         Input_Controller.Port_2_Support
      THEN
         Input_Controller.Identify_Device(Port_2 =>  true);
      END IF;

      -- Write my configuration.
      IF
         NOT Input_Controller.Send_Configuration
      THEN
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Write my typematic features configuration.
      IF
         NOT Input_Controller.Send_Typematics
      THEN
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Use the default scancode set (set 2).
      IF
         NOT Input_Controller.Send_Scancode_Set
      THEN
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Enable interrupts from the first PS2 port.
      IF
         NOT Input_Controller.Send_Controller_Command(Port_1_Enable)
      THEN
         Log("Couldn't enable PS/2 port 1.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Enable interrupts from the second PS2 port if it exists,
      -- but it is not vital.
      IF
         Input_Controller.Port_2_Support
      THEN
         IF
            NOT Input_Controller.Send_Controller_Command(Port_1_Enable)
         THEN
            Log("Couldn't enable PS/2 port 2.", fatal);
            Input_Controller.Port_2_Support := false; -- Unreliable port 2.
         END IF;
      END IF;

      -- Now re-enable scanning and/or reporting.
      IF
         NOT Input_Controller.Send_Keyboard_Command(Scanning_Enable)
      THEN
         Log("Failed to enable PS/2 port 1 data.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      ELSIF
         Input_Controller.Port_2_Support
      THEN
         IF
            NOT Input_Controller.Send_Mouse_Command(Reporting_Enable)
         THEN
            Log("Failed to enable PS/2 port 2 data.", fatal);
            Input_Controller.Port_2_Support := false; -- Unreliable port 2.
            RETURN;
         END IF;
      END IF;

      -- Flush the output buffer again just in case.
      Input_Controller.Flush;

      -- Set the controller's condition as functional as we have hopefully
      -- got to this line without any raised errors.
      Input_Controller.Current_Condition := functional;

      -- Finally, re-enable interrupts.
      Enable_Interrupts;
   END Setup;
END HAVK_Kernel.PS2;
