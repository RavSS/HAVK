WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.PS2
IS
   FUNCTION Ready_To_Receive(
      Object    : IN OUT controller'class)
   RETURN boolean IS
      Current_Status_Byte : CONSTANT num := INB(command'enum_rep);
      Current_Status      : CONSTANT status
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Current_Status_Byte'address;
   BEGIN
      Object.Last_Status := Current_Status;
      RETURN Current_Status.Output_Ready;
   END Ready_To_Receive;

   FUNCTION Ready_To_Send(
      Object    : IN OUT controller'class)
   RETURN boolean IS
      Current_Status_Byte : CONSTANT num := INB(command'enum_rep);
      Current_Status      : CONSTANT status
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Current_Status_Byte'address;
   BEGIN
      Object.Last_Status := Current_Status;
      RETURN NOT Current_Status.Input_Full; -- Invert it so it makes sense.
   END Ready_To_Send;

   FUNCTION Send(
      Object    : IN OUT controller;
      Port_Type : IN port;
      Byte      : IN num;
      Port_2    : IN boolean;
      Verify    : IN boolean)
   RETURN boolean IS
      Check     : response;
   BEGIN
      IF Port_2 THEN
         OUTB(command'enum_rep, port_2_data'enum_rep); -- Avoid recursion.
      END IF;

      FOR R IN 1 .. Object.Retry_Rate LOOP
         IF Object.Ready_To_Send THEN
            IF NOT Verify THEN
               OUTB(Port_Type'enum_rep, Byte);
               RETURN true;
            END IF;

            FOR I IN 1 .. Object.Retry_Rate LOOP
               OUTB(Port_Type'enum_rep, Byte);
               Check := Object.Receive(data);
               EXIT WHEN Check = data_acknowledged;
            END LOOP;

            RETURN (IF Check = data_acknowledged THEN true ELSE false);
         END IF;
      END LOOP;

      RETURN false;
   END Send;

   FUNCTION Send_Controller_Command(
      Object    : IN OUT controller;
      Operation : IN controller_command)
   RETURN boolean IS
   (Object.Send(command, Operation'enum_rep, false, false));

   FUNCTION Send_Keyboard_Command(
      Object    : IN OUT controller;
      Operation : IN keyboard_command;
      Port_2    : IN boolean := false)
   RETURN boolean IS
   (Object.Send(data, Operation'enum_rep, Port_2, true));

   FUNCTION Send_Mouse_Command(
      Object    : IN OUT controller;
      Operation : IN mouse_command;
      Port_2    : IN boolean := true)
   RETURN boolean IS
   (Object.Send(data, Operation'enum_rep, Port_2, true));

   FUNCTION Send_Data(
      Object    : IN OUT controller;
      Byte_Data : IN num;
      Port_2    : IN boolean := false;
      Verify    : IN boolean := true)
   RETURN boolean IS
   (Object.Send(data, Byte_Data, Port_2, Verify));

   FUNCTION Receive(
      Object     : IN OUT controller;
      Port_Type  : IN port)
   RETURN response IS
      -- Temporary subtype to avoid a size warning.
      SUBTYPE byte IS num RANGE 0 .. 16#FF#;
      FUNCTION Byte_To_Response IS NEW Ada.Unchecked_Conversion(
         byte,
         response);

      Response_Byte : byte;
   BEGIN
      FOR R IN 1 .. Object.Retry_Rate LOOP
         IF Object.Ready_To_Receive THEN
            Response_Byte := INB(Port_Type'enum_rep);
            IF -- Lazy workaround due to exception handling restrictions.
                    Response_Byte /= test_port_pass'enum_rep       AND
               THEN Response_Byte /= test_controller_pass'enum_rep AND
               THEN Response_Byte /= data_acknowledged'enum_rep    AND
               THEN Response_Byte /= failure'enum_rep              AND
               THEN Response_Byte /= data_resend'enum_rep
            THEN
               Log("Unexpected PS/2 input byte" & Response_Byte'img & ".",
                  warning);
               RETURN failure;
            ELSE
               RETURN Byte_To_Response(Response_Byte);
            END IF;
         END IF;
      END LOOP;

      RETURN failure;
   END Receive;

   PROCEDURE Flush(
      Object  : IN OUT controller)
   IS
      Discard : num; -- Magic variable name for GNAT. See pragma "unmodified".
   BEGIN
      WHILE Object.Ready_To_Receive LOOP
         Discard := INB(data'enum_rep);
      END LOOP;
   END Flush;

   FUNCTION Test(
      Object  : IN OUT controller)
   RETURN boolean IS
   BEGIN
      -- First, test the PS/2 controller itself.
      Log("Testing PS/2 controller.", nominal);
      IF
         NOT Object.Send_Controller_Command(test_controller_begin) OR
         ELSE Object.Receive(data) /= test_controller_pass
      THEN
         Object.Current_Condition := unreliable;
         Log("PS/2 controller test failure.", fatal);
         RETURN false;
      END IF;
      Log("PS/2 controller test successful.", nominal);

      -- Secondly, test the first PS/2 data port for a device.
      Log("Testing PS/2 port 1.");
      IF
         NOT Object.Send_Controller_Command(test_port_1_begin) OR
         ELSE Object.Receive(data) /= test_port_pass
      THEN
         Object.Current_Condition := unreliable;
         Log("PS/2 port 1 test failure.", fatal);
         RETURN false;
      END IF;
      Log("PS/2 port 1 test successful.", nominal);

      -- Finally, determine if we have a second port and check the port 2 clock
      -- if it is enabled already by the system. See the configuration record.
      IF
         Object.Send_Controller_Command(configuration_read) AND
         THEN BT(INB(data'enum_rep), 5) -- `Receive()` cannot handle this.
      THEN
         Log("Testing PS/2 port 2.");
         IF
            NOT Object.Send_Controller_Command(test_port_2_begin) OR
            ELSE Object.Receive(data) /= test_port_pass
         THEN
            -- Since the previous port worked, we will ignore this extra port.
            Log("PS/2 port 2 test failure.", warning);
         ELSE
            Log("PS/2 port 2 test successful.", nominal);
            Object.Current_Configuration.Port_2_Enabled := true;
            Object.Current_Configuration.Port_2_Clock   := true;
            Object.Port_2_Support := true;
         END IF;
      ELSE
         Log("PS/2 controller cannot support port 2.", warning);
      END IF;

      -- Flush any incoming bytes left over from the tests and leave.
      -- Depends on the PS/2 controller's implementation.
      Object.Flush;
      RETURN true;
   END Test;

   -- The device ID command and disable/enable scancode/reporting commands
   -- are the same across devices (0xFE, 0xF5, and 0xF4 respectively),
   -- but the device IDs themselves are not.
   FUNCTION Identify_Device(
      Object   : IN OUT controller;
      Port_No  : IN num)
   RETURN device IS
      -- Default values in this array are for the while loop.
      ID_Bytes : nums(1 .. 2) := (data_acknowledged'enum_rep, 256);
   BEGIN
      Object.Flush;

      IF
         Port_No = 1 AND
         THEN NOT Object.Send_Keyboard_Command(keyboard_identity)
      THEN
         RETURN unrecognised;
      ELSIF
         Port_No = 2 AND
         THEN NOT Object.Send_Mouse_Command(mouse_identity)
      THEN
         RETURN unrecognised;
      END IF;

      WHILE ID_Bytes(1) = data_acknowledged'enum_rep LOOP
         ID_Bytes(1) := INB(data'enum_rep);
      END LOOP;

      ID_Bytes(2) := INB(data'enum_rep);
      Object.Flush;

      -- TOOD: Is there a way to properly image an enumeration variable
      -- during runtime?
      CASE ID_Bytes(1) IS
         WHEN standard_keyboard'enum_rep    =>
            Log("PS/2 port" & Port_No'img & " is a " &
               standard_keyboard'img & ".", nominal);
            RETURN standard_keyboard;
         WHEN standard_mouse'enum_rep       =>
            Log("PS/2 port" & Port_No'img & " is a " &
               standard_mouse'img & ".", nominal);
            Object.Mouse_Support := true;
            RETURN standard_mouse;
         WHEN mouse_with_scroll'enum_rep    =>
            Log("PS/2 port" & Port_No'img & " is a " &
               mouse_with_scroll'img & ".", nominal);
            Object.Mouse_Support := true;
            RETURN mouse_with_scroll;
         WHEN mouse_with_5_buttons'enum_rep =>
            Log("PS/2 port" & Port_No'img & " is a " &
               mouse_with_5_buttons'img & ".", nominal);
            Object.Mouse_Support := true;
            RETURN mouse_with_5_buttons;
         WHEN OTHERS                        =>
            Log("PS/2 port" & Port_No'img & " is "   &
               unrecognised'img & " -" &
               ID_Bytes(1)'img & ID_Bytes(2)'img & ".", warning);
            RETURN unrecognised;
      END CASE;
   END Identify_Device;

   FUNCTION Send_Configuration(
      Object    : IN OUT controller)
   RETURN boolean IS
      Configuration_Settings : CONSTANT configuration :=
         Input_Controller.Current_Configuration;

      Configuration_Byte     : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Configuration_Settings'address;
   BEGIN
      IF NOT Object.Send_Controller_Command(configuration_write) THEN
         Log("Failed to write PS/2 configuration.", fatal);
         RETURN false;
      END IF;

      IF NOT Object.Send_Data(Configuration_Byte, Verify => false) THEN
         Log("Couldn't send configuration data to the PS/2 controller.",
            fatal);
         RETURN false;
      END IF;

      RETURN true;
   END Send_Configuration;

   FUNCTION Send_Typematics(
      Object             : IN OUT controller;
      Port_2             : IN boolean := false)
   RETURN boolean          IS
      Typematic_Settings : CONSTANT typematics :=
         Input_Controller.Current_Typematics;

      Typematics_Byte    : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Typematic_Settings'address;
   BEGIN
      IF
         Port_2 AND THEN (NOT Object.Port_2_Support OR
         ELSE Object.Port_2_Device /= standard_keyboard)
      THEN
         Log("Attempt to set typematics on non-existent port 2 PS/2 keyboard.",
            warning);
         RETURN false;
      END IF;

      IF
         NOT Object.Send_Keyboard_Command(typematics_write, Port_2)
      THEN
         Log("Failed to write PS/2 typematics.", warning);
         RETURN false;
      END IF;

      IF NOT Object.Send_Data(Typematics_Byte) THEN
         Log("Couldn't send typematics data to the PS/2 keyboard.", warning);
         RETURN false;
      END IF;

      RETURN true;
   END Send_Typematics;

   FUNCTION Send_Scancode_Set(
      Object    : IN OUT controller;
      Port_2    : IN boolean := false)
   RETURN boolean IS
   BEGIN
      IF
         Port_2 AND THEN (NOT Object.Port_2_Support OR
         ELSE Object.Port_2_Device /= standard_keyboard)
      THEN
         Log("Attempt to change scancode set on non-existent port 2 " &
            "PS/2 keyboard.", warning);
         RETURN false;
      END IF;

      IF
         NOT Input_Controller.Send_Keyboard_Command(scancode_set_options,
            Port_2)
      THEN
         Log("Failed to set keyboard scancode type.", fatal);
         RETURN false;
      END IF;

      IF
         NOT Input_Controller.Send_Data(
            Input_Controller.Current_Scancode_Set'enum_rep)
      THEN
         Log("Couldn't send set to PS/2 keyboard.", fatal);
         RETURN false;
      END IF;

      Log("PS/2 keyboard now using scancode set" &
         num'image(Object.Current_Scancode_Set'enum_rep) & ".");
      RETURN true;
   END Send_Scancode_Set;

   FUNCTION Check_Condition
   RETURN controller_condition IS
   (Input_Controller.Current_Condition);

   FUNCTION Mouse_Exists
   RETURN boolean              IS
   (Input_Controller.Mouse_Support);

   PROCEDURE Setup
   IS
      -- Make read-only copies of the variable so a warning doesn't appear.
      Configuration_Settings     : CONSTANT configuration :=
         Input_Controller.Current_Configuration;
      Typematics_Settings        : CONSTANT    typematics :=
         Input_Controller.Current_Typematics;

      Current_Configuration_Byte : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Configuration_Settings'address;

      Current_Typematics_Byte    : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Typematics_Settings'address;
   BEGIN
      -- First, stop any interrupts that'll ruin our expected IO communication.
      CLI;

      -- Disable both PS2 ports.
      IF NOT Input_Controller.Send_Controller_Command(port_1_disable) THEN
         Log("Failed to disable PS/2 port 1.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      IF NOT Input_Controller.Send_Controller_Command(port_2_disable) THEN
         Log("Failed to disable PS/2 port 2.", fatal);
         RETURN;
      END IF;

      -- Flush anything in the output buffer.
      Input_Controller.Flush;

      -- Run the controller and port tests.
      IF NOT Input_Controller.Test THEN
         Log("PS/2 controller failed test(s).", fatal);
         RETURN;
      END IF;

      -- Disable any scanning or reporting by the devices. Note that the
      -- disabling commands are the same byte. The difference between the
      -- two send functions here are to do with default ports.
      IF NOT Input_Controller.Send_Keyboard_Command(scanning_disable) THEN
         Log("Failed to disable PS/2 port 1 data.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      ELSIF Input_Controller.Port_2_Support THEN
         IF NOT Input_Controller.Send_Mouse_Command(reporting_disable) THEN
            Log("Failed to disable PS/2 port 2 data.", fatal);
            Input_Controller.Current_Condition := unreliable;
            RETURN;
         END IF;
      END IF;

      -- See what devices are connected to the ports.
      Input_Controller.Port_1_Device := Input_Controller.Identify_Device(1);
      Input_Controller.Port_2_Device := Input_Controller.Identify_Device(2);

      -- Write my configuration.
      IF NOT Input_Controller.Send_Configuration THEN
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Write my typematic features configuration.
      IF NOT Input_Controller.Send_Typematics THEN
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Use the default scancode set (set 2).
      IF NOT Input_Controller.Send_Scancode_Set THEN
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Enable the first PS2 port.
      IF NOT Input_Controller.Send_Controller_Command(port_1_enable) THEN
         Log("Couldn't enable PS/2 port 1.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Enable the second PS2 port if it exists, but it's not vital.
      IF Input_Controller.Port_2_Support THEN
         IF NOT Input_Controller.Send_Controller_Command(port_1_enable) THEN
            Log("Couldn't enable PS/2 port 2.", fatal);
            Input_Controller.Port_2_Support := false; -- Unreliable port 2.
         END IF;
      END IF;

      -- Now re-enable scanning and/or reporting.
      IF NOT Input_Controller.Send_Keyboard_Command(scanning_enable) THEN
         Log("Failed to enable PS/2 port 1 data.", fatal);
         Input_Controller.Current_Condition := unreliable;
         RETURN;
      ELSIF Input_Controller.Port_2_Support THEN
         IF NOT Input_Controller.Send_Mouse_Command(reporting_enable) THEN
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
      STI;
   END Setup;
END HAVK_Kernel.PS2;
