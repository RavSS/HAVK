-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System PS/2 Driver                      --
-- Filename        -- havk_ps2.adb                                           --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.Utility;
USE
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.Utility;

PACKAGE BODY HAVK_PS2
WITH
   Refined_State => (Controller_State => (Current_Scancode_Set,
                                          Current_Condition,
                                          Current_Configuration,
                                          Current_Typematics,
                                          Port_2_Support, Mouse_Support,
                                          Port_1_Device, Port_2_Device))
IS
   PRAGMA Warnings(GNAT, off, "types for unchecked conversion *",
      Reason => "This was checked by `gnatprove` instead to be acceptable.");

   PROCEDURE Ready
     (Is_Ready : OUT boolean;
      Sending  : IN boolean := false)
   IS
      FUNCTION To_Status IS NEW Ada.Unchecked_Conversion
        (source => number, target => status);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "The PS/2 controller will return an 8-bit status in a byte.");

      Current_Status_Byte : CONSTANT number :=
         Input_Byte(command_port'enum_rep);
      Current_Status      : CONSTANT status := To_Status(Current_Status_Byte);
   BEGIN
      IF
         Port_1_Device /= unrecognised OR ELSE
         Port_2_Device /= unrecognised
      THEN
         Is_Ready := (IF Sending THEN -- Invert "Input_Full" so it makes sense.
            NOT Current_Status.Input_Full ELSE Current_Status.Output_Ready);
      ELSE
         Is_Ready := false;
      END IF;
   END Ready;

   PROCEDURE Send
     (Success : OUT boolean;
      Data    : IN generic_data;
      Port_2  : IN boolean := false)
   IS
      FUNCTION To_Byte IS NEW Ada.Unchecked_Conversion
        (source => generic_data, target => number);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "The format for the data is manually checked (see representations).");

      Is_Ready      : boolean;
      Send_Response : response;
   BEGIN
      FOR
         Check_Ready_Attempt IN 1 .. Retry_Rate
      LOOP
         Ready(Is_Ready, Sending => true);

         IF
            Is_Ready
         THEN
            FOR
               Sending_Attempt IN 1 .. Retry_Rate
            LOOP
               IF
                  Port_2
               THEN
                  Output_Byte(command_port'enum_rep, port_2_data'enum_rep);
               END IF;

               -- Static "enum_rep" for an odd bug workaround (GNAT CE 2020).
               Output_Byte((IF Port_Type = data_port THEN data_port'enum_rep
                  ELSE command_port'enum_rep), To_Byte(Data) AND 16#FF#);

               IF
                  NOT Verify
               THEN
                  Success := true;
                  RETURN;
               END IF;

               Receive(Send_Response, data_port);

               IF
                  Send_Response = data_acknowledged
               THEN
                  Success := true;
                  RETURN;
               END IF;
            END LOOP;
         END IF;
      END LOOP;

      Success := false;
   END Send;

   PROCEDURE Send_Controller_Command IS NEW Send
     (generic_data => controller_command,
      Port_Type    => command_port,
      Verify       => false);

   PROCEDURE Send_Device_Command IS NEW Send
     (generic_data => keyboard_command,
      Port_Type    => data_port,
      Verify       => true);

   PROCEDURE Send_Device_Command IS NEW Send
     (generic_data => mouse_command,
      Port_Type    => data_port,
      Verify       => true);

   PROCEDURE Send_Configuration IS NEW Send
     (generic_data => configuration,
      Port_Type    => data_port,
      Verify       => false);

   PROCEDURE Send_Typematics IS NEW Send
     (generic_data => typematics,
      Port_Type    => data_port,
      Verify       => true);

   PROCEDURE Send_Scancode_Set IS NEW Send
     (generic_data => scancode_set,
      Port_Type    => data_port,
      Verify       => true);

   PROCEDURE Receive
     (Message   : OUT response;
      Port_Type : IN port)
   IS
      FUNCTION Enum_Val_Check IS NEW Ada.Unchecked_Conversion
        (source => number, target => response);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "Validity is manually checked after it's called.");

      Is_Ready     : boolean;
      Raw_Response : number RANGE 0 .. 2**8 - 1;
   BEGIN
      FOR
         R IN 1 .. Retry_Rate
      LOOP
         Ready(Is_Ready, Sending => false);

         IF
            Is_Ready
         THEN
            -- Static "enum_rep" for an odd bug workaround (GNAT CE 2020).
            Raw_Response := Input_Byte(IF Port_Type = data_port THEN
               data_port'enum_rep ELSE command_port'enum_rep);

            PRAGMA Warnings(GNATprove, off,
               "attribute Valid is assumed to return True",
               Reason => "Failure is indicated on an invalid response.");
            PRAGMA Warnings(GNATprove, off,
               "unreachable branch",
               Reason => "It is reachable if the response is invalid.");
            Message := (IF Enum_Val_Check(Raw_Response)'valid THEN
               Enum_Val_Check(Raw_Response) ELSE failure);

            RETURN;
         END IF;
      END LOOP;

      Message := failure;
   END Receive;

   PROCEDURE Flush
   IS
      -- Magic variable name for GNAT. See pragma "Unmodified".
      Discard     : number;
      Full_Buffer : boolean;
   BEGIN
      LOOP
         Ready(Full_Buffer, Sending => false);
         EXIT WHEN NOT Full_Buffer;
         Discard := Input_Byte(data_port'enum_rep);
      END LOOP;
   END Flush;

   -- The device ID command and disable/enable scancode/reporting commands
   -- are the same across devices (0xF2, 0xF5, and 0xF4 respectively),
   -- but the device IDs themselves are not.
   PROCEDURE Identify_Device
     (New_Device : OUT device;
      Port_2     : IN boolean)
   IS
      Port_Image : CONSTANT character := (IF Port_2 THEN '2' ELSE '1');

      -- Default values in this array are for the while loop.
      Identity          : ARRAY(number RANGE 1 .. 2) OF ALIASED number :=
        (number'last, number'last);
      Identified_Device : device;
      Successful        : boolean;
   BEGIN
      Flush;

      -- The keyboard and mouse identity bytes are the same.
      Send_Device_Command(Successful, keyboard_identity, Port_2);

      IF
         NOT Successful
      THEN
         New_Device := unrecognised;
         RETURN;
      END IF;

      WHILE
         Identity(1) = number'last OR ELSE -- Wait for the default to change.
         Identity(1) = 16#FA# -- On Bochs, wait for the "data_acknowledged"s.
      LOOP
         Identity(1) := Input_Byte(data_port'enum_rep);
      END LOOP;

      Identity(2) := Input_Byte(data_port'enum_rep);
      Flush;

      DECLARE -- Convert the "byte" (a 64-bit value) to the device type.
         FUNCTION Enum_Val_Check IS NEW Ada.Unchecked_Conversion
           (source => number, target => device);
         PRAGMA Annotate(GNATprove, False_Positive,
            "type with constraints on bit representation *",
            "We check it for validity after conversion.");

         Unchecked_Identity : CONSTANT device := Enum_Val_Check(Identity(1));
      BEGIN
         PRAGMA Warnings(GNATprove, off,
            "attribute Valid is assumed to return True",
            Reason => "If invalid, the variable will be discarded.");
         IF
            NOT Unchecked_Identity'valid
         THEN
            Log("PS/2 port " & Port_Image & " has an unrecognised device - " &
               integer(Identity(1))'image & ' ' & integer(Identity(2))'image &
               '.', Tag => PS2_Tag, Warn => true);
            New_Device := unrecognised;
            RETURN;
         END IF;

         Identified_Device := Unchecked_Identity;
      END;

      CASE
         Identified_Device
      IS
         WHEN standard_keyboard    =>
            Log("PS/2 port " & Port_Image & " has a standard keyboard.",
               Tag => PS2_Tag);
         WHEN standard_mouse       =>
            Mouse_Support := true;
            Log("PS/2 port " & Port_Image & " has a standard mouse.",
               Tag => PS2_Tag);
         WHEN mouse_with_scroll    =>
            Mouse_Support := true;
            Log("PS/2 port " & Port_Image & " has a scroll-wheel mouse.",
               Tag => PS2_Tag);
         WHEN mouse_with_5_buttons =>
            Mouse_Support := true;
            Log("PS/2 port " & Port_Image & " has a 5-button mouse.",
               Tag => PS2_Tag);
         WHEN unrecognised         => -- Still technically a valid enumeration.
            Log("PS/2 port " & Port_Image & " has an unrecognised device - " &
               integer(Identity(1))'image & ' ' & integer(Identity(2))'image &
               '.', Tag => PS2_Tag, Warn => true);
      END CASE;

      New_Device := Identified_Device;
   END Identify_Device;

   FUNCTION Check_Condition
      RETURN controller_condition
   IS
     (Current_Condition);

   FUNCTION Mouse_Exists
      RETURN boolean
   IS
     (Mouse_Support);

   PROCEDURE Setup -- TODO: Perhaps break this procedure up into separate ones.
   IS
      Old_Configuration_Byte : number RANGE 0 .. 2**8 - 1;
      New_Device             : device;
      Successful             : boolean;
      Message                : response;
   BEGIN
      -- Flush anything in the output buffer to make sure.
      Flush;

      -- Get the old configuration before we modify it. Needed to make sure
      -- a port 2 exists, which is nearly always where the PS/2 mouse is.
      Send_Controller_Command(Successful, configuration_read);

      IF
         NOT Successful
      THEN
         Log("Could not read default PS/2 controller configuration.",
            Tag => PS2_Tag, Critical => true);
         Current_Condition := unreliable;
         RETURN;
      ELSE
         Old_Configuration_Byte := Input_Byte(data_port'enum_rep);
      END IF;

      -- Disable any scanning or reporting by the devices. Note that the
      -- disabling commands are the same byte. The difference between the
      -- two send functions here are to do with default ports.
      Send_Device_Command(Successful, scanning_disable);

      IF
         NOT Successful
      THEN
         Log("Failed to disable PS/2 port 1 data.", Tag => PS2_Tag,
            Critical => true);
         Current_Condition := unreliable;
         RETURN;
      ELSIF -- Check for "Port_2_Clock".
         boolean'val(Shift_Right(Old_Configuration_Byte, 5) AND 1)
      THEN
         Send_Device_Command(Successful, reporting_disable, Port_2 => true);

         IF
            NOT Successful
         THEN
            Log("Failed to disable PS/2 port 2 data.", Tag => PS2_Tag,
               Critical => true);
            Current_Condition := unreliable;
            RETURN;
         ELSE -- Don't enable support before the port gets tested.
            Current_Configuration.Port_2_Enabled := true;
            Current_Configuration.Port_2_Clock   := true;
         END IF;
      END IF;

      -- Run the controller and port tests. First, test the PS/2 controller
      -- itself.
      Log("Testing PS/2 controller.", Tag => PS2_Tag);
      Send_Controller_Command(Successful, test_controller_begin);
      Receive(Message, data_port);

      IF
         NOT Successful OR ELSE
         Message /= test_controller_pass
      THEN
         Current_Condition := unreliable;
         Log("PS/2 controller test fail.", Tag => PS2_Tag, Critical => true);
         RETURN;
      ELSE
         Log("PS/2 controller test success.", Tag => PS2_Tag);
      END IF;

      -- Secondly, test the first PS/2 data port for a device.
      Log("Testing PS/2 port 1.", Tag => PS2_Tag);
      Send_Controller_Command(Successful, test_port_1_begin);
      Receive(Message, data_port);

      IF
         NOT Successful OR ELSE
         Message /= test_port_pass
      THEN
         Current_Condition := unreliable;
         Log("PS/2 port 1 test fail.", Tag => PS2_Tag, Critical => true);
         RETURN;
      ELSE
         Log("PS/2 port 1 test success.", Tag => PS2_Tag);
      END IF;

      -- Now determine if we have a second port and check the port 2 clock if
      -- it is enabled already by the system. See the configuration record.
      IF
         Current_Configuration.Port_2_Clock
      THEN
         Log("Testing PS/2 port 2.", Tag => PS2_Tag);
         Send_Controller_Command(Successful, test_port_2_begin);
         Receive(Message, data_port);

         IF
            NOT Successful OR ELSE
            Message /= test_port_pass
         THEN
            -- Since the previous port worked, we will ignore this extra port.
            Log("PS/2 port 2 test fail.", Tag => PS2_Tag, Warn => true);
         ELSE
            Log("PS/2 port 2 test success.", Tag => PS2_Tag);
            Port_2_Support := true;
         END IF;
      ELSE
         Log("PS/2 controller does not support port 2.", Tag => PS2_Tag,
            Warn => true);
      END IF;

      -- Flush any incoming bytes left over from the tests. It depends on the
      -- PS/2 controller's implementation.
      Flush;

      -- -- See what device is connected to port 1. Nearly always a keyboard.
      Identify_Device(New_Device, Port_2 => false);
      Port_1_Device := New_Device;

      IF -- Check port 2 if we have support for it.
         Port_2_Support
      THEN
         Identify_Device(New_Device, Port_2 => true);
         Port_2_Device := New_Device;
      END IF;

      -- Write my configuration.
      Send_Controller_Command(Successful, configuration_write);

      IF
         NOT Successful
      THEN
         Current_Condition := unreliable;
         Log("Failed to write PS/2 controller configuration.", Tag => PS2_Tag,
            Critical => true);
         RETURN;
      ELSE
         Send_Configuration(Successful, Current_Configuration);

         IF
            NOT Successful
         THEN
            Current_Condition := unreliable;
            Log("Failed to send PS/2 controller configuration.",
               Tag => PS2_Tag, Critical => true);
            RETURN;
         END IF;
      END IF;

      -- Write my typematic features configuration.
      Send_Device_Command(Successful, typematics_write); -- Port 1 first.

      IF
         NOT Successful
      THEN
         Current_Condition := unreliable;
         Log("Failed to write PS/2 keyboard typematics on port 1.",
            Tag => PS2_Tag, Critical => true);
         RETURN;
      ELSE
         Send_Typematics(Successful, Current_Typematics);

         IF
            NOT Successful
         THEN
            Current_Condition := unreliable;
            Log("Failed to send PS/2 keyboard typematics on port 1.",
               Tag => PS2_Tag, Critical => true);
            RETURN;
         END IF;
      END IF;

      IF -- Now do port 2's typematics if there's a keyboard on it.
         Port_2_Device = standard_keyboard AND THEN
         Port_2_Support
      THEN
         Send_Device_Command(Successful, typematics_write, Port_2 => true);

         IF
            NOT Successful
         THEN
            Log("Failed to write PS/2 keyboard typematics on port 2.",
               Tag => PS2_Tag, Warn => true);
            Port_2_Support := false; -- Unreliable port 2.
         ELSE
            Send_Typematics(Successful, Current_Typematics, Port_2 => true);

            IF
               NOT Successful
            THEN
               Log("Failed to send PS/2 keyboard typematics on port 2.",
                  Tag => PS2_Tag, Warn => true);
               Port_2_Support := false; -- Unreliable port 2.
            END IF;
         END IF;
      END IF;

      -- Use the default scancode set (set 2).
      Send_Device_Command(Successful, scancode_set_options); -- Port 1 first.

      IF
         NOT Successful
      THEN
         Log("Couldn't write the PS/2 keyboard's scancode set on port 1.",
            Tag => PS2_Tag, Critical => true);
         Current_Condition := unreliable;
         RETURN;
      ELSE
         Send_Scancode_Set(Successful, Current_Scancode_Set);

         IF
            NOT Successful
         THEN
            Log("Couldn't send the PS/2 keyboard's scancode set on port 1.",
               Tag => PS2_Tag, Critical => true);
            Current_Condition := unreliable;
            RETURN;
         END IF;
      END IF;

      IF -- Now do port 2's scancode set if there's a keyboard on it.
         Port_2_Device = standard_keyboard AND THEN
         Port_2_Support
      THEN
         Send_Device_Command(Successful, scancode_set_options, Port_2 => true);

         IF
            NOT Successful
         THEN
            Log("Couldn't write the PS/2 keyboard's scancode set on port 2.",
               Tag => PS2_Tag, Warn => true);
            Port_2_Support := false;
         ELSE
            Send_Scancode_Set(Successful, Current_Scancode_Set,
               Port_2 => true);

            IF
               NOT Successful
            THEN
               Log("Couldn't send the PS/2 keyboard's scancode set on port 2.",
                  Tag => PS2_Tag, Warn => true);
               Port_2_Support := false;
            END IF;
         END IF;
      END IF;

      -- Enable interrupts from the first PS2 port.
      Send_Controller_Command(Successful, port_1_enable);

      IF
         NOT Successful
      THEN
         Log("Couldn't enable PS/2 port 1 interrupts.", Tag => PS2_Tag,
            Critical => true);
         Current_Condition := unreliable;
         RETURN;
      END IF;

      -- Enable interrupts from the second PS2 device port if it exists, but it
      -- is not vital.
      IF
         Port_2_Support
      THEN
         Send_Controller_Command(Successful, port_2_enable);

         IF
            NOT Successful
         THEN
            Log("Couldn't enable PS/2 port 2 interrupts.", Tag => PS2_Tag,
               Warn => true);
            Port_2_Support := false; -- Unreliable port 2.
         END IF;
      END IF;

      -- Now re-enable scanning/reporting.
      Send_Device_Command(Successful, scanning_enable);

      IF
         NOT Successful
      THEN
         Log("Failed to enable PS/2 port 1 data.", Tag => PS2_Tag,
            Critical => true);
         Current_Condition := unreliable;
         RETURN;
      ELSIF
         Port_2_Support
      THEN
         Send_Device_Command(Successful, reporting_enable, Port_2 => true);

         IF
            NOT Successful
         THEN
            Log("Failed to enable PS/2 port 2 data.", Tag => PS2_Tag,
               Warn => true);
            Port_2_Support := false; -- Unreliable port 2.
         END IF;
      END IF;

      -- Flush the output buffer again just in case.
      Flush;

      -- Set the controller's condition as functional as we have hopefully
      -- got to this line without any raised errors.
      Current_Condition := functional;
   END Setup;
END HAVK_PS2;
