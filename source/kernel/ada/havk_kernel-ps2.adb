-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
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
   PROCEDURE Ready
     (Is_Ready : OUT boolean;
      Sending  : IN boolean := false)
   IS
      Current_Status_Byte : ALIASED CONSTANT number :=
         Input_Byte(Unchecked_Conversion(command_port));
      Current_Status      : CONSTANT status
      WITH
         Import  => true,
         Size    => 8,
         Address => Current_Status_Byte'address;
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
      Is_Ready      : boolean;
      Send_Response : response;
      Data_Byte     : CONSTANT number RANGE 0 .. 2**8 - 1
      WITH
         Import  => true,
         Size    => 8,
         Address => Data'address;
   BEGIN
      FOR
         R IN 1 .. Retry_Rate
      LOOP
         Ready(Is_Ready, Sending => true);

         IF
            Is_Ready
         THEN
            FOR
               I IN 1 .. Retry_Rate
            LOOP
               IF
                  Port_2
               THEN
                  -- TODO: No "enum_rep", 0xD4 is "port_2_data".
                  Output_Byte(Unchecked_Conversion(command_port), 16#D4#);
               END IF;

               Output_Byte(Unchecked_Conversion(Port_Type), Data_Byte);

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
      Is_Ready     : boolean;
      Port_Address : CONSTANT number RANGE 0 .. 2**8 - 1
      WITH
         Import  => true,
         Size    => 8,
         Address => Port_Type'address;

      Raw_Response         : ALIASED number RANGE 0 .. 2**8 - 1;
      Unvalidated_Response : response
      WITH
         Import  => true,
         Size    => 8,
         Address => Raw_Response'address; -- Aliased.
   BEGIN
      FOR
         R IN 1 .. Retry_Rate
      LOOP
         Ready(Is_Ready, Sending => false);

         IF
            Is_Ready
         THEN
            Raw_Response := Input_Byte(Port_Address);
            PRAGMA Warnings(GNATprove, off,
               "attribute Valid is assumed to return True",
               Reason => "Failure is indicated on an invalid response.");
            Message := (IF Unvalidated_Response'valid
               THEN Unvalidated_Response ELSE failure);
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
         Discard := Input_Byte(Unchecked_Conversion(data_port));
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
      Identity          : ARRAY(number RANGE 1 .. 2)
         OF ALIASED number := (number'last, number'last);
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
         Identity(1) := Input_Byte(Unchecked_Conversion(data_port));
      END LOOP;

      Identity(2) := Input_Byte(Unchecked_Conversion(data_port));
      Flush;

      DECLARE -- Convert the "byte" (actually a 64-bit value) to a device byte.
         Unchecked_Identity : device
         WITH
            Import  => true,
            Size    => number'size,
            Address => Identity(1)'address;
      BEGIN
         PRAGMA Warnings(GNATprove, off,
            "attribute Valid is assumed to return True",
            Reason => "If invalid, the variable won't be read.");
         IF
            NOT Unchecked_Identity'valid
         THEN
            Log("PS/2 port " & Port_Image & " has an unrecognised device -" &
               number'image(Identity(1)) & number'image(Identity(2)) & '.',
               Tag => PS2_Tag, Warn => true);
            New_Device := unrecognised;
            RETURN;
         ELSE
            Identified_Device := Unchecked_Identity;
         END IF;
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
            Log("PS/2 port " & Port_Image & " has an unrecognised device -" &
               number'image(Identity(1)) & number'image(Identity(2)) & '.',
               Tag => PS2_Tag, Warn => true);
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
   WITH
      -- TODO: `gnatprove` crashes here with the below error:
      -- "This expression has type bool, but is expected to have type int"
      -- Re-enable it later. Nothing should go wrong here outside of hardware.
      SPARK_Mode => off
   IS
      -- Make read-only copies of the variable so a warning doesn't appear.
      Configuration_Settings     : ALIASED CONSTANT configuration :=
         Current_Configuration;
      Typematics_Settings        : ALIASED CONSTANT    typematics :=
         Current_Typematics;

      Old_Configuration_Byte     :          number RANGE 0 .. 2**8 - 1;
      Current_Configuration_Byte : CONSTANT number RANGE 0 .. 2**8 - 1
      WITH
         Import  => true,
         Size    => 8,
         Address => Configuration_Settings'address;

      Current_Typematics_Byte    : CONSTANT number RANGE 0 .. 2**8 - 1
      WITH
         Import  => true,
         Size    => 8,
         Address => Typematics_Settings'address;

      Successful : boolean;
      Message    : response;
   BEGIN
      -- First, stop any interrupts that'll ruin our expected I/O
      -- communication. An alternative would be to disable interrupts in the
      -- configuration, but this is quicker and more reliable.
      Disable_Interrupts;

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
         Old_Configuration_Byte := Input_Byte(Unchecked_Conversion(data_port));
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
      ELSIF
         Bit_Test(Old_Configuration_Byte, 5) -- Check for "Port_2_Clock".
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
      Identify_Device(Port_1_Device, Port_2 => false);

      IF -- Check port 2 if we have support for it.
         Port_2_Support
      THEN
         Identify_Device(Port_2_Device, Port_2 => true);
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

      -- Finally, re-enable interrupts.
      Enable_Interrupts;
   END Setup;
END HAVK_Kernel.PS2;
