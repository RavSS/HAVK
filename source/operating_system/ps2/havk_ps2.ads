-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System PS/2 Driver                      --
-- Filename        -- havk_ps2.ads                                           --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System;
USE
   HAVK_Operating_System;

-- This package handles all aspects of the PS/2 controller with error checking.
-- All specific logic that does not interact specifically with the PS/2
-- controller should go into child packages.
-- READ: https://wiki.osdev.org/%228042%22_PS/2_Controller
-- READ: https://wiki.osdev.org/PS/2_Keyboard
-- READ: https://wiki.osdev.org/PS/2_Mouse
PACKAGE HAVK_PS2
WITH
   Abstract_State =>
   (
      Controller_State
      WITH
         External => (Async_Readers, Async_Writers,
                      Effective_Reads, Effective_Writes)
   )
IS
   -- A type that indicates the condition of the PS/2 controller.
   TYPE controller_condition IS
     (unknown,
      unemulated,
      unreliable,
      functional)
   WITH
      Default_Value => unknown;

   -- Does an initial PS/2 controller setup that uses the package specification
   -- values to configure the controller.
   PROCEDURE Setup;

   -- Returns the controller's current condition.
   FUNCTION Check_Condition
      RETURN controller_condition
   WITH
      Volatile_Function => true,
      Inline            => true;

   -- Returns true if a mouse is enabled.
   FUNCTION Mouse_Exists
      RETURN boolean
   WITH
      Volatile_Function => true,
      Inline            => true;

   -- If the output buffer is full on the controller, then empty it properly
   -- using this procedure.
   PROCEDURE Flush;

PRIVATE
   PS2_Tag : CONSTANT string := "PS/2";

   -- Holds all possible scancode sets for PS/2 keyboards.
   TYPE scancode_set IS
     (set_1,
      set_2,
      set_3)
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR scancode_set USE
     (set_1 => 1,
      set_2 => 2,
      set_3 => 3);

   -- Specifies the I/O address of the two PS/2 I/O ports (not device ports).
   TYPE port IS
     (data_port,
      command_port)
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR port USE
     (data_port    => 16#60#,
      command_port => 16#64#);

   -- There's five common types of devices with these specific identities.
   -- There are various keyboards with negligible differences not worth it.
   -- Note that a device identity can be up to two bytes. If the first byte
   -- belongs to the keyboard enumeration, then you can safely ignore the
   -- second one, as it often describes a multifunctional (MF2) keyboard
   -- variant with or without set 1 translation, which also means it is bound
   -- to port 1. The "unrecognised" device (and it's representation) is just a
   -- placeholder for a null device.
   TYPE device IS
     (standard_mouse,
      mouse_with_scroll,
      mouse_with_5_buttons,
      standard_keyboard,
      unrecognised)
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR device USE
     (standard_mouse       => 16#00#,
      mouse_with_scroll    => 16#03#,
      mouse_with_5_buttons => 16#04#,
      standard_keyboard    => 16#AB#,
      unrecognised         => 16#FC#);

   -- Describes the most common answers of `Input_Byte()` on the PS/2
   -- controller and device ports. The ones below do not cover them all.
   TYPE response IS
     (test_port_pass,
      test_controller_pass,
      data_acknowledged,
      failure,
      data_resend)
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR response USE
     (test_port_pass       => 16#00#,
      test_controller_pass => 16#55#,
      data_acknowledged    => 16#FA#,
      failure              => 16#FC#,
      data_resend          => 16#FE#);

   -- Constant values for standard PS/2 keyboard commands that must be sent
   -- to the keyboard's PS/2 data IO port. There are a few keyboard commands
   -- missing as they are specific to scancode set 3 and are not really useful.
   TYPE keyboard_command IS
     (set_lights,
      echo_keyboard,
      scancode_set_options,
      keyboard_identity,
      typematics_write,
      scanning_enable,
      scanning_disable,
      keyboard_defaults,
      keyboard_byte_resend,
      test_keyboard)
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR keyboard_command USE
     (set_lights           => 16#ED#,
      echo_keyboard        => 16#EE#,
      scancode_set_options => 16#F0#,
      keyboard_identity    => 16#F2#,
      typematics_write     => 16#F3#,
      scanning_enable      => 16#F4#,
      scanning_disable     => 16#F5#,
      keyboard_defaults    => 16#F6#,
      keyboard_byte_resend => 16#FE#,
      test_keyboard        => 16#FF#);

   -- Like with the keyboard command type, but for PS/2 mice commands.
   TYPE mouse_command IS
     (set_scaling,
      set_resolution,
      mouse_status,
      stream_mode,
      read_data,
      wrap_mode_reset,
      wrap_mode_set,
      remote_mode,
      mouse_identity,
      set_sample_rate,
      reporting_enable,
      reporting_disable,
      mouse_defaults,
      mouse_byte_resend,
      mouse_reset)
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR mouse_command USE
     (set_scaling       => 16#E6#,
      set_resolution    => 16#E8#,
      mouse_status      => 16#E9#,
      stream_mode       => 16#EA#,
      read_data         => 16#EB#,
      wrap_mode_reset   => 16#EC#,
      wrap_mode_set     => 16#EE#,
      remote_mode       => 16#F0#,
      mouse_identity    => 16#F2#,
      set_sample_rate   => 16#F3#,
      reporting_enable  => 16#F4#,
      reporting_disable => 16#F5#,
      mouse_defaults    => 16#F6#,
      mouse_byte_resend => 16#FE#,
      mouse_reset       => 16#FF#);

   -- Enumeration values for PS/2 controller operations.
   TYPE controller_command IS
     (configuration_read,
      configuration_write,
      port_2_disable,
      port_2_enable,
      test_port_2_begin,
      test_controller_begin,
      test_port_1_begin,
      port_1_disable,
      port_1_enable,
      port_2_data)
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR controller_command USE
     (configuration_read    => 16#20#,
      configuration_write   => 16#60#,
      port_2_disable        => 16#A7#,
      port_2_enable         => 16#A8#,
      test_port_2_begin     => 16#A9#,
      test_controller_begin => 16#AA#,
      test_port_1_begin     => 16#AB#,
      port_1_disable        => 16#AD#,
      port_1_enable         => 16#AE#,
      port_2_data           => 16#D4#);

   -- Outlines the configuration byte for the PS/2 controller itself.
   TYPE configuration IS RECORD
      -- Enables port 1.
      Port_1_Enabled     : boolean;
      -- Enables port 2 if present.
      Port_2_Enabled     : boolean;
      -- Technically always true.
      System_POST_Pass   : boolean;
      -- Always zero.
      Zeroed_1           : number RANGE 0 .. 0;
      -- Enables port 1's clock signal.
      Port_1_Clock       : boolean;
      -- Enables port 2's clock signal.
      Port_2_Clock       : boolean;
      -- Translate AT to XT for port 1.
      Port_1_Translation : boolean;
      -- Always zero.
      Zeroed_2           : number RANGE 0 .. 0;
   END RECORD
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR configuration USE RECORD
      Port_1_Enabled         AT 0 RANGE 0 .. 0;
      Port_2_Enabled         AT 0 RANGE 1 .. 1;
      System_POST_Pass       AT 0 RANGE 2 .. 2;
      Zeroed_1               AT 0 RANGE 3 .. 3;
      Port_1_Clock           AT 0 RANGE 4 .. 4;
      Port_2_Clock           AT 0 RANGE 5 .. 5;
      Port_1_Translation     AT 0 RANGE 6 .. 6;
      Zeroed_2               AT 0 RANGE 7 .. 7;
   END RECORD;

   -- A record describing the fields of the status register, which can
   -- be read by reading a byte with `Input_Byte()` from the PS/2 data IO port.
   TYPE status IS RECORD
      -- Output buffer is full.
      Output_Ready       : boolean;
      -- Controller is working on input.
      Input_Full         : boolean;
      -- The system passed POST successfully.
      System_POST_Pass   : boolean;
      -- Input data isn't keyboard configuration.
      Command_Data       : boolean;
      -- Always true if the keyboard sends data.
      Inhibited          : boolean;
      -- Time out transmission error.
      Timed_Out_Transmit : boolean;
      -- Time out receiving data error.
      Timed_Out_Receive  : boolean;
      -- The keyboard had even parity.
      Parity_Error       : boolean;
   END RECORD
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR status USE RECORD
      Output_Ready       AT 0 RANGE 0 .. 0;
      Input_Full         AT 0 RANGE 1 .. 1;
      System_POST_Pass   AT 0 RANGE 2 .. 2;
      Command_Data       AT 0 RANGE 3 .. 3;
      Inhibited          AT 0 RANGE 4 .. 4;
      Timed_Out_Transmit AT 0 RANGE 5 .. 5;
      Timed_Out_Receive  AT 0 RANGE 6 .. 6;
      Parity_Error       AT 0 RANGE 7 .. 7;
   END RECORD;

   -- A byte layout for configuring the type settings of PS/2 keyboards.
   -- Bochs for example completely ignores this.
   TYPE typematics IS RECORD
      -- Controls the repeat rate for the key resends.
      Repeat_Rate        : number RANGE 0 .. 16#1F#;
      -- Key resend delays: 0 = 250 ms, 1 = 500 ms, 2 = 750 ms, 3 = 1000 ms.
      Delay_Rate         : number RANGE 0 .. 000003;
      -- Always zeroed out.
      Zeroed             : number RANGE 0 .. 000000;
   END RECORD
   WITH
      Size        => 8,
      Object_Size => number'size;
   FOR typematics USE RECORD
      Repeat_Rate            AT 0 RANGE 0 .. 4;
      Delay_Rate             AT 0 RANGE 5 .. 6;
      Zeroed                 AT 0 RANGE 7 .. 7;
   END RECORD;

   -- The main procedure for interacting with the controller and its devices.
   GENERIC
      TYPE generic_data IS PRIVATE;
      Port_Type : IN port;
      Verify    : IN boolean;
   PROCEDURE Send
     (Success   : OUT boolean;
      Data      : IN generic_data;
      Port_2    : IN boolean := false);

   -- Receives a response from either one of the two ports and returns it in
   -- the first parameter.
   PROCEDURE Receive
     (Message   : OUT response;
      Port_Type : IN port);

   -- Checks the status register's output buffer indicator and returns true
   -- in the first parameter if it is full and receiving won't cause an error.
   -- If the second parameter is true, then it checks if the input buffer is
   -- not full (ready to send).
   PROCEDURE Ready
     (Is_Ready : OUT boolean;
      Sending  : IN boolean := false);

   -- Sets the port device fields in the package's port variables so they can
   -- be retrieved later without needing to reidentify them.
   PROCEDURE Identify_Device
     (New_Device : OUT device;
      Port_2     : IN boolean);

   -- Max `Input_Byte()` and `Output_Byte()` attempts. Doesn't truly matter.
   Retry_Rate            : CONSTANT number := 100;

   -- The default set is set 2, as it is the only one implemented as of now
   -- and it is the most common scancode set.
   Current_Scancode_Set  : scancode_set := set_2
   WITH
      Part_Of => Controller_State;

   -- The current condition of the controller is unknown at the start.
   Current_Condition     : controller_condition := unknown
   WITH
      Part_Of => Controller_State;

   -- The default configuration is safe and does not assume there
   -- is a mouse or a port 2 available.
   Current_Configuration : configuration :=
     (Port_1_Enabled     =>   true,
      Port_2_Enabled     =>  false,  -- Disable port 2 for now.
      System_POST_Pass   =>   true,  -- We've obviously passed POST.
      Zeroed_1           => 000000,
      Port_1_Clock       =>   true,
      Port_2_Clock       =>  false,  -- Again, disable port 2 for now.
      Port_1_Translation =>  false,  -- Do not translate all sets to set 1.
      Zeroed_2           => 000000)
   WITH
      Part_Of => Controller_State;

   -- The default typematic settings are slow as possible.
   Current_Typematics    : typematics :=
     (Repeat_Rate => 16#1F#, -- Slowest repeat rate.
      Delay_Rate  => 000003, -- Largest delay between repeats.
      Zeroed      => 000000)
   WITH
      Part_Of => Controller_State;

   -- Indicates whether or not the PS/2 controller is dual-channel capable.
   -- This does not mean there is a mouse available or if the second device
   -- is a mouse at all.
   Port_2_Support        : boolean := false
   WITH
      Part_Of => Controller_State;

   -- Indicates whether or not there is a PS/2 mouse connected to the system
   -- that is completely usable.
   Mouse_Support         : boolean := false
   WITH
      Part_Of => Controller_State;

   -- A keyboard is presumed to be at port 1.
   Port_1_Device         : device  := standard_keyboard
   WITH
      Part_Of => Controller_State;

   -- Port 2 takes no guesses.
   Port_2_Device         : device  := unrecognised
   WITH
      Part_Of => Controller_State;

END HAVK_PS2;
