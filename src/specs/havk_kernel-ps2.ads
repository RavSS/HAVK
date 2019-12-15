-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-ps2.ads                                    --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

-- This package handles all aspects of the PS/2 controller with error checking.
-- All specific logic that does not interact specifically with the PS/2
-- controller should go into child packages.
-- READ: https://wiki.osdev.org/%228042%22_PS/2_Controller
-- READ: https://wiki.osdev.org/PS/2_Keyboard
-- READ: https://wiki.osdev.org/PS/2_Mouse
PACKAGE HAVK_Kernel.PS2
IS
   -- A type that indicates the condition of the PS/2 controller.
   TYPE controller_condition IS(
      unknown,
      unemulated,
      unreliable,
      functional);

   -- Holds all possible scancode sets for PS/2 keyboards.
   SUBTYPE scancode_set IS num RANGE 1 .. 3;

   -- Specifies the IO address of the two PS/2 IO ports (not the device ports).
   SUBTYPE port IS num RANGE 16#60# .. 16#64#;

   -- There's five common types of devices with these specific IDs.
   -- There are various keyboards with negligible differences not worth it.
   -- Note that a device ID can be up to two bytes. If the first byte belongs
   -- to the keyboard enumeration, then you can safely ignore the second one,
   -- as it often describes a multifunctional (MF2) keyboard variant with or
   -- without set 1 translation, which also means it is bound to port 1.
   SUBTYPE device IS num RANGE 16#00# .. 16#FF#;

   -- Constant values for abstracting PS/2 controller operations.
   SUBTYPE controller_command IS num RANGE 16#20# .. 16#D4#;

   -- Constant values for standard PS/2 keyboard commands that must be sent
   -- to the keyboard's PS/2 data IO port. There are a few keyboard commands
   -- missing as they are specific to scancode set 3 and are not really useful.
   SUBTYPE keyboard_command IS num RANGE 16#ED# .. 16#FF#;

   -- Like with the keyboard command type, but for PS/2 mice commands.
   SUBTYPE mouse_command IS num RANGE 16#E6# .. 16#FF#;

   -- Describes the most common answers of `INB()` on the PS/2
   -- controller and device ports. The ones below do not cover them all.
   SUBTYPE response IS num RANGE 16#00# .. 16#FF#;

   Data                  : CONSTANT               port := 16#60#;
   Command               : CONSTANT               port := 16#64#;
   Standard_Mouse        : CONSTANT             device := 16#00#;
   Mouse_With_Scroll     : CONSTANT             device := 16#03#;
   Mouse_With_5_Buttons  : CONSTANT             device := 16#04#;
   Standard_Keyboard     : CONSTANT             device := 16#AB#;
   Unrecognised          : CONSTANT             device := 16#FC#;
   Test_Port_Pass        : CONSTANT           response := 16#00#;
   Test_Controller_Pass  : CONSTANT           response := 16#55#;
   Data_Acknowledged     : CONSTANT           response := 16#FA#;
   Failure               : CONSTANT           response := 16#FC#;
   Data_Resend           : CONSTANT           response := 16#FE#;
   Set_Scaling           : CONSTANT      mouse_command := 16#E6#;
   Set_Resolution        : CONSTANT      mouse_command := 16#E8#;
   Mouse_Status          : CONSTANT      mouse_command := 16#E9#;
   Stream_Mode           : CONSTANT      mouse_command := 16#EA#;
   Read_Data             : CONSTANT      mouse_command := 16#EB#;
   Wrap_Mode_Reset       : CONSTANT      mouse_command := 16#EC#;
   Wrap_Mode_Set         : CONSTANT      mouse_command := 16#EE#;
   Remote_Mode           : CONSTANT      mouse_command := 16#F0#;
   Mouse_Identity        : CONSTANT      mouse_command := 16#F2#;
   Set_Sample_Rate       : CONSTANT      mouse_command := 16#F3#;
   Reporting_Enable      : CONSTANT      mouse_command := 16#F4#;
   Reporting_Disable     : CONSTANT      mouse_command := 16#F5#;
   Mouse_Defaults        : CONSTANT      mouse_command := 16#F6#;
   Mouse_Byte_Resend     : CONSTANT      mouse_command := 16#FE#;
   Mouse_Reset           : CONSTANT      mouse_command := 16#FF#;
   Set_Lights            : CONSTANT   keyboard_command := 16#ED#;
   Echo_Keyboard         : CONSTANT   keyboard_command := 16#EE#;
   Scancode_Set_Options  : CONSTANT   keyboard_command := 16#F0#;
   Keyboard_Identity     : CONSTANT   keyboard_command := 16#F2#;
   Typematics_Write      : CONSTANT   keyboard_command := 16#F3#;
   Scanning_Enable       : CONSTANT   keyboard_command := 16#F4#;
   Scanning_Disable      : CONSTANT   keyboard_command := 16#F5#;
   Keyboard_Defaults     : CONSTANT   keyboard_command := 16#F6#;
   Keyboard_Byte_Resend  : CONSTANT   keyboard_command := 16#FE#;
   Test_Keyboard         : CONSTANT   keyboard_command := 16#FF#;
   Configuration_Read    : CONSTANT controller_command := 16#20#;
   Configuration_Write   : CONSTANT controller_command := 16#60#;
   Port_2_Disable        : CONSTANT controller_command := 16#A7#;
   Port_2_Enable         : CONSTANT controller_command := 16#A8#;
   Test_Port_2_Begin     : CONSTANT controller_command := 16#A9#;
   Test_Controller_Begin : CONSTANT controller_command := 16#AA#;
   Test_Port_1_Begin     : CONSTANT controller_command := 16#AB#;
   Port_1_Disable        : CONSTANT controller_command := 16#AD#;
   Port_1_Enable         : CONSTANT controller_command := 16#AE#;
   Port_2_Data           : CONSTANT controller_command := 16#D4#;

   -- Outlines the configuration byte for the PS/2 controller itself.
   TYPE configuration      IS RECORD
      Port_1_Enabled     : boolean;          -- Enables port 1.
      Port_2_Enabled     : boolean;          -- Enables port 2 if present.
      System_POST_Pass   : boolean;          -- Technically always true.
      Zeroed_1           : num RANGE 0 .. 0; -- Always zero.
      Port_1_Clock       : boolean;          -- Enables port 1's clock signal.
      Port_2_Clock       : boolean;          -- Enables port 2's clock signal.
      Port_1_Translation : boolean;          -- Translate AT to XT for port 1.
      Zeroed_2           : num RANGE 0 .. 0; -- Always zero.
   END RECORD;
   FOR  configuration     USE RECORD
      Port_1_Enabled      AT 0 RANGE 0 .. 0;
      Port_2_Enabled      AT 0 RANGE 1 .. 1;
      System_POST_Pass    AT 0 RANGE 2 .. 2;
      Zeroed_1            AT 0 RANGE 3 .. 3;
      Port_1_Clock        AT 0 RANGE 4 .. 4;
      Port_2_Clock        AT 0 RANGE 5 .. 5;
      Port_1_Translation  AT 0 RANGE 6 .. 6;
      Zeroed_2            AT 0 RANGE 7 .. 7;
   END RECORD;

   -- A record describing the fields of the status register, which can
   -- be read by reading a byte with `INB()` from the PS/2 data IO port.
   TYPE status             IS RECORD
      Output_Ready       : boolean; -- Output buffer is full.
      Input_Full         : boolean; -- Controller is working on input.
      System_POST_Pass   : boolean; -- The system passed POST successfully.
      Command_Data       : boolean; -- Input data isn't keyboard configuration.
      Inhibited          : boolean; -- Always true if the keyboard sends data.
      Timed_Out_Transmit : boolean; -- Time out transmission error.
      Timed_Out_Receive  : boolean; -- Time out receiving data error.
      Parity_Error       : boolean; -- The keyboard had even parity.
   END RECORD;
   FOR  status            USE RECORD
      Output_Ready        AT 0 RANGE 0 .. 0;
      Input_Full          AT 0 RANGE 1 .. 1;
      System_POST_Pass    AT 0 RANGE 2 .. 2;
      Command_Data        AT 0 RANGE 3 .. 3;
      Inhibited           AT 0 RANGE 4 .. 4;
      Timed_Out_Transmit  AT 0 RANGE 5 .. 5;
      Timed_Out_Receive   AT 0 RANGE 6 .. 6;
      Parity_Error        AT 0 RANGE 7 .. 7;
   END RECORD;

   -- A byte layout for configuring the type settings of PS/2 keyboards.
   -- Bochs for example completely ignores this.
   TYPE typematics         IS RECORD
      -- Controls the repeat rate for the key resends.
      Repeat_Rate        : num RANGE 0 .. 16#1F#;
      -- Key resend delays: 0 = 250 ms, 1 = 500 ms, 2 = 750 ms, 3 = 1000 ms.
      Delay_Rate         : num RANGE 0 .. 3;
      -- Always zeroed out.
      Zeroed             : num RANGE 0 .. 0;
   END RECORD;
   FOR  typematics        USE RECORD
      Repeat_Rate         AT 0 RANGE 0 .. 4;
      Delay_Rate          AT 0 RANGE 5 .. 6;
      Zeroed              AT 0 RANGE 7 .. 7;
   END RECORD;

   -- Represents the 8042 PS/2 controller as a single object.
   TYPE controller  IS TAGGED RECORD
      -- Max `INB()` and `OUTB()` attempts. Honestly doesn't matter.
      Retry_Rate            : num RANGE 10 .. 1000 := 100;
      -- The default set is set 2, as it is the only one implemented as of now
      -- and it is the most common scancode set.
      Current_Scancode_Set  : scancode_set         := 2;
      -- The current condition of the controller is unknown at the start.
      Current_Condition     : controller_condition := unknown;
      -- The default configuration is safe and does not assume there
      -- is a mouse or a port 2 available.
      Current_Configuration : configuration :=
      (
         Port_1_Enabled     =>   true,
         Port_2_Enabled     =>  false,  -- Disable port 2 for now.
         System_POST_Pass   =>   true,  -- We've obviously passed POST.
         Zeroed_1           =>      0,
         Port_1_Clock       =>   true,
         Port_2_Clock       =>  false,  -- Again, disable port 2 for now.
         Port_1_Translation =>  false,  -- Do not translate all sets to set 1.
         Zeroed_2           =>      0
      );
      -- The default typematic settings are slow as possible.
      Current_Typematics    : typematics    :=
      (
         Repeat_Rate        => 16#1F#, -- Slowest repeat rate.
         Delay_Rate         =>      3, -- Largest delay between repeats.
         Zeroed             =>      0
      );
      -- Indicates whether or not the PS/2 controller is dual-channel capable.
      -- This does not mean there is a mouse available or if the second device
      -- is a mouse at all.
      Port_2_Support        : boolean := false;
      -- Indicates whether or not there is a PS/2 mouse connected to the system
      -- that is completely usable.
      Mouse_Support         : boolean := false;
      -- A keyboard is presumed to be at port 1. Port 2 takes no guesses.
      Port_1_Device         : device  := Standard_Keyboard;
      Port_2_Device         : device  := Unrecognised;
   END RECORD;

   -- The main 8042 PS/2 controller. The IBM PC system by design only has one.
   -- Declaring new controllers can be used for switching PS/2 controller
   -- configurations, but that is not tested as of now.
   Input_Controller : controller;

   -- Receives a response from either one of the two ports.
   FUNCTION Receive(
      Object    : IN controller'class;
      Port_Type : IN port)
   RETURN response;

   -- If the output buffer is full on the controller, then empty it properly.
   PROCEDURE Flush(
      Object    : IN controller'class);

   -- Returns the controller's current condition.
   FUNCTION Check_Condition
   RETURN controller_condition
   WITH
      Inline => true;

   -- Returns true if a mouse is enabled.
   FUNCTION Mouse_Exists
   RETURN boolean
   WITH
      Inline => true;

   -- Does an initial PS/2 controller setup that uses values from the
   -- tagged record itself to configure the controller.
   PROCEDURE Setup;

PRIVATE
   -- The main function for interacting with the controller and its devices.
   -- Don't call this directly. Call the wrappers instead.
   FUNCTION Send(
      Object    : IN controller'class;
      Port_Type : IN port;
      Byte      : IN num;
      Port_2    : IN boolean;
      Verify    : IN boolean)
   RETURN boolean
   WITH
      Pre => Byte <= 16#FF#;

   -- Issues a controller specific command to the PS/2 controller IO port.
   FUNCTION Send_Controller_Command(
      Object    : IN controller'class;
      Operation : IN controller_command)
   RETURN boolean;

   -- Issues a keyboard specific command to the PS/2 controller's first data
   -- IO port by default, as the keyboard is often on the first port.
   FUNCTION Send_Keyboard_Command(
      Object    : IN controller'class;
      Operation : IN keyboard_command;
      Port_2    : IN boolean := false)
   RETURN boolean;

   -- Issues a mouse specific command to the PS/2 controller's second data
   -- IO port by default, as the mouse is often on the second port.
   FUNCTION Send_Mouse_Command(
      Object    : IN controller'class;
      Operation : IN mouse_command;
      Port_2    : IN boolean := true)
   RETURN boolean;

   -- Sends a byte to the data IO port. By default, this attempts to verify or
   -- error check the response, but not all previously sent commands return
   -- actual responses. An example of that is "configuration_read", which
   -- instead returns an arbitrary byte describing the configuration, so that
   -- would cause an error with this function. Disable "Verify" to avoid it.
   FUNCTION Send_Data(
      Object    : IN controller'class;
      Byte_Data : IN num;
      Port_2    : IN boolean := false;
      Verify    : IN boolean := true)
   RETURN boolean
   WITH
      Pre => Byte_Data <= 16#FF#;

   -- Sets the port device fields in the class object. Returns false on
   -- any errors. Handles port 2 support.
   PROCEDURE Identify_Device(
      Object    : IN OUT controller'class;
      Port_2    : IN boolean);

   -- Sends the PS/2 configuration in the record to the controller.
   FUNCTION Send_Configuration(
      Object    : IN controller'class)
   RETURN boolean;

   -- Sends typematic settings to a PS/2 keyboard on either port.
   FUNCTION Send_Typematics(
      Object    : IN controller'class;
      Port_2    : IN boolean := false)
   RETURN boolean;

   -- Sends a preference for a scancode set to a PS/2 keyboard on either port.
   FUNCTION Send_Scancode_Set(
      Object    : IN controller'class;
      Port_2    : IN boolean := false)
   RETURN boolean;

   -- Checks the status register's output buffer indicator and returns true
   -- if it is full and receiving won't cause an error.
   FUNCTION Ready_To_Receive(
      Object    : IN controller'class)
   RETURN boolean
   WITH
      Inline => true;

   -- Checks the status register's input buffer indicator and returns true
   -- if it is empty.
   FUNCTION Ready_To_Send(
      Object    : IN controller'class)
   RETURN boolean
   WITH
      Inline => true;

END HAVK_Kernel.PS2;
