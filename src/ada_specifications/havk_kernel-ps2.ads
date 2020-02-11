-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-ps2.ads                                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package handles all aspects of the PS/2 controller with error checking.
-- All specific logic that does not interact specifically with the PS/2
-- controller should go into child packages.
-- READ: https://wiki.osdev.org/%228042%22_PS/2_Controller
-- READ: https://wiki.osdev.org/PS/2_Keyboard
-- READ: https://wiki.osdev.org/PS/2_Mouse
PACKAGE HAVK_Kernel.PS2
IS
   PRAGMA Preelaborate;

   -- A type that indicates the condition of the PS/2 controller.
   TYPE controller_condition IS
     (unknown,
      unemulated,
      unreliable,
      functional);

   -- Holds all possible scancode sets for PS/2 keyboards.
   SUBTYPE scancode_set       IS number RANGE 16#01# .. 16#03#;
   -- Specifies the IO address of the two PS/2 IO ports (not the device ports).
   SUBTYPE port               IS number RANGE 16#60# .. 16#64#;
   -- There's five common types of devices with these specific IDs.
   -- There are various keyboards with negligible differences not worth it.
   -- Note that a device ID can be up to two bytes. If the first byte belongs
   -- to the keyboard enumeration, then you can safely ignore the second one,
   -- as it often describes a multifunctional (MF2) keyboard variant with or
   -- without set 1 translation, which also means it is bound to port 1.
   SUBTYPE device             IS number RANGE 16#00# .. 16#FF#;
   -- Constant values for abstracting PS/2 controller operations.
   SUBTYPE controller_command IS number RANGE 16#20# .. 16#D4#;
   -- Constant values for standard PS/2 keyboard commands that must be sent
   -- to the keyboard's PS/2 data IO port. There are a few keyboard commands
   -- missing as they are specific to scancode set 3 and are not really useful.
   SUBTYPE keyboard_command   IS number RANGE 16#ED# .. 16#FF#;
   -- Like with the keyboard command type, but for PS/2 mice commands.
   SUBTYPE mouse_command      IS number RANGE 16#E6# .. 16#FF#;
   -- Describes the most common answers of `Input_Byte()` on the PS/2
   -- controller and device ports. The ones below do not cover them all.
   SUBTYPE response           IS number RANGE 16#00# .. 16#FF#;

   -- TODO: Switch these to enumeration values when GNAT CE supplies a version
   -- of `gnatprove` that supports enumeration representations, which should
   -- be GNAT CE 2020. I cannot be bothered using unchecked conversion for it.
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
   END RECORD;
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
   END RECORD;
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
   END RECORD;
   FOR typematics USE RECORD
      Repeat_Rate            AT 0 RANGE 0 .. 4;
      Delay_Rate             AT 0 RANGE 5 .. 6;
      Zeroed                 AT 0 RANGE 7 .. 7;
   END RECORD;

   -- Does an initial PS/2 controller setup that uses values from the
   -- tagged record itself to configure the controller.
   PROCEDURE Setup;

   -- Receives a response from either one of the two ports.
   FUNCTION Receive
     (Port_Type : IN port)
      RETURN response;

   -- If the output buffer is full on the controller, then empty it properly.
   PROCEDURE Flush;

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

   -- The main function for interacting with the controller and its devices.
   -- Don't call this directly. Call the wrappers instead.
   FUNCTION Send
     (Port_Type : IN port;
      Byte      : IN number;
      Port_2    : IN boolean;
      Verify    : IN boolean)
      RETURN boolean
   WITH
      Pre => Byte <= 16#FF#;

   -- Issues a controller specific command to the PS/2 controller I/O port.
   FUNCTION Send_Controller_Command
     (Operation : IN controller_command)
      RETURN boolean;

   -- Issues a keyboard specific command to the PS/2 controller's first data
   -- I/O port by default, as the keyboard is often on the first port.
   FUNCTION Send_Keyboard_Command
     (Operation : IN keyboard_command;
      Port_2    : IN boolean := false)
      RETURN boolean;

   -- Issues a mouse specific command to the PS/2 controller's second data
   -- I/O port by default, as the mouse is often on the second port.
   FUNCTION Send_Mouse_Command
     (Operation : IN mouse_command;
      Port_2    : IN boolean := true)
      RETURN boolean;

   -- Sends a byte to the data I/O port. By default, this attempts to verify or
   -- error check the response, but not all previously sent commands return
   -- actual responses. An example of that is "configuration_read", which
   -- instead returns an arbitrary byte describing the configuration, so that
   -- would cause an error with this function. Disable "Verify" to avoid it.
   FUNCTION Send_Data
     (Byte_Data : IN number;
      Port_2    : IN boolean := false;
      Verify    : IN boolean := true)
      RETURN boolean
   WITH
      Pre => Byte_Data <= 16#FF#;

   -- Sets the port device fields in the class object. Returns false on
   -- any errors. Handles port 2 support.
   PROCEDURE Identify_Device
     (Port_2    : IN boolean);

   -- Sends the PS/2 configuration in the record to the controller.
   FUNCTION Send_Configuration
      RETURN boolean;

   -- Sends typematic settings to a PS/2 keyboard on either port.
   FUNCTION Send_Typematics
     (Port_2    : IN boolean := false)
      RETURN boolean;

   -- Sends a preference for a scancode set to a PS/2 keyboard on either port.
   FUNCTION Send_Scancode_Set
     (Port_2    : IN boolean := false)
      RETURN boolean;

   -- Checks the status register's output buffer indicator and returns true
   -- if it is full and receiving won't cause an error.
   FUNCTION Ready_To_Receive
      RETURN boolean;

   -- Checks the status register's input buffer indicator and returns true
   -- if it is empty.
   FUNCTION Ready_To_Send
      RETURN boolean;

   -- Max `Input_Byte()` and `Output_Byte()` attempts. Doesn't truly matter.
   Retry_Rate            : number RANGE 10 .. 1000 := 100;

   -- The default set is set 2, as it is the only one implemented as of now
   -- and it is the most common scancode set.
   Current_Scancode_Set  : scancode_set := 002;

   -- The current condition of the controller is unknown at the start.
   Current_Condition     : controller_condition := unknown;

   -- The default configuration is safe and does not assume there
   -- is a mouse or a port 2 available.
   Current_Configuration : configuration :=
   (
      Port_1_Enabled     =>   true,
      Port_2_Enabled     =>  false,  -- Disable port 2 for now.
      System_POST_Pass   =>   true,  -- We've obviously passed POST.
      Zeroed_1           => 000000,
      Port_1_Clock       =>   true,
      Port_2_Clock       =>  false,  -- Again, disable port 2 for now.
      Port_1_Translation =>  false,  -- Do not translate all sets to set 1.
      Zeroed_2           => 000000
   );

   -- The default typematic settings are slow as possible.
   Current_Typematics    : typematics :=
   (
      Repeat_Rate        => 16#1F#, -- Slowest repeat rate.
      Delay_Rate         => 000003, -- Largest delay between repeats.
      Zeroed             => 000000
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

END HAVK_Kernel.PS2;
