PACKAGE HAVK_Kernel.PS2
IS
   TYPE controller_condition IS(
      unknown,
      unemulated,
      unreliable,
      functional);
   FOR controller_condition USE(
      unknown    => 0,
      unemulated => 1,
      unreliable => 2,
      functional => 3);

   SUBTYPE scancode_set IS num RANGE 1 .. 3;

   Controller_State      : controller_condition := unknown;
   Current_Scancode_Set  :         scancode_set := 2; -- IBM PC AT (default).
   Current_Shift_State   :              boolean := false;

   -- Constants for abstracting PS/2 controller operations.
   IO_Command_Port       : CONSTANT num := 16#64#;
   IO_Data_Port          : CONSTANT num := 16#60#;

   Data_Acknowledged     : CONSTANT num := 16#FA#;
   Data_Resend           : CONSTANT num := 16#FE#;

   Test_Begin            : CONSTANT num := 16#AA#;
   Test_Pass             : CONSTANT num := 16#55#;
   Test_Fail             : CONSTANT num := 16#FC#;

   Configuration_Write   : CONSTANT num := 16#60#;
   Configuration_Read    : CONSTANT num := 16#20#;

   Typematics_Write      : CONSTANT num := 16#F3#;

   Disable_Port_1        : CONSTANT num := 16#AD#;
   Disable_Port_2        : CONSTANT num := 16#A7#;

   Enable_Port_1         : CONSTANT num := 16#AE#;
   Enable_Port_2         : CONSTANT num := 16#A8#;

   Scancode_Set_Options  : CONSTANT num := 16#F0#;

   TYPE PS2_configuration  IS RECORD
      Port_1_Enabled     : boolean; -- Enables port 1.
      Port_2_Enabled     : boolean; -- Enables port 2 if present.
      System_POST_Pass   : boolean; -- If the system passed POST (always true).
      Zeroed_1           : num RANGE 0 .. 0; -- Always zero.
      Port_1_Clock       : boolean; -- Enables port 1's clock signal.
      Port_2_Clock       : boolean; -- Enables port 2's clock signal.
      Port_1_Translation : boolean; -- Translate AT to XT for the keyboard.
      Zeroed_2           : num RANGE 0 .. 0; -- Always zero.
   END RECORD;
   FOR  PS2_configuration USE RECORD
      Port_1_Enabled      AT 0 RANGE 0 .. 0;
      Port_2_Enabled      AT 0 RANGE 1 .. 1;
      System_POST_Pass    AT 0 RANGE 2 .. 2;
      Zeroed_1            AT 0 RANGE 3 .. 3;
      Port_1_Clock        AT 0 RANGE 4 .. 4;
      Port_2_Clock        AT 0 RANGE 5 .. 5;
      Port_1_Translation  AT 0 RANGE 6 .. 6;
      Zeroed_2            AT 0 RANGE 7 .. 7;
   END RECORD;

   TYPE PS2_status         IS RECORD
      Output_Ready       : boolean; -- Output buffer is full.
      Input_Full         : boolean; -- Controller is working on input.
      System_POST_Pass   : boolean; -- The system passed POST successfully.
      Command_Data       : boolean; -- Input data isn't keyboard configuration.
      Inhibited          : boolean; -- Always true if the keyboard sends data.
      Timed_Out_Transmit : boolean; -- Time out transmission error.
      Timed_Out_Receive  : boolean; -- Time out receiving data error.
      Parity_Error       : boolean; -- The keyboard had even parity.
   END RECORD;
   FOR  PS2_status        USE RECORD
      Output_Ready        AT 0 RANGE 0 .. 0;
      Input_Full          AT 0 RANGE 1 .. 1;
      System_POST_Pass    AT 0 RANGE 2 .. 2;
      Command_Data        AT 0 RANGE 3 .. 3;
      Inhibited           AT 0 RANGE 4 .. 4;
      Timed_Out_Transmit  AT 0 RANGE 5 .. 5;
      Timed_Out_Receive   AT 0 RANGE 6 .. 6;
      Parity_Error        AT 0 RANGE 7 .. 7;
   END RECORD;

   TYPE PS2_typematics     IS RECORD
      -- Controls the repeat rate for the key resends.
      Repeat_Rate        : num RANGE 0 .. 16#1F#;
      -- Key resend delays : 0 = 250 ms, 1 = 500 ms, 2 = 750 ms, 3 = 1000 ms.
      Delay_Rate         : num RANGE 0 .. 3;
      -- Always zeroed out.
      Zeroed             : num RANGE 0 .. 0;
   END RECORD;
   FOR  PS2_typematics    USE RECORD
      Repeat_Rate         AT 0 RANGE 0 .. 4;
      Delay_Rate          AT 0 RANGE 5 .. 6;
      Zeroed              AT 0 RANGE 7 .. 7;
   END RECORD;

   FUNCTION Configuration_To_Byte(
      Configuration : IN PS2_configuration)
   RETURN num
   WITH
      Inline_Always => true;

   FUNCTION Byte_To_Status(
      Status_Byte   : IN num)
   RETURN PS2_status
   WITH
      Inline_Always => true;

   PROCEDURE Controller_Flush
   WITH
      Inline_Always => true;

   PROCEDURE Controller_Initialise;

   PROCEDURE Keyboard_Interrupt_Manager
   WITH
      Inline_Always => true;

   PROCEDURE Scancode_Set_2(
      Scancode     : IN num;
      Shifted      : IN boolean;
      Break        : IN boolean)
   WITH
      Inline_Always => true;
END HAVK_Kernel.PS2;
