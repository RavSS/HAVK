-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-debug.adb                                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Debug
WITH
   Refined_State => (Debugging_State => Debugger)
IS
   PROCEDURE Activate
   WITH
      Refined_Global => (In_Out => Serial.UART_State,
                         Input  => Debugger,
                         Output => Intrinsics.CPU_Port_State)
   IS
   BEGIN
      Serial.Prepare_Connection(Debugger);
   END Activate;

   PROCEDURE Message
     (Information : IN string)
   WITH
      Refined_Global => (In_Out => (Intrinsics.CPU_Port_State,
                                    Serial.UART_State),
                         Input  => Debugger)
   IS
      -- Strings for indicating to the receiver what they're getting and
      -- where it ends. Useful for regular expressions on the receiver's side.
      Subject : CONSTANT string := "LOG: ";
      Prefix  : CONSTANT string := "<< ";
      Suffix  : CONSTANT string := " >>";
   BEGIN
      Serial.Print(Debugger, Subject & Prefix & Information & Suffix &
         Debugger.Line_Ender);
   END Message;
END HAVK_Kernel.Debug;
