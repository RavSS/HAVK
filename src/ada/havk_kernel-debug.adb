-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-debug.adb                                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Debug
WITH
   SPARK_Mode    => off, -- See the private specification of this package.
   Refined_State => (Debugging_State => (Debugger, Terminal_Hook,
                                         Terminal_Printing))
IS
   PROCEDURE Initialise
     (Terminal : IN OUT Graphics.Text.textbox;
      Printing : IN boolean)
   WITH
      Refined_Global => (Input => (Debugger, Terminal_Hook, Terminal_Printing))
   IS
   BEGIN
      Debugger.Interface_Initialise;

      -- This is unchecked access should be safe as long as the terminal
      -- textbox still exists, which it will until the system turns off, as it
      -- is a static object and is placed into the higher-half kernel space.
      Terminal_Hook     := Terminal'unchecked_access;
      Terminal_Printing := Printing;
   END Initialise;

   PROCEDURE Message
     (Information : IN string)
   WITH
      Refined_Global => (Input => (Debugger, Terminal_Hook, Terminal_Printing))
   IS
      -- Strings for indicating to the receiver what they're getting and
      -- where it ends. Useful for regular expressions on the receiver's side.
      Subject     : CONSTANT string := "LOG: ";
      Prefix      : CONSTANT string := "<< ";
      Suffix      : CONSTANT string := " >>";
   BEGIN
      Debugger.Print(Subject & Prefix & Information & Suffix &
         Debugger.Line_Ender);

      IF
         Terminal_Printing AND THEN Terminal_Hook /= NULL
      THEN
         Terminal_Hook.Print(Prefix & Information & Suffix);
      END IF;
   END Message;
END HAVK_Kernel.Debug;
