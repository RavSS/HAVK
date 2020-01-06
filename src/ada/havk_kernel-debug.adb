-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-debug.adb                                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Debug
IS
   PROCEDURE Initialise
   IS
   BEGIN
      Debugger.Interface_Initialise;
   END Initialise;

   PROCEDURE Message
     (Information : IN string)
   IS
      -- Strings for indicating to the receiver what they're getting and
      -- where it ends. Useful for regular expressions on the receiver's side.
      Prefix      : CONSTANT string := "LOG: << ";
      Suffix      : CONSTANT string := " >>";
   BEGIN
      Debugger.Print(Prefix & Information & Suffix & Debugger.Line_Ender);
   END Message;
END HAVK_Kernel.Debug;
