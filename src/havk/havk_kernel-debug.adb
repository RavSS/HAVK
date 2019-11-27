WITH
   System.Machine_Code;
USE
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel.Debug IS
   PROCEDURE Initialise
   IS
   BEGIN
      Debugger.Interface_Initialise;
   END Initialise;

   PROCEDURE Message(
      Information : IN string)
   IS
      -- Strings for indicating to the receiver what they're getting and
      -- where it ends. Useful for regular expressions on the receiver's side.
      Starter : CONSTANT string := "DEBUG MESSAGE: << ";
      Ender   : CONSTANT string := " >>";
   BEGIN
      Debugger.Print(Starter & Information & Ender & Debugger.Line_Ender);
   END Message;

   PROCEDURE Breakpoint
   IS
   BEGIN
      Asm(
         "XCHG BX, BX;" & -- For the Bochs magic breakpoint.
         "INT 3;",        -- Interrupt 3 is the breakpoint trap.
         Volatile => true);
   END Breakpoint;
END HAVK_Kernel.Debug;