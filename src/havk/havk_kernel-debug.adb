WITH
   System.Machine_Code;
USE
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel.Debug IS
   PROCEDURE Initialize
   IS
   BEGIN
      Debugger.Interface_Initialize;
   END Initialize;

   PROCEDURE Message(
      Info : IN string)
   IS
      -- Strings for indicating to the receiver what they're getting and
      -- where it ends. Useful for regular expressions on the receiver's side.
      Info_Start : CONSTANT string(1 .. 18) := "DEBUG MESSAGE: << ";
      Info_Ender : CONSTANT string(1 ..  3) := " >>";
   BEGIN
      Debugger.Print(Info_Start);
      Debugger.Print(Info);
      Debugger.Print(Info_Ender);
      Debugger.Print(Debugger.Line_Ender);
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