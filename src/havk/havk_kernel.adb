WITH
   System.Machine_Code;
USE
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel IS
   PROCEDURE Breakpoint
   IS
   BEGIN
      Asm(
         "XCHG BX, BX;" & -- For the Bochs magic breakpoint.
         "INT 3;",        -- Interrupt 3 is the breakpoint trap.
         Volatile => true);
   END Breakpoint;
END HAVK_Kernel;