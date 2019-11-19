PACKAGE BODY HAVK_Kernel.PS2.Mouse
IS
   PROCEDURE Interrupt_Manager
   IS
   BEGIN
      Input_Controller.Flush;
   END Interrupt_Manager;
END HAVK_Kernel.PS2.Mouse;
