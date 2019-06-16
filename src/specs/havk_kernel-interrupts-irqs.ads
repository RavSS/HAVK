PACKAGE HAVK_Kernel.Interrupts.IRQs
IS
   PROCEDURE ISR_32_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_32_Handler, "interrupt");

   PROCEDURE ISR_33_Handler( -- Keyboard.
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_33_Handler, "interrupt");

   PROCEDURE ISR_34_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_34_Handler, "interrupt");

   PROCEDURE ISR_35_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_35_Handler, "interrupt");

   PROCEDURE ISR_36_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_36_Handler, "interrupt");

   PROCEDURE ISR_37_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_37_Handler, "interrupt");

   PROCEDURE ISR_38_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_38_Handler, "interrupt");

   PROCEDURE ISR_39_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_39_Handler, "interrupt");

   PROCEDURE ISR_40_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_40_Handler, "interrupt");

   PROCEDURE ISR_41_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_41_Handler, "interrupt");

   PROCEDURE ISR_42_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_42_Handler, "interrupt");

   PROCEDURE ISR_43_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_43_Handler, "interrupt");

   PROCEDURE ISR_44_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_44_Handler, "interrupt");

   PROCEDURE ISR_45_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_45_Handler, "interrupt");

   PROCEDURE ISR_46_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_46_Handler, "interrupt");

   PROCEDURE ISR_47_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_47_Handler, "interrupt");
END HAVK_Kernel.Interrupts.IRQs;
