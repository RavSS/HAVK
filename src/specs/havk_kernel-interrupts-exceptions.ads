PACKAGE HAVK_Kernel.Interrupts.Exceptions
IS
   PROCEDURE ISR_0_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_0_Handler,  "interrupt");

   PROCEDURE ISR_1_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_1_Handler,  "interrupt");

   PROCEDURE ISR_2_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_2_Handler,  "interrupt");

   PROCEDURE ISR_3_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_3_Handler,  "interrupt");

   PROCEDURE ISR_4_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_4_Handler,  "interrupt");

   PROCEDURE ISR_5_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_5_Handler,  "interrupt");

   PROCEDURE ISR_6_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_6_Handler,  "interrupt");

   PROCEDURE ISR_7_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_7_Handler,  "interrupt");

   PROCEDURE ISR_8_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_8_Handler,  "interrupt");

   PROCEDURE ISR_9_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_9_Handler,  "interrupt");

   PROCEDURE ISR_10_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_10_Handler, "interrupt");

   PROCEDURE ISR_11_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_11_Handler, "interrupt");

   PROCEDURE ISR_12_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_12_Handler, "interrupt");

   PROCEDURE ISR_13_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_13_Handler, "interrupt");

   PROCEDURE ISR_14_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_14_Handler, "interrupt");

   PROCEDURE ISR_15_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_15_Handler, "interrupt");

   PROCEDURE ISR_16_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_16_Handler, "interrupt");

   PROCEDURE ISR_17_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_17_Handler, "interrupt");

   PROCEDURE ISR_18_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_18_Handler, "interrupt");

   PROCEDURE ISR_19_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_19_Handler, "interrupt");

   PROCEDURE ISR_20_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_20_Handler, "interrupt");

   PROCEDURE ISR_21_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_21_Handler, "interrupt");

   PROCEDURE ISR_22_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_22_Handler, "interrupt");

   PROCEDURE ISR_23_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_23_Handler, "interrupt");

   PROCEDURE ISR_24_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_24_Handler, "interrupt");

   PROCEDURE ISR_25_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_25_Handler, "interrupt");

   PROCEDURE ISR_26_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_26_Handler, "interrupt");

   PROCEDURE ISR_27_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_27_Handler, "interrupt");

   PROCEDURE ISR_28_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_28_Handler, "interrupt");

   PROCEDURE ISR_29_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_29_Handler, "interrupt");

   PROCEDURE ISR_30_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN u64);
   PRAGMA Machine_Attribute(ISR_30_Handler, "interrupt");

   PROCEDURE ISR_31_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_31_Handler, "interrupt");
END HAVK_Kernel.Interrupts.Exceptions;
