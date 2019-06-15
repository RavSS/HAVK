WITH
   HAVK_Kernel.Interrupts.PIC,
   System.Machine_Code;
USE
   HAVK_Kernel.Interrupts.PIC,
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel.Interrupts.IRQs IS
   PRAGMA Warnings(Off, "formal parameter ""Stack_Frame"" is not referenced");
   PRAGMA Warnings(Off, "formal parameter ""Error_Code"" is not referenced");

   PROCEDURE ISR_32_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Ticker := Ticker + 1;
      PIC_EOI;
   END ISR_32_Handler;

   PROCEDURE ISR_33_Handler( -- Keyboard.
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Asm("NOP;", Volatile => True);
      PIC_EOI;
   END ISR_33_Handler;

   PROCEDURE ISR_34_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_34_Handler;

   PROCEDURE ISR_35_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_35_Handler;

   PROCEDURE ISR_36_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_36_Handler;

   PROCEDURE ISR_37_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_37_Handler;

   PROCEDURE ISR_38_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_38_Handler;

   PROCEDURE ISR_39_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_39_Handler;

   PROCEDURE ISR_40_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_40_Handler;

   PROCEDURE ISR_41_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_41_Handler;

   PROCEDURE ISR_42_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_42_Handler;

   PROCEDURE ISR_43_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_43_Handler;

   PROCEDURE ISR_44_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_44_Handler;

   PROCEDURE ISR_45_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_45_Handler;

   PROCEDURE ISR_46_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_46_Handler;

   PROCEDURE ISR_47_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      PIC_EOI;
   END ISR_47_Handler;
END HAVK_Kernel.Interrupts.IRQs;