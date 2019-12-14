WITH
   System.Address_Image,
   HAVK_Kernel.Paging,
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Paging,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Interrupts.Exceptions
WITH
   -- `gnatprove` included with GNAT CE 2019 crashes without this, see below.
   SPARK_Mode => off -- Address attributes are used outside of address clauses.
IS
   PRAGMA Warnings(off, "formal parameter ""Stack_Frame"" is not referenced",
      Reason => "The ISRs must take the parameter in regardless of usage.");
   PRAGMA Warnings(off, "formal parameter ""Error_Code"" is not referenced",
      Reason => "Some CPU exception ISRs are passed an error code.");

   PROCEDURE ISR_0_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_0_Handler;

   PROCEDURE ISR_1_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_1_Handler;

   PROCEDURE ISR_2_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_2_Handler;

   PROCEDURE ISR_3_Handler( -- Breakpoint. Lazy way for now.
      Stack_Frame : IN access_interrupt)
   IS
      -- Set this to true in GDB and then single-step out of the ISR to return
      -- to the place where the interrupt was called (at RIP).
      GDB_Ready   : ALIASED boolean := false
      WITH
         Export   => true,
         Volatile => true;
   BEGIN
      WHILE NOT GDB_Ready LOOP
         PAUSE;
      END LOOP;
   END ISR_3_Handler;

   PROCEDURE ISR_4_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_4_Handler;

   PROCEDURE ISR_5_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_5_Handler;

   PROCEDURE ISR_6_Handler( -- Invalid opcode.
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Log("Invalid opcode at: 0x" &
         System.Address_Image(Stack_Frame.RIP) & '.', warning);
      RAISE Program_Error;
   END ISR_6_Handler;

   PROCEDURE ISR_7_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_7_Handler;

   PROCEDURE ISR_8_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_8_Handler;

   PROCEDURE ISR_9_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_9_Handler;

   PROCEDURE ISR_10_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_10_Handler;

   PROCEDURE ISR_11_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_11_Handler;

   PROCEDURE ISR_12_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_12_Handler;

   PROCEDURE ISR_13_Handler( -- General protection fault.
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Log("ISR 13: General protection fault triggered - Error code:" &
         Error_Code'img & " - Fault address: 0x" &
         System.Address_Image(Stack_Frame.RIP) & '.', warning);
   END ISR_13_Handler;

   PROCEDURE ISR_14_Handler( -- Page fault.
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Page_Fault_Handler(Error_Code);
   END ISR_14_Handler;

   PROCEDURE ISR_15_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_15_Handler;

   PROCEDURE ISR_16_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_16_Handler;

   PROCEDURE ISR_17_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_17_Handler;

   PROCEDURE ISR_18_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_18_Handler;

   PROCEDURE ISR_19_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_19_Handler;

   PROCEDURE ISR_20_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_20_Handler;

   PROCEDURE ISR_21_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_21_Handler;

   PROCEDURE ISR_22_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_22_Handler;

   PROCEDURE ISR_23_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_23_Handler;

   PROCEDURE ISR_24_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_24_Handler;

   PROCEDURE ISR_25_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_25_Handler;

   PROCEDURE ISR_26_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_26_Handler;

   PROCEDURE ISR_27_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_27_Handler;

   PROCEDURE ISR_28_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_28_Handler;

   PROCEDURE ISR_29_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_29_Handler;

   PROCEDURE ISR_30_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_30_Handler;

   PROCEDURE ISR_31_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Program_Error;
   END ISR_31_Handler;
END HAVK_Kernel.Interrupts.Exceptions;
