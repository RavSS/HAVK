WITH
   GNAT.Source_Info,
   HAVK_Kernel.Exceptions,
   System.Machine_Code;
USE
   HAVK_Kernel.Exceptions,
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel.Interrupts.Exceptions IS
   PRAGMA Warnings(off, "formal parameter ""Stack_Frame"" is not referenced");
   PRAGMA Warnings(off, "formal parameter ""Error_Code"" is not referenced");

   PROCEDURE ISR_0_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_0_Handler'address, GNAT.Source_Info.Line);
   END ISR_0_Handler;

   PROCEDURE ISR_1_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_1_Handler'address, GNAT.Source_Info.Line);
   END ISR_1_Handler;

   PROCEDURE ISR_2_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_2_Handler'address, GNAT.Source_Info.Line);
   END ISR_2_Handler;

   PROCEDURE ISR_3_Handler( -- Breakpoint. Lazy way for now.
      Stack_Frame : IN access_interrupt)
   IS
      -- For quick use in GDB when in this ISR's frame e.g. `set $rip=rip`.
      -- GCC should optimize this out for the final build.
      PRAGMA Warnings(off, "variable ""RIP"" is not referenced");
      RIP : num := Stack_Frame.RIP;
   BEGIN
      Asm(
         "CLI;" &
         "HLT;",
         Volatile => true);
   END ISR_3_Handler;

   PROCEDURE ISR_4_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_4_Handler'address, GNAT.Source_Info.Line);
   END ISR_4_Handler;

   PROCEDURE ISR_5_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_5_Handler'address, GNAT.Source_Info.Line);
   END ISR_5_Handler;

   PROCEDURE ISR_6_Handler( -- Invalid opcode.
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Debug_Message("Invalid opcode at: " & Stack_Frame.RIP'img & '.');
      Last_Chance_Handler(ISR_6_Handler'address, GNAT.Source_Info.Line);
   END ISR_6_Handler;

   PROCEDURE ISR_7_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_7_Handler'address, GNAT.Source_Info.Line);
   END ISR_7_Handler;

   PROCEDURE ISR_8_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Last_Chance_Handler(ISR_8_Handler'address, GNAT.Source_Info.Line);
   END ISR_8_Handler;

   PROCEDURE ISR_9_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_9_Handler'address, GNAT.Source_Info.Line);
   END ISR_9_Handler;

   PROCEDURE ISR_10_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Last_Chance_Handler(ISR_10_Handler'address, GNAT.Source_Info.Line);
   END ISR_10_Handler;

   PROCEDURE ISR_11_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Last_Chance_Handler(ISR_11_Handler'address, GNAT.Source_Info.Line);
   END ISR_11_Handler;

   PROCEDURE ISR_12_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Last_Chance_Handler(ISR_12_Handler'address, GNAT.Source_Info.Line);
   END ISR_12_Handler;

   PROCEDURE ISR_13_Handler( -- General protection fault.
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      PRAGMA Debug(Debug_Message(
         "ISR 13: General protection fault triggered."));
   END ISR_13_Handler;

   PROCEDURE ISR_14_Handler( -- Page fault.
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      PRAGMA Debug(Debug_Message("ISR 14: Page fault triggered."));
   END ISR_14_Handler;

   PROCEDURE ISR_15_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_15_Handler'address, GNAT.Source_Info.Line);
   END ISR_15_Handler;

   PROCEDURE ISR_16_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_16_Handler'address, GNAT.Source_Info.Line);
   END ISR_16_Handler;

   PROCEDURE ISR_17_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Last_Chance_Handler(ISR_17_Handler'address, GNAT.Source_Info.Line);
   END ISR_17_Handler;

   PROCEDURE ISR_18_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_18_Handler'address, GNAT.Source_Info.Line);
   END ISR_18_Handler;

   PROCEDURE ISR_19_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_19_Handler'address, GNAT.Source_Info.Line);
   END ISR_19_Handler;

   PROCEDURE ISR_20_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_20_Handler'address, GNAT.Source_Info.Line);
   END ISR_20_Handler;

   PROCEDURE ISR_21_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_21_Handler'address, GNAT.Source_Info.Line);
   END ISR_21_Handler;

   PROCEDURE ISR_22_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_22_Handler'address, GNAT.Source_Info.Line);
   END ISR_22_Handler;

   PROCEDURE ISR_23_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_23_Handler'address, GNAT.Source_Info.Line);
   END ISR_23_Handler;

   PROCEDURE ISR_24_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_24_Handler'address, GNAT.Source_Info.Line);
   END ISR_24_Handler;

   PROCEDURE ISR_25_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_25_Handler'address, GNAT.Source_Info.Line);
   END ISR_25_Handler;

   PROCEDURE ISR_26_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_26_Handler'address, GNAT.Source_Info.Line);
   END ISR_26_Handler;

   PROCEDURE ISR_27_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_27_Handler'address, GNAT.Source_Info.Line);
   END ISR_27_Handler;

   PROCEDURE ISR_28_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_28_Handler'address, GNAT.Source_Info.Line);
   END ISR_28_Handler;

   PROCEDURE ISR_29_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_29_Handler'address, GNAT.Source_Info.Line);
   END ISR_29_Handler;

   PROCEDURE ISR_30_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num)
   IS
   BEGIN
      Last_Chance_Handler(ISR_30_Handler'address, GNAT.Source_Info.Line);
   END ISR_30_Handler;

   PROCEDURE ISR_31_Handler(
      Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Last_Chance_Handler(ISR_31_Handler'address, GNAT.Source_Info.Line);
   END ISR_31_Handler;
END HAVK_Kernel.Interrupts.Exceptions;
