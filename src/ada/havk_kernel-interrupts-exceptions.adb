-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-exceptions.adb                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
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

   PROCEDURE ISR_0_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 0 handler entry.";
   END ISR_0_Handler;

   PROCEDURE ISR_1_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 1 handler entry.";
   END ISR_1_Handler;

   PROCEDURE ISR_2_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 2 handler entry.";
   END ISR_2_Handler;

   PROCEDURE ISR_3_Handler -- Breakpoint. Lazy way for now.
     (Stack_Frame : IN access_interrupt)
   IS
      -- Set this to true in GDB and then single-step out of the ISR to return
      -- to the place where the interrupt was called (at RIP).
      GDB_Ready   : ALIASED boolean := false
      WITH
         Export   => true,
         Volatile => true;
   BEGIN
      WHILE
         NOT GDB_Ready
      LOOP
         Spinlock_Pause;
      END LOOP;
   END ISR_3_Handler;

   PROCEDURE ISR_4_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 5 handler entry.";
   END ISR_4_Handler;

   PROCEDURE ISR_5_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 6 handler entry.";
   END ISR_5_Handler;

   PROCEDURE ISR_6_Handler -- Invalid opcode.
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      Log("Invalid opcode at: 0x" & Hex_Image(Stack_Frame.RIP) & '.', warning);
      RAISE Panic
      WITH
         Source_Location & " - Cannot handle invalid opcodes as of now.";
   END ISR_6_Handler;

   PROCEDURE ISR_7_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 7 handler entry.";
   END ISR_7_Handler;

   PROCEDURE ISR_8_Handler
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 8 handler entry.";
   END ISR_8_Handler;

   PROCEDURE ISR_9_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 9 handler entry.";
   END ISR_9_Handler;

   PROCEDURE ISR_10_Handler
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 10 handler entry.";
   END ISR_10_Handler;

   PROCEDURE ISR_11_Handler
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 11 handler entry.";
   END ISR_11_Handler;

   PROCEDURE ISR_12_Handler
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 12 handler entry.";
   END ISR_12_Handler;

   PROCEDURE ISR_13_Handler -- General protection fault.
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      Log("ISR 13: General protection fault triggered - Error code:" &
         number'image(Error_Code) & " - Fault address: 0x" &
         Hex_Image(Stack_Frame.RIP) & '.', warning);
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 13 handler entry (GPF).";
   END ISR_13_Handler;

   PROCEDURE ISR_14_Handler -- Page fault.
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      Page_Fault_Handler(Error_Code);
   END ISR_14_Handler;

   PROCEDURE ISR_15_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 15 handler entry.";
   END ISR_15_Handler;

   PROCEDURE ISR_16_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 16 handler entry.";
   END ISR_16_Handler;

   PROCEDURE ISR_17_Handler
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 17 handler entry.";
   END ISR_17_Handler;

   PROCEDURE ISR_18_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 18 handler entry.";
   END ISR_18_Handler;

   PROCEDURE ISR_19_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 19 handler entry.";
   END ISR_19_Handler;

   PROCEDURE ISR_20_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 20 handler entry.";
   END ISR_20_Handler;

   PROCEDURE ISR_21_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 21 handler entry.";
   END ISR_21_Handler;

   PROCEDURE ISR_22_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 22 handler entry.";
   END ISR_22_Handler;

   PROCEDURE ISR_23_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 23 handler entry.";
   END ISR_23_Handler;

   PROCEDURE ISR_24_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 24 handler entry.";
   END ISR_24_Handler;

   PROCEDURE ISR_25_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 25 handler entry.";
   END ISR_25_Handler;

   PROCEDURE ISR_26_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 26 handler entry.";
   END ISR_26_Handler;

   PROCEDURE ISR_27_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 27 handler entry.";
   END ISR_27_Handler;

   PROCEDURE ISR_28_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 28 handler entry.";
   END ISR_28_Handler;

   PROCEDURE ISR_29_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 29 handler entry.";
   END ISR_29_Handler;

   PROCEDURE ISR_30_Handler
     (Stack_Frame : IN access_interrupt;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 30 handler entry.";
   END ISR_30_Handler;

   PROCEDURE ISR_31_Handler
     (Stack_Frame : IN access_interrupt)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 31 handler entry.";
   END ISR_31_Handler;
END HAVK_Kernel.Interrupts.Exceptions;
