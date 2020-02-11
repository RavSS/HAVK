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

   PROCEDURE ISR_000_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 0 handler entry.";
   END ISR_000_Handler;

   PROCEDURE ISR_001_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 1 handler entry.";
   END ISR_001_Handler;

   PROCEDURE ISR_002_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 2 handler entry.";
   END ISR_002_Handler;

   PROCEDURE ISR_003_Handler -- Breakpoint. Lazy way for now.
     (Stack_Frame : IN access_interrupted_state)
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
   END ISR_003_Handler;

   PROCEDURE ISR_004_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 5 handler entry.";
   END ISR_004_Handler;

   PROCEDURE ISR_005_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 6 handler entry.";
   END ISR_005_Handler;

   PROCEDURE ISR_006_Handler -- Invalid opcode.
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      Log("Invalid opcode at: 0x" & Hex_Image(Stack_Frame.RIP) & '.', warning);
      RAISE Panic
      WITH
         Source_Location & " - Cannot handle invalid opcodes as of now.";
   END ISR_006_Handler;

   PROCEDURE ISR_007_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 7 handler entry.";
   END ISR_007_Handler;

   PROCEDURE ISR_008_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 8 handler entry.";
   END ISR_008_Handler;

   PROCEDURE ISR_009_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 9 handler entry.";
   END ISR_009_Handler;

   PROCEDURE ISR_010_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 10 handler entry.";
   END ISR_010_Handler;

   PROCEDURE ISR_011_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 11 handler entry.";
   END ISR_011_Handler;

   PROCEDURE ISR_012_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 12 handler entry.";
   END ISR_012_Handler;

   PROCEDURE ISR_013_Handler -- General protection fault.
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      Log("ISR 13: General protection fault triggered - Error code:" &
         number'image(Error_Code) & " - Fault address: 0x" &
         Hex_Image(Stack_Frame.RIP) & '.', warning);
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 13 handler entry (GPF).";
   END ISR_013_Handler;

   PROCEDURE ISR_014_Handler -- Page fault.
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      Page_Fault_Handler(Error_Code);
   END ISR_014_Handler;

   PROCEDURE ISR_015_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 15 handler entry.";
   END ISR_015_Handler;

   PROCEDURE ISR_016_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 16 handler entry.";
   END ISR_016_Handler;

   PROCEDURE ISR_017_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 17 handler entry.";
   END ISR_017_Handler;

   PROCEDURE ISR_018_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 18 handler entry.";
   END ISR_018_Handler;

   PROCEDURE ISR_019_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 19 handler entry.";
   END ISR_019_Handler;

   PROCEDURE ISR_020_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 20 handler entry.";
   END ISR_020_Handler;

   PROCEDURE ISR_021_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 21 handler entry.";
   END ISR_021_Handler;

   PROCEDURE ISR_022_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 22 handler entry.";
   END ISR_022_Handler;

   PROCEDURE ISR_023_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 23 handler entry.";
   END ISR_023_Handler;

   PROCEDURE ISR_024_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 24 handler entry.";
   END ISR_024_Handler;

   PROCEDURE ISR_025_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 25 handler entry.";
   END ISR_025_Handler;

   PROCEDURE ISR_026_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 26 handler entry.";
   END ISR_026_Handler;

   PROCEDURE ISR_027_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 27 handler entry.";
   END ISR_027_Handler;

   PROCEDURE ISR_028_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 28 handler entry.";
   END ISR_028_Handler;

   PROCEDURE ISR_029_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 29 handler entry.";
   END ISR_029_Handler;

   PROCEDURE ISR_030_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 30 handler entry.";
   END ISR_030_Handler;

   PROCEDURE ISR_031_Handler
     (Stack_Frame : IN access_interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 31 handler entry.";
   END ISR_031_Handler;
END HAVK_Kernel.Interrupts.Exceptions;
