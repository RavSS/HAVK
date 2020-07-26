-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts-exceptions.adb                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Descriptors,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Tasking,
   HAVK_Kernel.Tasking.Exceptions;

PACKAGE BODY HAVK_Kernel.Interrupts.Exceptions
IS
   PRAGMA Warnings(off,
      "formal parameter ""Interrupt_Frame"" is not referenced",
      Reason => "The ISRs should take the parameter in regardless of usage.");
   PRAGMA Warnings(off, "formal parameter ""Error_Code"" is not referenced",
      Reason => "Some CPU exception ISRs are passed an error code.");

   PROCEDURE ISR_000_Handler -- Divide-by-zero.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      IF
         Interrupt_Frame.CS = Descriptors.CS_Ring_3 AND THEN
         Interrupt_Frame.SS = Descriptors.DS_Ring_3
      THEN
         Tasking.Exceptions.Zero_Division_Handler(Interrupt_Frame.RIP);
      ELSE
         RAISE Panic
         WITH
            Source_Location & " - Unexpected ISR 0 handler entry.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "This CPU exception should not occur in kernel space.");
      END IF;
   END ISR_000_Handler;

   PROCEDURE ISR_001_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 1 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_001_Handler;

   PROCEDURE ISR_002_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 2 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_002_Handler;

   PROCEDURE ISR_003_Handler -- Breakpoint. Lazy way for now.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      SPARK_Mode => off -- This is not used outside of debugging.
   IS
      -- Set this to true in GDB and then single-step out of the ISR to return
      -- to the place where the interrupt was called (at RIP).
      GDB_Ready : ALIASED boolean := false
      WITH
         Export   => true,
         Volatile => true;
   BEGIN
      WHILE
         NOT GDB_Ready
      LOOP
         Intrinsics.Spinlock_Pause;
      END LOOP;
   END ISR_003_Handler;

   PROCEDURE ISR_004_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 5 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_004_Handler;

   PROCEDURE ISR_005_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 6 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_005_Handler;

   PROCEDURE ISR_006_Handler -- Invalid opcode.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      Log("Invalid opcode at: 0x" & Image(Interrupt_Frame.RIP) & '.',
         Warn => true);
      RAISE Panic
      WITH
         Source_Location & " - Cannot handle invalid opcodes as of now.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_006_Handler;

   PROCEDURE ISR_007_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 7 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_007_Handler;

   PROCEDURE ISR_008_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 8 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_008_Handler;

   PROCEDURE ISR_009_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 9 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_009_Handler;

   PROCEDURE ISR_010_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 10 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_010_Handler;

   PROCEDURE ISR_011_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 11 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_011_Handler;

   PROCEDURE ISR_012_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 12 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_012_Handler;

   PROCEDURE ISR_013_Handler -- General protection fault.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      Log("ISR 13: General protection fault triggered - Error code: " &
         Image(Error_Code, Base => 16) & " - Fault address: 0x" &
         Image(Interrupt_Frame.RIP) & '.', Warn => true);
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 13 handler entry (GPF).";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_013_Handler;

   PROCEDURE ISR_014_Handler -- Page fault.
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      IF
         Interrupt_Frame.CS = Descriptors.CS_Ring_3 AND THEN
         Interrupt_Frame.SS = Descriptors.DS_Ring_3
      THEN
         Tasking.Exceptions.Page_Fault_Handler(Interrupt_Frame.RIP,
            Error_Code);
      ELSE
         Log("Page fault in kernel - Error code: " & Image(Error_Code) & '.',
            Tag => Exceptions_Tag, Critical => true);
         RAISE Panic
         WITH
            Source_Location & " - Unexpected ISR 14 handler entry.";
         PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
            "Only valid for tasks. The kernel should not cause page faults.");
      END IF;
   END ISR_014_Handler;

   PROCEDURE ISR_015_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 15 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_015_Handler;

   PROCEDURE ISR_016_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 16 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_016_Handler;

   PROCEDURE ISR_017_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 17 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_017_Handler;

   PROCEDURE ISR_018_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 18 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_018_Handler;

   PROCEDURE ISR_019_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 19 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_019_Handler;

   PROCEDURE ISR_020_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 20 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_020_Handler;

   PROCEDURE ISR_021_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 21 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_021_Handler;

   PROCEDURE ISR_022_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 22 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_022_Handler;

   PROCEDURE ISR_023_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 23 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_023_Handler;

   PROCEDURE ISR_024_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 24 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_024_Handler;

   PROCEDURE ISR_025_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 25 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_025_Handler;

   PROCEDURE ISR_026_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 26 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_026_Handler;

   PROCEDURE ISR_027_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 27 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_027_Handler;

   PROCEDURE ISR_028_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 28 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_028_Handler;

   PROCEDURE ISR_029_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 29 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_029_Handler;

   PROCEDURE ISR_030_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 30 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_030_Handler;

   PROCEDURE ISR_031_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   IS
   BEGIN
      RAISE Panic
      WITH
         Source_Location & " - Unexpected ISR 31 handler entry.";
      PRAGMA Annotate(GNATprove, Intentional, "exception might be raised",
         "This CPU exception is not yet handled, nor should it occur.");
   END ISR_031_Handler;
END HAVK_Kernel.Interrupts.Exceptions;
