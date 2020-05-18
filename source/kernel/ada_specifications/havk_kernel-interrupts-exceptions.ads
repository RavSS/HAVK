-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts-exceptions.ads                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- A package that is home to CPU exception handlers.
-- READ: https://wiki.osdev.org/Exceptions
PACKAGE HAVK_Kernel.Interrupts.Exceptions
WITH
   Preelaborate => true
IS
   PRAGMA Warnings(GNATprove, off, "pragma ""Machine_Attribute"" ignored",
      Reason => "The pragma must be used to create CPU exception ISRs.");

   -- Format: <exception name> - <mnemonic code> - <exception type>.

   -- Divide-by-zero error - DE - fault.
   PROCEDURE ISR_000_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_000_Handler, "interrupt");

   -- Debug exception - DB - fault/trap.
   PROCEDURE ISR_001_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_001_Handler, "interrupt");

   -- Non-maskable interrupt (NMI) exception - no mnemonic - interrupt.
   PROCEDURE ISR_002_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_002_Handler, "interrupt");

   -- Breakpoint exception - BP - trap.
   PROCEDURE ISR_003_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_003_Handler, "interrupt");

   -- Overflow exception - OF - trap.
   PROCEDURE ISR_004_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_004_Handler, "interrupt");

   -- Bound range exceeded exception - BR - fault.
   PROCEDURE ISR_005_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_005_Handler, "interrupt");

   -- Invalid opcode exception - UD - fault.
   PROCEDURE ISR_006_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_006_Handler, "interrupt");

   -- Device unavailable exception - NM - fault.
   PROCEDURE ISR_007_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_007_Handler, "interrupt");

   -- Double fault exception - DF - abort. Error code is always zero.
   PROCEDURE ISR_008_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_008_Handler, "interrupt");

   -- Coprocessor segment overrun - no mnemonic - no type. Never raised.
   PROCEDURE ISR_009_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_009_Handler, "interrupt");

   -- Invalid task state segment - TS - fault.
   PROCEDURE ISR_010_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_010_Handler, "interrupt");

   -- Segment not present - NP - fault.
   PROCEDURE ISR_011_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_011_Handler, "interrupt");

   -- Stack segment error - SS - fault.
   PROCEDURE ISR_012_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_012_Handler, "interrupt");

   -- General protection fault - GP - fault.
   PROCEDURE ISR_013_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_013_Handler, "interrupt");

   -- Page fault - PF - fault.
   PROCEDURE ISR_014_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_014_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_015_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_015_Handler, "interrupt");

   -- x87 floating-point error - MF - fault.
   PROCEDURE ISR_016_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_016_Handler, "interrupt");

   -- Alignment check - AC - fault.
   PROCEDURE ISR_017_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_017_Handler, "interrupt");

   -- Machine check - MC - abort.
   PROCEDURE ISR_018_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_018_Handler, "interrupt");

   -- SIMD floating-point error - XM/XF - fault.
   PROCEDURE ISR_019_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_019_Handler, "interrupt");

   -- Virtualization exception - VE - fault.
   PROCEDURE ISR_020_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_020_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_021_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_021_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_022_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_022_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_023_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_023_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_024_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_024_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_025_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_025_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_026_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_026_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_027_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_027_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_028_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_028_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_029_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_029_Handler, "interrupt");

   -- Security exception - SX - no type.
   PROCEDURE ISR_030_Handler
     (Stack_Frame : IN access_interrupted_state;
      Error_Code  : IN number)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_030_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_031_Handler
     (Stack_Frame : IN access_interrupted_state)
   WITH
      Linker_Section => ".isolated_text";
   PRAGMA Machine_Attribute(ISR_031_Handler, "interrupt");
END HAVK_Kernel.Interrupts.Exceptions;
