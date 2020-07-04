-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-interrupts-exceptions.ads                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- A package that is home to CPU exception handlers.
-- READ: https://wiki.osdev.org/Exceptions
PACKAGE HAVK_Kernel.Interrupts.Exceptions
IS
   -- Format: <exception name> - <mnemonic code> - <exception type>.

   -- Divide-by-zero error - DE - fault.
   PROCEDURE ISR_000_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_000";
   PROCEDURE ISR_000_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_000";

   -- Debug exception - DB - fault/trap.
   PROCEDURE ISR_001_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_001";
   PROCEDURE ISR_001_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_001";

   -- Non-maskable interrupt (NMI) exception - no mnemonic - interrupt.
   PROCEDURE ISR_002_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_002";
   PROCEDURE ISR_002_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_002";

   -- Breakpoint exception - BP - trap.
   PROCEDURE ISR_003_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_003";
   PROCEDURE ISR_003_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_003";

   -- Overflow exception - OF - trap.
   PROCEDURE ISR_004_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_004";
   PROCEDURE ISR_004_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_004";

   -- Bound range exceeded exception - BR - fault.
   PROCEDURE ISR_005_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_005";
   PROCEDURE ISR_005_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_005";

   -- Invalid opcode exception - UD - fault.
   PROCEDURE ISR_006_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_006";
   PROCEDURE ISR_006_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_006";

   -- Device unavailable exception - NM - fault.
   PROCEDURE ISR_007_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_007";
   PROCEDURE ISR_007_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_007";

   -- Double fault exception - DF - abort. Error code is always zero.
   PROCEDURE ISR_008_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_008";
   PROCEDURE ISR_008_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_008";

   -- Coprocessor segment overrun - no mnemonic - no type. Never raised.
   PROCEDURE ISR_009_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_009";
   PROCEDURE ISR_009_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_009";

   -- Invalid task state segment - TS - fault.
   PROCEDURE ISR_010_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_010";
   PROCEDURE ISR_010_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_010";

   -- Segment not present - NP - fault.
   PROCEDURE ISR_011_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_011";
   PROCEDURE ISR_011_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_011";

   -- Stack segment error - SS - fault.
   PROCEDURE ISR_012_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_012";
   PROCEDURE ISR_012_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_012";

   -- General protection fault - GP - fault.
   PROCEDURE ISR_013_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_013";
   PROCEDURE ISR_013_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_013";

   -- Page fault - PF - fault.
   PROCEDURE ISR_014_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_014",
      Annotate      => (GNATprove, Might_Not_Return);
   PROCEDURE ISR_014_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_014",
      Annotate      => (GNATprove, Might_Not_Return);

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_015_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_015";
   PROCEDURE ISR_015_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_015";

   -- x87 floating-point error - MF - fault.
   PROCEDURE ISR_016_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_016";
   PROCEDURE ISR_016_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_016";

   -- Alignment check - AC - fault.
   PROCEDURE ISR_017_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_017";
   PROCEDURE ISR_017_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_017";

   -- Machine check - MC - abort.
   PROCEDURE ISR_018_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_018";
   PROCEDURE ISR_018_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_018";

   -- SIMD floating-point error - XM/XF - fault.
   PROCEDURE ISR_019_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_019";
   PROCEDURE ISR_019_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_019";

   -- Virtualization exception - VE - fault.
   PROCEDURE ISR_020_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_020";
   PROCEDURE ISR_020_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_020";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_021_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_021";
   PROCEDURE ISR_021_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_021";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_022_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_022";
   PROCEDURE ISR_022_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_022";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_023_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_023";
   PROCEDURE ISR_023_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_023";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_024_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_024";
   PROCEDURE ISR_024_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_024";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_025_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_025";
   PROCEDURE ISR_025_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_025";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_026_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_026";
   PROCEDURE ISR_026_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_026";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_027_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_027";
   PROCEDURE ISR_027_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_027";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_028_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_028";
   PROCEDURE ISR_028_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_028";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_029_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_029";
   PROCEDURE ISR_029_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_029";

   -- Security exception - SX - no type.
   PROCEDURE ISR_030_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_030";
   PROCEDURE ISR_030_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state;
      Error_Code      : IN number)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_030";

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_031_Stub
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__interrupt_handler_stub_031";
   PROCEDURE ISR_031_Handler
     (Interrupt_Frame : NOT NULL ACCESS CONSTANT interrupted_state)
   WITH
      No_Return     => true,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__interrupt_handler_031";

PRIVATE
   Exceptions_Tag : CONSTANT string := "CPUEXCPT";

END HAVK_Kernel.Interrupts.Exceptions;
