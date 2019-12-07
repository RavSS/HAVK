-- A package that is home to CPU exception handlers.
-- READ: https://wiki.osdev.org/Exceptions

PACKAGE HAVK_Kernel.Interrupts.Exceptions
IS
   PRAGMA Warnings(GNATprove, off, "pragma ""Machine_Attribute"" ignored",
      Reason => "The pragma must be used to create CPU exception ISRs.");

   -- Format: <exception name> - <mnemonic code> - <exception type>.

   -- Divide-by-zero error - DE - fault.
   PROCEDURE ISR_0_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_0_Handler,  "interrupt");

   -- Debug exception - DB - fault/trap.
   PROCEDURE ISR_1_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_1_Handler,  "interrupt");

   -- Non-maskable interrupt (NMI) exception - no mnemonic - interrupt.
   PROCEDURE ISR_2_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_2_Handler,  "interrupt");

   -- Breakpoint exception - BP - trap.
   PROCEDURE ISR_3_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_3_Handler,  "interrupt");

   -- Overflow exception - OF - trap.
   PROCEDURE ISR_4_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_4_Handler,  "interrupt");

   -- Bound range exceeded exception - BR - fault.
   PROCEDURE ISR_5_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_5_Handler,  "interrupt");

   -- Invalid opcode exception - UD - fault.
   PROCEDURE ISR_6_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_6_Handler,  "interrupt");

   -- Device unavailable exception - NM - fault.
   PROCEDURE ISR_7_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_7_Handler,  "interrupt");

   -- Double fault exception - DF - abort. Error code is always zero.
   PROCEDURE ISR_8_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_8_Handler,  "interrupt");

   -- Coprocessor segment overrun - no mnemonic - no type. Never raised.
   PROCEDURE ISR_9_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_9_Handler,  "interrupt");

   -- Invalid task state segment - TS - fault.
   PROCEDURE ISR_10_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_10_Handler, "interrupt");

   -- Segment not present - NP - fault.
   PROCEDURE ISR_11_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_11_Handler, "interrupt");

   -- Stack segment error - SS - fault.
   PROCEDURE ISR_12_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_12_Handler, "interrupt");

   -- General protection fault - GP - fault.
   PROCEDURE ISR_13_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_13_Handler, "interrupt");

   -- Page fault - PF - fault.
   PROCEDURE ISR_14_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_14_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_15_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_15_Handler, "interrupt");

   -- x87 floating-point error - MF - fault.
   PROCEDURE ISR_16_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_16_Handler, "interrupt");

   -- Alignment check - AC - fault.
   PROCEDURE ISR_17_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_17_Handler, "interrupt");

   -- Machine check - MC - abort.
   PROCEDURE ISR_18_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_18_Handler, "interrupt");

   -- SIMD floating-point error - XM/XF - fault.
   PROCEDURE ISR_19_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_19_Handler, "interrupt");

   -- Virtualization exception - VE - fault.
   PROCEDURE ISR_20_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_20_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_21_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_21_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_22_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_22_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_23_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_23_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_24_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_24_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_25_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_25_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_26_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_26_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_27_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_27_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_28_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_28_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_29_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_29_Handler, "interrupt");

   -- Security exception - SX - no type.
   PROCEDURE ISR_30_Handler(
      Stack_Frame : IN access_interrupt;
      Error_Code  : IN num);
   PRAGMA Machine_Attribute(ISR_30_Handler, "interrupt");

   -- Reserved - no mnemonic - no type. Never raised.
   PROCEDURE ISR_31_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_31_Handler, "interrupt");

END HAVK_Kernel.Interrupts.Exceptions;
