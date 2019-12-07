-- READ: https://en.wikipedia.org/wiki/Interrupt_request_(PC_architecture)

PACKAGE HAVK_Kernel.Interrupts.IRQs
IS
   PRAGMA Warnings(GNATprove, off, "pragma ""Machine_Attribute"" ignored",
      Reason => "The pragma must be used to create IRQ ISRs.");

   PRAGMA Warnings(GNATprove, off, "unused variable ""Stack_Frame""",
      Reason => "The ISRs must take the parameter in regardless of usage.");

   -- IRQ 0 - System timer.
   PROCEDURE ISR_32_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_32_Handler, "interrupt");

   -- IRQ 1 - PS/2 keyboard controller.
   PROCEDURE ISR_33_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_33_Handler, "interrupt");

   -- IRQ 2 (behaves like IRQ 9) - Cascades interrupts down to slave PIC.
   PROCEDURE ISR_34_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_34_Handler, "interrupt");

   -- IRQ 3 - RS232 serial port COM2 and/or COM4.
   PROCEDURE ISR_35_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_35_Handler, "interrupt");

   -- IRQ 4 - RS232 serial port COM1 and/or COM3.
   PROCEDURE ISR_36_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_36_Handler, "interrupt");

   -- IRQ 5 - Parallel port LPT2 and/or LPT3, or a sound card.
   PROCEDURE ISR_37_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_37_Handler, "interrupt");

   -- IRQ 6 - Floppy disk controller.
   PROCEDURE ISR_38_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_38_Handler, "interrupt");

   -- IRQ 7 - Parallel port LPT1 or a sound card with careful configuration.
   PROCEDURE ISR_39_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_39_Handler, "interrupt");

   -- IRQ 8 - Real-time clock (RTC).
   PROCEDURE ISR_40_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_40_Handler, "interrupt");

   -- IRQ 9 - ACPI control interrupt handler. IRQ 2 reroutes here.
   PROCEDURE ISR_41_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_41_Handler, "interrupt");

   -- IRQ 10 - Free IRQ.
   PROCEDURE ISR_42_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_42_Handler, "interrupt");

   -- IRQ 11 - Spare IRQ.
   PROCEDURE ISR_43_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_43_Handler, "interrupt");

   -- IRQ 12 - PS/2 mouse.
   PROCEDURE ISR_44_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_44_Handler, "interrupt");

   -- IRQ 13 - Inter-processor interrupt.
   PROCEDURE ISR_45_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_45_Handler, "interrupt");

   -- IRQ 14 - Primary ATA channel.
   PROCEDURE ISR_46_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_46_Handler, "interrupt");

   -- IRQ 15 - Secondary ATA channel.
   PROCEDURE ISR_47_Handler(
      Stack_Frame : IN access_interrupt);
   PRAGMA Machine_Attribute(ISR_47_Handler, "interrupt");

   -- TODO: Add in ISR handlers for IRQ 16 to IRQ 23 (extra APIC IRQs).

END HAVK_Kernel.Interrupts.IRQs;
