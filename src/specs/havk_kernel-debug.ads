WITH
   HAVK_Kernel.Serial;
USE
   HAVK_Kernel.Serial;

-- This package will do for now before I get GDB working over
-- a serial connection via a stub.
PACKAGE HAVK_Kernel.Debug IS
   -- Variable for usage with kernel debugging. Used by two
   -- functions in the base package. For now, it exists to send messages
   -- over COM1 as a quick and easy alternative to proper debugging.
   -- Remove the "CONSTANT" if you need to modify the settings at runtime.
   Debugger : CONSTANT serial_connection :=
   (
      -- Use COM1 for debugging purposes.
      Port                    => COM(1),
      -- Going with a very safe baud rate.
      Baud_Rate               => 9600,
      -- Assume Windows line enders (CRLF), which should stay compatible
      -- with Unix line enders (LF). UEFI firmware uses them too.
      Line_Ender              => (character'val(13), character'val(10)),
      Line_Settings           => -- Settings for the connection itself.
      (
         Data_Size            => word_7_bit, -- We're only sending ASCII.
         Extra_Stop           => false,      -- Only one stop bit required.
         Parity               => none,       -- No parity.
         Data_Breaking        => false,      -- Not line breaking, no need.
         Divisor_Latch_Access => false       -- This is modified externally.
      ),
      -- Entirely disable interrupts.
      Interrupt_Settings      => (Zeroed => 0, OTHERS => false)
   );

   -- Initializes the debugging serial port for sending debug info to.
   PROCEDURE Initialize
   WITH
      Inline => true;

   -- Writes a string to the serial port used for debugging.
   PROCEDURE Message(
      Info : IN string);

   -- This is for causing an emulator breakpoint, so I can inspect
   -- values with GDB etc. Not very sophisticated, but it gets the job done.
   PROCEDURE Breakpoint
   WITH
      Inline => true;
END HAVK_Kernel.Debug;