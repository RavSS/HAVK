-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-debug.ads                                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Serial;

-- This package will do for now before I get GDB working over a serial
-- connection via a stub.
PACKAGE HAVK_Kernel.Debug
WITH
   Preelaborate   => true,
   Abstract_State => Debugging_State,
   Initializes    => Debugging_State
IS
   -- Activates the serial port that is used for the debugger.
   PROCEDURE Activate
   WITH
      Global => (In_Out => Serial.UART_State,
                 Input  => Debugging_State,
                 Output => Intrinsics.CPU_Port_State),
      Inline => true;

   -- Writes a string to the serial port used for debugging.
   PROCEDURE Message
     (Information : IN string)
   WITH
      Global => (In_Out => (Intrinsics.CPU_Port_State, Serial.UART_State),
                 Input  => Debugging_State),
      Pre    => Information'first = 1 AND THEN -- A soft limit below.
                Information'last IN Information'first .. positive'last / 2;

   -- This is for causing an emulator breakpoint, so I can inspect
   -- values with GDB etc. Not very sophisticated, but it gets the job done.
   PROCEDURE Breakpoint
   WITH
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__breakpoint";

   -- Returns the current file of where this function was called.
   FUNCTION File
      RETURN string
   WITH
      Import     => true,
      Convention => Intrinsic;

   -- Returns the current line number of where this procedure was called.
   FUNCTION Line
      RETURN positive
   WITH
      Import     => true,
      Convention => Intrinsic;

   -- Returns the current package/procedure/function name of where this
   -- procedure was called depending on what encloses it.
   FUNCTION Enclosing_Entity
      RETURN string
   WITH
      Import     => true,
      Convention => Intrinsic;

PRIVATE
   USE TYPE
      Serial.length_select,
      Serial.parity_select;

   -- Variable for usage with kernel debugging. Used by two
   -- functions in the base package. For now, it exists to send messages
   -- over COM1 as a quick and easy alternative to proper debugging.
   Debugger : Serial.connection :=
   (
      -- Use COM1 for debugging purposes.
      Port                    => 16#3F8#, -- Port address declared statically.
      -- Going with a very safe baud rate.
      Baud_Rate               => 9600,
      -- Assume Windows line enders (CRLF), which should stay compatible
      -- with Unix line enders (LF). UEFI firmware uses them too.
      Line_Ender              => (character'val(13), character'val(10)),
      Line_Settings           => -- Settings for the connection itself.
      (
         Data_Size            => Serial.word_8_bit, -- We're sending ASCII.
         Extra_Stop           => false,       -- Only one stop bit required.
         Parity               => Serial.none, -- No parity.
         Data_Breaking        => false,       -- Not line breaking, no need.
         Divisor_Latch_Access => false        -- This is modified externally.
      ),
      -- Entirely disable interrupts.
      Interrupt_Settings      => (Zeroed => 0, OTHERS => false)
   )
   WITH
      Part_Of => Debugging_State;

END HAVK_Kernel.Debug;
