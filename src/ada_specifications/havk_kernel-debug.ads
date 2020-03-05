-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-debug.ads                                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Serial,
   HAVK_Kernel.Graphics.Text;
USE
   HAVK_Kernel.Serial;

-- This package will do for now before I get GDB working over
-- a serial connection via a stub.
PACKAGE HAVK_Kernel.Debug
WITH
   Abstract_State => Debugging_State,
   Initializes    => Debugging_State
IS
   PRAGMA Preelaborate;

   -- Initializes the debugging serial port for sending debug info to.
   PROCEDURE Initialise
     (Terminal : IN OUT Graphics.Text.textbox;
      Printing : IN boolean)
   WITH
      Global => (In_Out => Debugging_State),
      Inline => true;

   -- Writes a string to the serial port used for debugging.
   PROCEDURE Message
     (Information : IN string)
   WITH
      Global => (Input => Debugging_State),
      Pre    => Information'length <= 500; -- Absolute maximum.

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
   -- The on-screen printing functionality needs to hook onto a terminal
   -- (textbox type) in order to function properly. This means an unchecked
   -- access has to be created.
   PRAGMA SPARK_Mode(off);

   -- Indicates whether or not the package will print debug messages to the
   -- terminal. By default, it does not.
   Terminal_Printing : boolean := false
   WITH
      Part_Of => Debugging_State;

   -- When a message is sent, it is also printed to a textbox. This is set via
   -- an external package.
   Terminal_Hook     : ACCESS Graphics.Text.textbox := NULL
   WITH
      Part_Of => Debugging_State;

   -- Variable for usage with kernel debugging. Used by two
   -- functions in the base package. For now, it exists to send messages
   -- over COM1 as a quick and easy alternative to proper debugging.
   -- Remove the "CONSTANT" if you need to modify the settings at runtime.
   Debugger          : CONSTANT serial_connection :=
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
         Data_Size            => word_8_bit, -- We're only sending ASCII.
         Extra_Stop           => false,      -- Only one stop bit required.
         Parity               => none,       -- No parity.
         Data_Breaking        => false,      -- Not line breaking, no need.
         Divisor_Latch_Access => false       -- This is modified externally.
      ),
      -- Entirely disable interrupts.
      Interrupt_Settings      => (Zeroed => 0, OTHERS => false)
   )
   WITH
      Part_Of => Debugging_State;

END HAVK_Kernel.Debug;
