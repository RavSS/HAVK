-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-serial.adb                                 --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Serial
IS
   PROCEDURE Interface_Initialise(
      Object                  : IN serial_connection)
   IS
      -- TODO: Perhaps make these into functions.
      Interrupt_Settings_Byte : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Object.Interrupt_Settings'address;

      Line_Settings_Byte      : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Object.Line_Settings'address;

      -- Only a baud rate between 50 to 115200 is supported.
      Divisor_Latch_Value     : CONSTANT num RANGE 1 .. 2304 :=
         115200 / Object.Baud_Rate;
   BEGIN
      -- Disable interrupt handling of the connection.
      OUTB(Object.Port + 1, Interrupt_Settings_Byte);

      -- Set the divisor latch access bit to true for the purposes of setting
      -- the baud rate. Offsets +0 and +1 are now used for setting the divisor.
      OUTB(Object.Port + 3, 16#80#);

      -- Send the low byte and then the high byte.
      OUTB(Object.Port + 0,     Divisor_Latch_Value     AND 16#FF#);
      OUTB(Object.Port + 1, SHR(Divisor_Latch_Value, 8) AND 16#FF#);

      -- Apply the line control settings.
      OUTB(Object.Port + 3, Line_Settings_Byte);

      -- TODO: Make records for these.
      OUTB(Object.Port + 2, 16#C7#); -- 11000111 -  FIFO control register.
      OUTB(Object.Port + 4, 16#0B#); --     1011 - modem control register.
   END Interface_Initialise;

   PROCEDURE Write(
      Object     : IN serial_connection;
      Data       : IN character)
   IS
   BEGIN
      WHILE NOT Object.Get_Status.Empty_Buffer LOOP
         PAUSE; -- Wait for the buffer to become empty.
      END LOOP;
      -- Send data.
      OUTB(Object.Port, character'pos(Data));
   END Write;

   PROCEDURE Print(
      Object     : IN serial_connection;
      Data       : IN string)
   IS
   BEGIN
      FOR C IN Data'range LOOP
         IF Data(C) /= character'val(0) THEN -- Never write null bytes in here.
            Object.Write(Data(C));
         END IF;
      END LOOP;
   END Print;

   FUNCTION Get_Status(
      Object     : IN serial_connection)
   RETURN line_status_register IS
      Error_Text : CONSTANT string := " {ERROR} ";
      Status     : CONSTANT line_status_register
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => INB(Object.Port + 5)'address;
   BEGIN
      IF -- Output the error text if something has gone wrong.
              Status.Error_Detected OR
         ELSE Status.Data_Error     OR
         ELSE Status.Data_Lost      OR
         ELSE Status.Incomplete
      THEN
         FOR I IN Error_Text'range LOOP
            OUTB(Object.Port, character'pos(Error_Text(I)));
         END LOOP;
      END IF; -- Continue regardless.

      RETURN Status;
   END Get_Status;
END HAVK_Kernel.Serial;
