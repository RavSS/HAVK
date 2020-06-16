-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-serial.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion;

PACKAGE BODY HAVK_Kernel.Serial
WITH
   Refined_State => (UART_State => NULL)
IS
   PROCEDURE Interface_Initialise
     (Object : IN serial_connection)
   IS
      FUNCTION Convert IS NEW Ada.Unchecked_Conversion
        (source => interrupt_enable_register, target => number);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "Getting the numeric representation of it should be safe.");

      FUNCTION Convert IS NEW Ada.Unchecked_Conversion
        (source => line_control_register,     target => number);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "Getting the numeric representation of it should be safe.");

      -- Only a baud rate between 50 to 115200 is supported.
      Divisor_Latch_Value : CONSTANT number RANGE 1 .. 2304 :=
         115200 / Object.Baud_Rate;
   BEGIN
      -- Disable interrupt handling of the connection.
      Output_Byte(Object.Port + 1,
         Convert(Object.Interrupt_Settings) AND 16#FF#);

      -- Set the divisor latch access bit to true for the purposes of setting
      -- the baud rate. Offsets +0 and +1 are now used for setting the divisor.
      Output_Byte(Object.Port + 3, 16#80#);

      -- Send the low byte and then the high byte.
      Output_Byte
        (Object.Port + 0,             Divisor_Latch_Value     AND 16#FF#);
      Output_Byte
        (Object.Port + 1, Shift_Right(Divisor_Latch_Value, 8) AND 16#FF#);

      -- Apply the line control settings.
      Output_Byte(Object.Port + 3, Convert(Object.Line_Settings) AND 16#FF#);

      -- TODO: Make records for these.
      Output_Byte(Object.Port + 2, 2#11000111#); --  FIFO control register.
      Output_Byte(Object.Port + 4, 2#00001011#); -- Modem control register.
   END Interface_Initialise;

   PROCEDURE Write
     (Object : IN serial_connection;
      Data   : IN character)
   IS
      Empty_Buffer : boolean := Object.Get_Status.Empty_Buffer;
   BEGIN
      WHILE
         NOT Empty_Buffer
      LOOP
         Empty_Buffer := Object.Get_Status.Empty_Buffer;
         Spinlock_Pause; -- Wait for the buffer to become empty.
      END LOOP;

      Output_Byte(Object.Port, character'pos(Data)); -- Send data.
   END Write;

   PROCEDURE Print
     (Object : IN serial_connection;
      Data   : IN string)
   IS
   BEGIN
      FOR
         ASCII OF Data
      LOOP
         IF
            ASCII /= character'val(0) -- Never write null bytes in here.
         THEN
            Object.Write(ASCII);
         END IF;
      END LOOP;
   END Print;

   FUNCTION Get_Status
     (Object : IN serial_connection)
      RETURN line_status_register
   IS
      FUNCTION Convert IS NEW Ada.Unchecked_Conversion
        (source => number, target => line_status_register);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "The value is straight from the CPU itself, so it must be valid.");

      Data : CONSTANT number := Input_Byte(Object.Port + 5);
   BEGIN
      RETURN Convert(Data);
   END Get_Status;
END HAVK_Kernel.Serial;
